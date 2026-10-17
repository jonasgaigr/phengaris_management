#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: the cascade and its inputs and outputs
#
#----------------------------------------------------------#
#
# PIPELINE lists every step with the files it reads and the files it writes.
# run_all.R runs the steps from this list, and every step script opens with
# step_begin() and closes with step_end(), which check those files. A step
# whose input is missing stops with a message naming the file and the step
# that writes it, so any step can be run on its own on existing
# intermediates, from a clean session:
#
#   Rscript R/07_describe_tables.R
#
# Specifications. The analysis can be run under more than one set of
# analytical choices (config.yml, `specifications`), e.g. the original
# "legacy" choices and a "revised" set. Steps come in three scopes:
#
#   common  does not depend on the specification; runs once and writes to
#           Data/Processed/common and Outputs/common
#   spec    runs once per specification and writes to Data/Processed/<spec>
#           and Outputs/<spec>
#   final   runs once at the end (the compiled report)
#
# A spec step can be limited to some model sets (the `model_set` of a
# specification): the original model steps 09-13 run under "legacy", the
# revised ones, 17-20, under "revised".
#
# File names in the step declarations are templates: {processed} and
# {outputs} are the folders of config.yml, {spec} is the specification being
# run, {input} the input folder. A name starting with "?" is optional: the
# step copes without it.
#
#----------------------------------------------------------#

#' Declare one step of the cascade.
#'
#' @param model_sets  for a spec step, the model sets (config.yml,
#'                    `model_set` of a specification) it runs under; NULL
#'                    for every specification
pipeline_step <- function(id, file, title, scope = c("common", "spec", "final"),
                          inputs = character(), outputs = character(),
                          model_sets = NULL) {
  list(
    id = id, file = file, title = title, scope = match.arg(scope),
    inputs = inputs, outputs = outputs, model_sets = model_sets
  )
}

# Shorthands for the most common files, to keep the declarations readable.
.records <- "{processed}/common/records.csv"
.sites <- "{processed}/common/sites.gpkg"
.analysis <- "{processed}/{spec}/data_analysis.csv"
.site_design <- "{processed}/common/site_design.csv"
.flight_window <- "{processed}/common/flight_window.csv"
.flight_curve <- "{processed}/common/flight_curve.csv"
.connectivity <- "{processed}/common/connectivity.csv"
# The frozen and the current site x VMB intersections and the site x KVES
# one; each is written only when its source is there. The site areas of step
# 05 are an optional output of their own.
.habitat_overlaps <- paste0(
  "?{processed}/common/",
  c("data_lokal_vmb.csv", "data_lokal_vmb_current.csv", "data_lokal_kves.csv")
)

PIPELINE <- list(
  pipeline_step(
    "01", "R/01_load_source_data.R", "Load source data", "common",
    inputs = c(
      unlist(CONFIG$paths$occurrence),
      unlist(CONFIG$paths$site_layers)
    ),
    outputs = c(.records, .sites)
  ),
  pipeline_step(
    "03", "R/03_protected_areas.R", "Protected areas", "common",
    inputs = c(
      .records, .sites,
      file.path(CONFIG$paths$reference, c("evl.gpkg.zip", "mzchu.gpkg.zip", "n2k_site_subjects.csv"))
    ),
    outputs = "{processed}/common/protected_area_id.csv"
  ),
  pipeline_step(
    "05", "R/05_habitat_intersect.R", "Habitat intersection", "common",
    inputs = c(
      .records, .sites,
      paste0("?", c(
        CONFIG$paths$habitat_legacy_intersection, CONFIG$habitat$layer, unlist(CONFIG$habitat$kves_layers)
      ))
    ),
    outputs = c(.habitat_overlaps, "?{processed}/common/site_areas.gpkg")
  ),
  pipeline_step(
    "16", "R/16_site_design.R", "Sites, flight season and connectivity", "common",
    inputs = c(.records, .sites, paste0("?", file.path(CONFIG$paths$reference, "elevation.tif"))),
    outputs = c(.site_design, .flight_window, .flight_curve, .connectivity)
  ),
  pipeline_step(
    "02", "R/02_impute_absences.R", "Impute absences", "spec",
    inputs = c(.records, .sites, unlist(CONFIG$paths$ranges)),
    outputs = "{processed}/{spec}/data_with_imputed.csv"
  ),
  pipeline_step(
    "04", "R/04_clean_occurrence.R", "Clean occurrence data", "spec",
    inputs = c(
      "{processed}/{spec}/data_with_imputed.csv",
      "{processed}/common/protected_area_id.csv",
      # Needed only by the specifications with design_variables: true; step
      # 04 checks for them itself.
      paste0("?", c(.site_design, .flight_window, .flight_curve, .connectivity))
    ),
    outputs = "{processed}/{spec}/data_clean.csv"
  ),
  pipeline_step(
    "06", "R/06_habitat_join.R", "Habitat attributes", "spec",
    inputs = c("{processed}/{spec}/data_clean.csv", .habitat_overlaps),
    outputs = .analysis
  ),
  pipeline_step("07", "R/07_describe_tables.R", "Descriptive tables", "spec", inputs = .analysis),
  pipeline_step("08", "R/08_describe_figures.R", "Descriptive figures", "spec", inputs = .analysis),
  # The original model set.
  pipeline_step("09", "R/09_models_nausithous.R", "Models P. nausithous", "spec",
    inputs = .analysis, model_sets = "legacy"
  ),
  pipeline_step("10", "R/10_models_teleius.R", "Models P. teleius", "spec",
    inputs = .analysis, model_sets = "legacy"
  ),
  pipeline_step("11", "R/11_models_both_species.R", "Models both species", "spec",
    inputs = .analysis, model_sets = "legacy"
  ),
  pipeline_step("12", "R/12_model_figures.R", "Model figures", "spec",
    inputs = .analysis, model_sets = "legacy"
  ),
  pipeline_step("13", "R/13_threats_ordination.R", "Threats and pressures", "spec",
    inputs = .analysis, model_sets = "legacy"
  ),
  # The revised model set.
  pipeline_step("17", "R/17_occupancy_models.R", "Occupancy models", "spec",
    inputs = c(.analysis, .flight_curve), model_sets = "revised"
  ),
  pipeline_step("18", "R/18_spatial_autocorrelation.R", "Spatial autocorrelation", "spec",
    inputs = .analysis, model_sets = "revised"
  ),
  pipeline_step("19", "R/19_threat_models.R", "Threat models", "spec",
    inputs = .analysis, model_sets = "revised"
  ),
  pipeline_step("20", "R/20_abundance_models.R", "Abundance models", "spec",
    inputs = .analysis, model_sets = "revised"
  ),
  pipeline_step(
    "14", "R/14_maps.R", "Maps", "spec",
    inputs = c(
      .analysis, .records, .sites,
      file.path(CONFIG$paths$reference, c("czechia_border.gpkg.zip", "sitmap.gpkg.zip", "relief_shaded.tif"))
    )
  ),
  pipeline_step(
    "15", "R/15_compile_report.R", "Compile the report", "final",
    outputs = "{outputs}/REPORT.md"
  )
)

#--------------------------------------------------#
## The step being run -----
#--------------------------------------------------#

# Which step and specification are being run, so that the output helpers
# know where to write.
.pipeline_env <- new.env(parent = emptyenv())

#' The specifications config.yml asks to run.
pipeline_specs <- function() {
  specs <- CONFIG$specifications$run
  checkmate::assert_character(specs, min.len = 1, any.missing = FALSE)
  unknown <- setdiff(specs, names(CONFIG$specifications$definitions))
  if (length(unknown)) {
    stop("Unknown specification(s) in config.yml: ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  specs
}

#' The specification a spec-scoped step runs under.
#'
#' run_all.R sets PHENGARIS_SPEC; a step run on its own uses the first
#' specification in config.yml.
current_spec <- function() {
  spec <- Sys.getenv("PHENGARIS_SPEC", unset = "")
  if (!nzchar(spec)) spec <- pipeline_specs()[1]
  spec
}

#' The settings of one specification, from config.yml.
spec_settings <- function(spec = current_spec()) {
  CONFIG$specifications$definitions[[spec]]
}

#' Does a step run under a specification?
#'
#' Common and final steps always run; a spec step runs unless it is limited
#' to model sets other than the specification's.
step_runs_under <- function(step, spec) {
  is.null(step$model_sets) || spec_settings(spec)$model_set %in% step$model_sets
}

#' Stop when a step's own conditional inputs are missing.
#'
#' For inputs declared optional ("?") because only some specifications need
#' them.
#'
#' @param templates  declared file names
require_inputs <- function(templates) {
  paths <- vapply(templates, pipeline_path, character(1))
  missing <- !file.exists(paths)
  if (any(missing)) {
    writers <- vapply(sub("^\\?", "", templates[missing]), function(f) {
      w <- Filter(function(s) any(sub("^\\?", "", s$outputs) == f), PIPELINE)
      if (length(w)) paste0("step ", w[[1]]$id) else "an input file"
    }, character(1))
    stop(
      "Step ", .pipeline_env$step$id, " [", current_spec(), "] cannot run: missing input(s)\n",
      paste0("  ", paths[missing], "  (written by ", writers, ")", collapse = "\n"),
      call. = FALSE
    )
  }
  invisible(paths)
}

#' Look up one step of the cascade by id.
pipeline_step_by_id <- function(id) {
  ids <- vapply(PIPELINE, function(s) s$id, character(1))
  if (!id %in% ids) stop("Unknown step id: ", id, call. = FALSE)
  PIPELINE[[match(id, ids)]]
}

#' Fill in the folder templates of a declared file name.
pipeline_path <- function(template, spec = current_spec()) {
  path <- sub("^\\?", "", template)
  path <- gsub("{processed}", CONFIG$paths$processed, path, fixed = TRUE)
  path <- gsub("{outputs}", CONFIG$paths$outputs, path, fixed = TRUE)
  path <- gsub("{input}", CONFIG$paths$input, path, fixed = TRUE)
  gsub("{spec}", spec, path, fixed = TRUE)
}

#' Folder the current step writes its processed data to, or reads from.
#'
#' @param scope  "spec" for the folder of the current specification, "common"
#'               for the specification-independent intermediates
processed_dir <- function(scope = c("spec", "common")) {
  scope <- match.arg(scope)
  dir <- file.path(CONFIG$paths$processed, if (scope == "common") "common" else current_spec())
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dir
}

#' Full path of a processed file.
processed_path <- function(file, scope = c("spec", "common")) {
  file.path(processed_dir(match.arg(scope)), file)
}

#' Folder the current step writes its tables, figures and report into.
#'
#' @param kind  "Tables", "Figures" or "Reports"
output_dir <- function(kind) {
  scope <- .pipeline_env$output_scope
  if (is.null(scope)) scope <- current_spec()
  dir <- file.path(CONFIG$paths$outputs, scope, kind)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dir
}

#' Open a step: check its inputs and point the output helpers at its folders.
#'
#' @param id  the step id, as in PIPELINE
step_begin <- function(id) {
  step <- pipeline_step_by_id(id)
  # A step limited to some model sets, run on its own, runs under the first
  # specification that uses one of them.
  run_alone <- !nzchar(Sys.getenv("PHENGARIS_SPEC"))
  if (step$scope == "spec" && run_alone && !step_runs_under(step, current_spec())) {
    matching <- Filter(function(s) step_runs_under(step, s), pipeline_specs())
    if (!length(matching)) {
      stop("Step ", id, " runs under no specification listed in config.yml.", call. = FALSE)
    }
    Sys.setenv(PHENGARIS_SPEC = matching[[1]])
  }
  spec <- current_spec()
  if (step$scope == "spec" && !step_runs_under(step, spec)) {
    stop(
      "Step ", id, " runs only under the model set(s) ", paste(step$model_sets, collapse = ", "),
      "; specification ", spec, " uses ", spec_settings(spec)$model_set, ".",
      call. = FALSE
    )
  }
  .pipeline_env$step <- step
  .pipeline_env$output_scope <- if (step$scope == "spec") spec else "common"

  required <- step$inputs[!startsWith(step$inputs, "?")]
  paths <- vapply(required, pipeline_path, character(1), spec = spec)
  missing <- !file.exists(paths)
  if (any(missing)) {
    writers <- vapply(step$inputs[!startsWith(step$inputs, "?")][missing], function(f) {
      w <- Filter(function(s) any(sub("^\\?", "", s$outputs) == f), PIPELINE)
      if (length(w)) paste0("step ", w[[1]]$id) else "an input file"
    }, character(1))
    stop(
      "Step ", id, " cannot run: missing input(s)\n",
      paste0("  ", paths[missing], "  (written by ", writers, ")", collapse = "\n"),
      call. = FALSE
    )
  }

  label <- if (step$scope == "spec") paste0(" [", spec, "]") else ""
  message("Step ", id, label, ": ", tolower(step$title))
  invisible(step)
}

#' Close a step: check that it wrote every declared output.
step_end <- function(id) {
  step <- pipeline_step_by_id(id)
  required <- step$outputs[!startsWith(step$outputs, "?")]
  paths <- vapply(required, pipeline_path, character(1))
  if (any(!file.exists(paths))) {
    stop(
      "Step ", id, " finished without writing: ",
      paste(paths[!file.exists(paths)], collapse = ", "),
      call. = FALSE
    )
  }
  message("Step ", id, " done")
  invisible(step)
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#              Run the whole analysis
#
#
#                   Jonáš Gaigr
#                       2025
#
#----------------------------------------------------------#
#
# Runs every step in dependency order, in one session.
#
#   From RStudio      source("run_all.R")
#   From the shell    Rscript run_all.R
#   Selected steps    Rscript run_all.R 07 08 15
#
# Steps share one session on purpose: steps 01 and 14 exchange spatial objects
# that are expensive to rebuild. Every other step reads its input from
# Data/Processed, so a partial re-run of steps 02 onwards is safe as long as
# the earlier outputs exist.
#
# Each step writes its own tables to Outputs/Tables, its figures to
# Outputs/Figures and a Markdown report to Outputs/Reports. Step 15 collects
# those into Outputs/REPORT.md.
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# The cascade -----
#----------------------------------------------------------#

step <- function(id, file, title) list(id = id, file = file, title = title)

STEPS <- list(
  step("01", "R/01_load_source_data.R",    "Load source data"),
  step("02", "R/02_impute_absences.R",     "Impute absences"),
  step("03", "R/03_protected_areas.R",     "Protected areas"),
  step("04", "R/04_clean_occurrence.R",    "Clean occurrence data"),
  step("05", "R/05_habitat_intersect.R",   "Habitat intersection"),
  step("06", "R/06_habitat_join.R",        "Habitat attributes"),
  step("07", "R/07_describe_tables.R",     "Descriptive tables"),
  step("08", "R/08_describe_figures.R",    "Descriptive figures"),
  step("09", "R/09_models_nausithous.R",   "Models P. nausithous"),
  step("10", "R/10_models_teleius.R",      "Models P. teleius"),
  step("11", "R/11_models_both_species.R", "Models both species"),
  step("12", "R/12_model_figures.R",       "Model figures"),
  step("13", "R/13_threats_ordination.R",  "Threats and pressures"),
  step("14", "R/14_maps.R",                "Maps"),
  step("15", "R/15_compile_report.R",      "Compile the report")
)

#----------------------------------------------------------#
# Setup -----
#----------------------------------------------------------#

# Always runs: it loads the packages, the helpers and the constants.
# The encoding is given explicitly because the monitoring source names and the
# habitat search patterns are Czech, and the files are UTF-8 regardless of the
# locale R happens to start in.
source("R/00_setup.R", encoding = "UTF-8")

#----------------------------------------------------------#
# Select the steps to run -----
#----------------------------------------------------------#

requested <- commandArgs(trailingOnly = TRUE)

# Interactive use: set run_steps before sourcing to run a subset,
# e.g. run_steps <- c("07", "08", "15")
if (!length(requested) && exists("run_steps")) {
  requested <- as.character(run_steps)
}

steps_to_run <- if (length(requested)) {
  unknown <- setdiff(requested, vapply(STEPS, function(s) s$id, character(1)))
  if (length(unknown)) {
    stop("Unknown step id(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  Filter(function(s) s$id %in% requested, STEPS)
} else {
  STEPS
}

#----------------------------------------------------------#
# Run -----
#----------------------------------------------------------#

started <- Sys.time()
timings <- list()

for (this_step in steps_to_run) {
  banner <- paste0("== Step ", this_step$id, " - ", this_step$title, " ")
  message("\n", banner, strrep("=", max(0, 60 - nchar(banner))))

  step_started <- Sys.time()
  source(this_step$file, local = FALSE, encoding = "UTF-8")
  elapsed <- as.numeric(difftime(Sys.time(), step_started, units = "secs"))

  timings[[length(timings) + 1]] <- data.frame(
    step = this_step$id, title = this_step$title,
    seconds = round(elapsed, 1),
    stringsAsFactors = FALSE
  )
}

#----------------------------------------------------------#
# Summary -----
#----------------------------------------------------------#

message("\n", strrep("=", 62))
print(do.call(rbind, timings), row.names = FALSE)
message(
  "Total: ",
  round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1),
  " minutes"
)
message("Report: ", file.path(PATHS$outputs, "REPORT.md"))

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

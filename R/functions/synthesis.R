#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: the robustness of the key estimates
#
#----------------------------------------------------------#
#
# Steps 17-24 estimate the same few associations in many ways: three samples,
# windows of the flight season, connectivity, the habitat and the landscape,
# a spatial field and spatial+, a mixed-design occupancy model and a known
# detection probability, and matching. Step 25 collects the odds ratios of
# the key terms from their result tables into one table and one figure per
# species, so that the robustness of each can be read at a glance.
#
# The tables are read as the steps wrote them; nothing is refitted. Some
# tables hold the odds ratio as a formatted string, "2.16 (1.48-3.15)",
# which parse_ratio_ci() turns back into numbers.
#
#----------------------------------------------------------#

#' The key terms of the synthesis, with their labels.
SYNTHESIS_TERMS <- c(
  TIMING = "mowing timed appropriately",
  METHOD = "patches left uncut",
  `TIMING:METHOD` = "timing x patches",
  PLANTabundant = "host plant abundant",
  PLANTdominant = "host plant dominant",
  EVL_species = "EVL designated for the species"
)

#' The key of a species in config.yml `species`, which also names its result
#' tables: SPECIES_TEL -> "teleius".
species_key <- function(species) {
  key <- names(CONFIG$species)[vapply(CONFIG$species, function(s) identical(s$name, species), logical(1))]
  if (length(key) != 1) stop("Unknown species: ", species, call. = FALSE)
  key
}

#' The sample a species' results are read from (config.yml `synthesis:
#' primary_sample`).
#'
#' @param species  the species name, e.g. SPECIES_TEL
#' @return its label in REVISED_SAMPLES, e.g. "with plausible imputed"
primary_sample <- function(species) {
  REVISED_SAMPLES[[CONFIG$synthesis$primary_sample[[species_key(species)]]]]
}

#' Numbers from a formatted odds ratio, "2.16 (1.48-3.15)".
#'
#' @param x  formatted strings
#' @return a data frame of odds_ratio, or_low and or_high (NA where a part
#'         is not a number)
parse_ratio_ci <- function(x) {
  pattern <- "^\\s*([0-9.]+|NaN|Inf)\\s*\\(\\s*([0-9.]+|NaN|Inf)\\s*-\\s*([0-9.]+|NaN|Inf)\\s*\\)\\s*$"
  ok <- !is.na(x) & grepl(pattern, x)
  part <- function(i) {
    out <- rep(NA_real_, length(x))
    out[ok] <- suppressWarnings(as.numeric(sub(pattern, paste0("\\", i), x[ok])))
    out
  }
  data.frame(odds_ratio = part(1), or_low = part(2), or_high = part(3))
}

#' Read a result table of the current specification, or NULL when missing.
.read_result_table <- function(stem) {
  path <- file.path(output_dir("Tables"), paste0(stem, ".csv"))
  if (!file.exists(path)) {
    return(NULL)
  }
  utils::read.csv(path, fileEncoding = "CP1250", stringsAsFactors = FALSE)
}

#' Rows of the synthesis from a coefficient table (numeric ratios).
.from_coefficients <- function(tab, models, analysis, source) {
  if (is.null(tab)) {
    return(NULL)
  }
  rows <- tab[tab$model %in% names(models) & tab$term %in% names(SYNTHESIS_TERMS), , drop = FALSE]
  if (!nrow(rows)) {
    return(NULL)
  }
  data.frame(
    analysis = analysis, sample = unname(models[rows$model]), term = rows$term,
    odds_ratio = rows$ratio, or_low = rows$ratio_low, or_high = rows$ratio_high,
    source = source, stringsAsFactors = FALSE
  )
}

#' The odds ratios of the key terms of one species across the analyses.
#'
#' @param code          "nau" or "tel"
#' @param species_file  "nausithous" or "teleius"
#' @return one row per analysis, sample and term: odds_ratio, or_low,
#'         or_high and the table it came from
collect_robustness <- function(code, species_file) {
  s <- REVISED_SAMPLES
  ids <- function(base) stats::setNames(unname(s), paste0(code, base, vapply(s, sample_suffix, character(1))))
  out <- list()
  t17 <- .read_result_table(paste0("17_", species_file, "_coefficients"))
  out$management <- .from_coefficients(t17, ids("_management"), "management model (step 17)", "17 coefficients")
  out$conn <- .from_coefficients(t17, ids("_management_conn"), "+ connectivity", "17 coefficients")
  out$y2021 <- .from_coefficients(
    t17, ids("_management_2021"), "2021-2024 polygons, + grazing and area", "17 coefficients"
  )
  t17w <- .read_result_table(paste0("17_", species_file, "_window_coefficients"))
  for (days in unlist(CONFIG$design$flight_window$sensitivity_days)) {
    out[[paste0("w", days)]] <- .from_coefficients(
      t17w, ids(paste0("_management_w", days)), sprintf("window of %d days", days), "17 window coefficients"
    )
  }
  t22 <- .read_result_table(paste0("22_", species_file, "_coefficients"))
  out$habitat <- .from_coefficients(t22, ids("_habitat_landscape"), "+ habitat and landscape", "22 coefficients")

  spde <- .read_result_table("18_grid_vs_spde")
  if (!is.null(spde)) {
    keep <- spde$random_effect == "SPDE spatial field" & startsWith(spde$model, paste0(code, "_"))
    spde <- spde[keep & spde$term %in% names(SYNTHESIS_TERMS), , drop = FALSE]
    if (nrow(spde)) {
      out$spde <- data.frame(
        analysis = "SPDE spatial field (step 18)", sample = spde$sample, term = spde$term,
        parse_ratio_ci(spde$ratio_ci), source = "18 grid vs SPDE", stringsAsFactors = FALSE
      )
    }
  }
  plus <- .read_result_table("18_spatial_plus")
  if (!is.null(plus)) {
    plus <- plus[startsWith(plus$model, paste0(code, "_")) & plus$term %in% names(SYNTHESIS_TERMS), , drop = FALSE]
    if (nrow(plus)) {
      out$plus <- data.frame(
        analysis = "spatial+ (step 18)", sample = plus$sample, term = plus$term,
        parse_ratio_ci(plus$odds_ratio_spatialplus), source = "18 spatial+", stringsAsFactors = FALSE
      )
    }
  }

  known <- .read_result_table(paste0("23_", species_file, "_known_detection_coefficients"))
  if (!is.null(known)) {
    known <- known[known$p == 0.7 & known$status == "ok" & known$term %in% names(SYNTHESIS_TERMS), , drop = FALSE]
    if (nrow(known)) {
      out$known <- data.frame(
        analysis = "known detection p = 0.7 (step 23)", sample = known$sample, term = known$term,
        odds_ratio = known$ratio, or_low = known$ratio_low, or_high = known$ratio_high,
        source = "23 known detection", stringsAsFactors = FALSE
      )
    }
  }
  posterior <- .read_result_table(paste0("23_", species_file, "_occupancy_posterior"))
  if (!is.null(posterior)) {
    clean <- stats::setNames(names(SYNTHESIS_TERMS), gsub("[^A-Za-z0-9_]", "_", names(SYNTHESIS_TERMS)))
    posterior <- posterior[posterior$part == "occupancy" & posterior$term %in% names(clean), , drop = FALSE]
    labels <- c(
      "with imputed, prior on p" = s[["with_imputed"]],
      "with plausible imputed, prior on p" = s[["plausible"]],
      "observed only, prior on p" = s[["observed"]]
    )
    posterior <- posterior[posterior$model %in% names(labels), , drop = FALSE]
    if (nrow(posterior)) {
      out$occupancy <- data.frame(
        analysis = "occupancy model, mixed design (step 23)", sample = unname(labels[posterior$model]),
        term = unname(clean[posterior$term]),
        odds_ratio = posterior$odds_ratio, or_low = posterior$or_low, or_high = posterior$or_high,
        source = "23 occupancy posterior", stringsAsFactors = FALSE
      )
    }
  }

  designation <- .read_result_table(paste0("24_", species_file, "_designation"))
  if (!is.null(designation)) {
    keep <- c(
      "GLMM, covariate-adjusted, unmatched" = "EVL: covariate-adjusted (step 24)",
      "GLM, cluster-robust by subclass" = "EVL: matched, cluster-robust GLM (step 24)"
    )
    designation <- designation[designation$outcome_model %in% names(keep), , drop = FALSE]
    if (nrow(designation)) {
      out$designation <- data.frame(
        analysis = unname(keep[designation$outcome_model]), sample = designation$sample, term = "EVL_species",
        odds_ratio = designation$odds_ratio, or_low = designation$or_low, or_high = designation$or_high,
        source = "24 designation", stringsAsFactors = FALSE
      )
    }
  }

  out <- dplyr::bind_rows(out)
  out$term_label <- unname(SYNTHESIS_TERMS[out$term])
  out
}

#' Whether each odds ratio is an estimate: a finite interval whose upper
#' limit is less than `max_interval_ratio` times its lower one. A separated
#' term has an interval spanning many orders of magnitude instead.
#'
#' @param robustness          collect_robustness()
#' @param max_interval_ratio  config.yml `synthesis: max_interval_ratio`
#' @return a logical vector, one per row
is_estimable <- function(robustness, max_interval_ratio = CONFIG$synthesis$max_interval_ratio) {
  ok <- !is.na(robustness$odds_ratio) & is.finite(robustness$or_low) &
    is.finite(robustness$or_high) & robustness$or_low > 0
  ok & robustness$or_high / robustness$or_low < max_interval_ratio
}

#' How robust each term is across the analyses of one sample.
#'
#' A fit that is not an estimate (is_estimable()) is counted in
#' `not_estimable` and left out of everything else.
#'
#' @param robustness          collect_robustness()
#' @param max_interval_ratio  config.yml `synthesis: max_interval_ratio`
#' @return one row per sample and term: the analyses, the range of the odds
#'         ratios, how many lie above 1, and how many intervals exclude 1
summarise_robustness <- function(robustness, max_interval_ratio = CONFIG$synthesis$max_interval_ratio) {
  robustness <- robustness[!is.na(robustness$odds_ratio), , drop = FALSE]
  robustness$estimable <- is_estimable(robustness, max_interval_ratio)
  robustness %>%
    dplyr::group_by(sample, term, term_label) %>%
    dplyr::summarise(
      analyses = sum(estimable),
      not_estimable = sum(!estimable),
      min_odds_ratio = if (analyses > 0) min(odds_ratio[estimable]) else NA_real_,
      max_odds_ratio = if (analyses > 0) max(odds_ratio[estimable]) else NA_real_,
      above_1 = sum(estimable & odds_ratio > 1),
      below_1 = sum(estimable & odds_ratio < 1),
      ci_excluding_1 = sum(estimable & (or_low > 1 | or_high < 1)),
      direction = dplyr::case_when(
        analyses == 0 ~ "not estimable",
        above_1 == analyses ~ "always above 1",
        below_1 == analyses ~ "always below 1",
        TRUE ~ "mixed"
      ),
      .groups = "drop"
    ) %>%
    dplyr::select(-below_1)
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: habitat and landscape models (P4.5-P4.7)
#
#----------------------------------------------------------#
#
# Step 22 adds to the management model of step 17 what the literature review
# found missing:
#
#   P4.5  an objective check of the mowing timing the surveyors recorded:
#         the AOPK rule is no cut between 1 July and 1 September, but the form
#         records no cut dates. The notes that say the site was freshly mown
#         at a visit in that window are the only objective evidence of a cut
#         inside it.
#   P4.6  host-plant abundance as an ordered variable, and proxies for the
#         habitat of the Myrmica host ants: the share of wet meadows and, on
#         polygon sites, the share near the edge.
#   P4.7  the landscape around the site, with the radius of each variable
#         chosen by AIC among 0.2-4 km.
#
# Shares enter the models in tenths: an odds ratio is per 10 percentage
# points.
#
#----------------------------------------------------------#

#' Does a note say the site was freshly mown at the visit?
#'
#' @param note     POZNAMKA
#' @param pattern  config.yml `management: fresh_mown_pattern`
fresh_mown <- function(note, pattern = CONFIG$management$fresh_mown_pattern) {
  !is.na(note) & grepl(pattern, note, ignore.case = TRUE, perl = TRUE)
}

#' Is a date inside the window in which no cut should fall?
#'
#' @param datum   YYYYMMDD
#' @param window  config.yml `management: no_cut_window`: first and last
#'                day as "MM-DD"
in_no_cut_window <- function(datum, window = unlist(CONFIG$management$no_cut_window)) {
  month_day <- substr(sprintf("%08d", as.integer(datum)), 5, 8)
  bounds <- gsub("-", "", window)
  month_day >= bounds[1] & month_day <= bounds[2]
}

#' A share in tenths, so that an odds ratio is per 10 percentage points.
tenths <- function(share) 10 * share

#' The landscape columns of a variable at every radius.
landscape_columns <- function(variable, radii = unlist(CONFIG$landscape$radii_m)) {
  landscape_column(variable, radii)
}

#' The records of a habitat model, with the shares in tenths.
#'
#' @param data  revised_model_data() of one species and sample
#' @return `data` complete on the management variables, with SANGUISORBA_10,
#'         WET_10, X_10, EDGE_10 and every landscape column in tenths
habitat_model_data <- function(data) {
  mgmt_vars <- c("TIMING", "METHOD", "PLANT", "EVL_species")
  out <- data[stats::complete.cases(data[, mgmt_vars]), , drop = FALSE]
  share <- function(col) if (col %in% names(out)) tenths(out[[col]]) else NA_real_
  out$SANGUISORBA_10 <- share("SANGUISORBA_SHARE")
  out$WET_10 <- share("WET_SHARE")
  out$X_10 <- share("X_SHARE")
  out$EDGE_10 <- share("EDGE_SHARE")
  out$PLANT_LINEAR <- out$PLANT_QUANT
  out$PLANT_ORDERED <- factor(out$PLANT, levels = levels(out$PLANT), ordered = TRUE)
  landscape <- as.vector(outer(landscape_variables(), unlist(CONFIG$landscape$radii_m), landscape_column))
  for (col in intersect(landscape, names(out))) {
    out[[col]] <- tenths(out[[col]])
  }
  out
}

#' The landscape scale models of one species and sample.
#'
#' The management model plus one landscape variable at one radius, for every
#' variable and radius, all on the records complete on every landscape
#' column, so that their AIC values compare.
#'
#' @param data     habitat_model_data()
#' @param species  the species
#' @param sample   the sample
#' @return specs, each with `variable` and `radius`
landscape_scale_specs <- function(data, species, sample, variables = landscape_variables(),
                                  radii = unlist(CONFIG$landscape$radii_m)) {
  code <- species_code(species)
  suffix <- sample_suffix(sample)
  columns <- as.vector(outer(variables, radii, landscape_column))
  d <- data[stats::complete.cases(data[, columns]), , drop = FALSE]
  specs <- list(list(
    id = paste0(code, "_scale_none", suffix), label = "Management model, records with the landscape",
    group = "Landscape scale", formula = revised_formula(terms = MANAGEMENT_TERMS), data = d,
    sample = sample, variable = "none", radius = NA_real_
  ))
  for (v in variables) {
    for (r in radii) {
      specs[[length(specs) + 1]] <- list(
        id = sprintf("%s_scale_%s_%d%s", code, tolower(v), r, suffix),
        label = sprintf("Management model + %s within %d m", v, r),
        group = "Landscape scale",
        formula = revised_formula(terms = c(MANAGEMENT_TERMS, landscape_column(v, r))),
        data = d, sample = sample, variable = v, radius = r
      )
    }
  }
  specs
}

#' The scale of effect of each landscape variable.
#'
#' @param models  the fitted landscape_scale_specs()
#' @param specs   the specs
#' @return list(table = every variable x radius with its AIC, the difference
#'         to the management model alone and the odds ratio; best = the radius
#'         with the lowest AIC per variable)
landscape_scales <- function(models, specs) {
  rows <- lapply(specs, function(s) {
    m <- models[[s$id]]
    if (is.null(m$fit)) {
      return(NULL)
    }
    co <- summary(m$fit)$coefficients$cond
    term <- if (s$variable == "none") NA_character_ else landscape_column(s$variable, s$radius)
    data.frame(
      model = m$id, sample = m$sample, variable = s$variable, radius_m = s$radius, n_obs = m$n_input,
      AIC = stats::AIC(m$fit),
      odds_ratio = if (is.na(term)) NA_real_ else exp(co[term, 1]),
      or_low = if (is.na(term)) NA_real_ else exp(co[term, 1] - stats::qnorm(0.975) * co[term, 2]),
      or_high = if (is.na(term)) NA_real_ else exp(co[term, 1] + stats::qnorm(0.975) * co[term, 2]),
      p_value = if (is.na(term)) NA_real_ else co[term, 4],
      converged = tmb_converged(m),
      stringsAsFactors = FALSE
    )
  })
  table <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(table)) {
    return(list(table = data.frame(), best = data.frame()))
  }
  stopifnot(length(unique(table$n_obs)) == 1)
  reference <- table$AIC[table$variable == "none"]
  table$delta_AIC_vs_management <- table$AIC - reference
  table <- table[table$variable != "none", ]
  best <- table %>%
    dplyr::group_by(variable) %>%
    dplyr::slice_min(AIC, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
  list(table = table, best = best)
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

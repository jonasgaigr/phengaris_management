#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: the flight curve
#
#----------------------------------------------------------#
#
# Where in the flight season a visit falls decides how likely the species is
# to be found. Phase 3 placed each visit by DOY_DEV, its day of year minus the
# median day of year of the year's positive records of the species (P3.3),
# entered it as a quadratic and kept the visits within 45 days of the median.
# Phase 4 (P4.1) replaces both with a flight curve, as the butterfly
# monitoring schemes estimate it (Dennis et al. 2013, Schmucki et al. 2016):
# the expected count of adults at a visit across the season, pooled over the
# sites.
#
#   flight_curve_data()   the targeted visits with an exact date, their count
#                         (0 at a negative visit), DOY_DEV and elevation
#   fit_flight_curve()    per species, a negative-binomial GAM of the count
#                         against DOY_DEV, with random intercepts for the year
#                         and the mapping grid square; with the elevation and
#                         its interaction with DOY_DEV when that lowers the
#                         AIC, since the season starts later higher up
#   flight_curve_table()  the curve on a grid of DOY_DEV (and elevation), as
#                         the relative activity: the expected count divided by
#                         the peak of the curve at the same elevation
#   relative_activity()   the relative activity of each record
#
# Step 16 fits the curve and writes the table; step 04 gives each record its
# FLIGHT_ACTIVITY and LOG_ACTIVITY, and places it in or outside the flight
# season (R/functions/design_variables.R).
#
#----------------------------------------------------------#

#' The visits the flight curve is fitted on.
#'
#' Targeted records with an exact date (DATUM_OD = DATUM_DO), within
#' `max_deviation` days of their year's median, with a count: POCET for a
#' positive record, 0 for a negative one. A positive record without a count
#' is left out.
#'
#' @param records        occurrence records (records.csv of step 01)
#' @param flight_window  flight_window_reference(): the median day of year of
#'                       each species and year
#' @param site_design    site_design_table(), for the elevation of the site
#'                       (ELEVATION; NA when there is no elevation layer)
#' @param sources        ZDROJ values of the targeted monitoring
#' @param max_deviation  config.yml `design: flight_window:
#'                       max_abs_deviation_days`
#' @return one row per record: DRUH, YEAR, SITMAP, ID_LOKAL, DOY_DEV, COUNT,
#'         ELEVATION
flight_curve_data <- function(records, flight_window, site_design, sources = target_mon_zdroj,
                              max_deviation = CONFIG$design$flight_window$max_abs_deviation_days) {
  elevation <- if ("ELEVATION" %in% names(site_design)) {
    dplyr::select(site_design, ID_LOKAL, ELEVATION)
  } else {
    dplyr::transmute(site_design, ID_LOKAL, ELEVATION = NA_real_)
  }
  records %>%
    dplyr::filter(ZDROJ %in% sources, DATUM_OD == DATUM_DO) %>%
    dplyr::mutate(
      YEAR = as.integer(substr(DATUM_OD, 1, 4)),
      DOY = day_of_year(DATUM_OD)
    ) %>%
    dplyr::inner_join(dplyr::select(flight_window, DRUH, YEAR, median_doy), by = c("DRUH", "YEAR")) %>%
    dplyr::left_join(elevation, by = "ID_LOKAL") %>%
    dplyr::mutate(
      DOY_DEV = DOY - median_doy,
      COUNT = dplyr::if_else(NEGATIV == 1, 0, POCET)
    ) %>%
    dplyr::filter(!is.na(COUNT), abs(DOY_DEV) <= max_deviation) %>%
    dplyr::select(DRUH, YEAR, SITMAP, ID_LOKAL, DOY_DEV, COUNT, ELEVATION)
}

#' Fit the flight curve of one species.
#'
#' A negative-binomial GAM (mgcv::bam, fast REML) of the count against a
#' smooth of DOY_DEV with `k` basis functions, with random intercepts for the
#' year and the mapping grid square, which take up the differences in
#' abundance between years and places. With an elevation, both variants have
#' a smooth of the elevation, which shifts the abundance only and so leaves
#' the relative activity unchanged; the "elevation" variant adds the tensor
#' interaction ti(DOY_DEV, ELEVATION), through which the curve can shift and
#' change shape with elevation. The two are compared by AIC.
#'
#' @param data       flight_curve_data() of one species
#' @param k          config.yml `design: flight_curve: k`
#' @param elevation  "when_better" (the variant with the lower AIC), "always"
#'                   or "never"; "never" whenever the data have no elevation
#' @return list(model = the chosen bam fit, elevation = whether its curve
#'         changes with the elevation, comparison = AIC of the variants
#'         fitted, doy_range = the range of DOY_DEV in the data,
#'         elevation_range)
fit_flight_curve <- function(data, k = CONFIG$design$flight_curve$k,
                             elevation = c("when_better", "always", "never")) {
  elevation <- match.arg(elevation)
  data <- data %>%
    dplyr::mutate(YEAR_F = factor(YEAR), SITMAP_F = factor(SITMAP))
  has_elevation <- !all(is.na(data$ELEVATION))
  if (has_elevation) data <- dplyr::filter(data, !is.na(ELEVATION))
  if (!has_elevation) elevation <- "never"

  fit <- function(formula) {
    mgcv::bam(formula, family = mgcv::nb(), data = data, method = "fREML", discrete = TRUE)
  }
  base <- paste0(
    sprintf("COUNT ~ s(DOY_DEV, k = %d)", k),
    if (has_elevation) sprintf(" + s(ELEVATION, k = %d)", k),
    " + s(YEAR_F, bs = \"re\") + s(SITMAP_F, bs = \"re\")"
  )
  interaction <- sprintf("%s + ti(DOY_DEV, ELEVATION, k = c(%d, %d))", base, k, k)

  models <- list()
  if (elevation != "always") models$plain <- fit(stats::as.formula(base))
  if (elevation != "never") models$elevation <- fit(stats::as.formula(interaction))
  comparison <- data.frame(
    variant = names(models),
    formula = vapply(models, function(m) {
      paste(deparse(stats::formula(m), width.cutoff = 500L), collapse = " ")
    }, character(1)),
    n = nrow(data),
    AIC = vapply(models, stats::AIC, numeric(1)),
    deviance_explained = vapply(models, function(m) summary(m)$dev.expl, numeric(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
  chosen <- names(models)[which.min(comparison$AIC)]
  comparison$chosen <- comparison$variant == chosen

  list(
    model = models[[chosen]],
    elevation = chosen == "elevation",
    comparison = comparison,
    doy_range = range(data$DOY_DEV),
    elevation_range = if (has_elevation) range(data$ELEVATION, na.rm = TRUE) else c(NA_real_, NA_real_)
  )
}

#' The flight curve of one species on a grid, as relative activity.
#'
#' The expected count with the random intercepts left out, divided by its
#' largest value within the range of DOY_DEV of the visits, at the same
#' elevation: 1 at the peak of the season as far as the visits show it.
#' Outside that range the curve is the GAM's extrapolation, capped at 1. The
#' cap matters where the visits do not reach the peak: the P. teleius visits
#' begin when its season is already declining, and the extrapolated curve
#' would keep rising before them.
#'
#' @param curve           fit_flight_curve()
#' @param species         the species, written into the table
#' @param doy             the DOY_DEV grid (whole days)
#' @param elevation_step  config.yml `design: flight_curve: elevation_step_m`
#' @return one row per DOY_DEV (and elevation): DRUH, ELEVATION (NA when the
#'         curve has none), DOY_DEV, RELATIVE_ACTIVITY, IN_DATA_RANGE
flight_curve_table <- function(curve, species,
                               doy = seq(
                                 -CONFIG$design$flight_window$max_abs_deviation_days,
                                 CONFIG$design$flight_window$max_abs_deviation_days
                               ),
                               elevation_step = CONFIG$design$flight_curve$elevation_step_m) {
  m <- curve$model
  elevations <- if (curve$elevation) {
    seq(
      floor(curve$elevation_range[1] / elevation_step) * elevation_step,
      ceiling(curve$elevation_range[2] / elevation_step) * elevation_step,
      by = elevation_step
    )
  } else {
    NA_real_
  }
  grid <- expand.grid(DOY_DEV = doy, ELEVATION = elevations)
  # Any level will do: the random intercepts are excluded.
  grid$YEAR_F <- m$model$YEAR_F[1]
  grid$SITMAP_F <- m$model$SITMAP_F[1]
  # A curve without the interaction is the same at every elevation; it is
  # evaluated at the median elevation of the visits.
  if (!curve$elevation && "ELEVATION" %in% names(m$model)) grid$ELEVATION <- stats::median(m$model$ELEVATION)
  grid$eta <- as.numeric(stats::predict(m, grid, exclude = c("s(YEAR_F)", "s(SITMAP_F)"), type = "link"))
  grid$in_range <- grid$DOY_DEV >= curve$doy_range[1] & grid$DOY_DEV <= curve$doy_range[2]
  tibble::as_tibble(grid) %>%
    dplyr::group_by(ELEVATION) %>%
    dplyr::mutate(RELATIVE_ACTIVITY = pmin(1, exp(eta - max(eta[in_range])))) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      DRUH = species,
      ELEVATION = if (curve$elevation) ELEVATION else NA_real_,
      DOY_DEV = as.integer(DOY_DEV),
      RELATIVE_ACTIVITY,
      IN_DATA_RANGE = in_range
    )
}

#' The relative activity of the flight curve at each record.
#'
#' A record takes the curve of its species at its DOY_DEV and, when the
#' curve has an elevation, at the tabulated elevation nearest its own; a
#' record without an elevation, or beyond the tabulated range, takes the
#' nearest end of it.
#'
#' @param druh       species of each record
#' @param doy_dev    DOY_DEV of each record
#' @param elevation  elevation of each record (may be NA)
#' @param curve      flight_curve_table() of both species
#' @return the relative activity of each record, NA where DOY_DEV is outside
#'         the tabulated grid
relative_activity <- function(druh, doy_dev, elevation, curve) {
  out <- rep(NA_real_, length(druh))
  for (species in unique(curve$DRUH)) {
    tab <- curve[curve$DRUH == species, ]
    rows <- which(druh == species)
    if (!length(rows)) next
    if (all(is.na(tab$ELEVATION))) {
      key_record <- as.character(doy_dev[rows])
      key_curve <- as.character(tab$DOY_DEV)
    } else {
      levels <- sort(unique(tab$ELEVATION))
      e <- elevation[rows]
      e[is.na(e)] <- stats::median(levels)
      nearest <- levels[pmax(1, pmin(length(levels), round((e - levels[1]) / diff(levels[1:2])) + 1))]
      key_record <- paste(doy_dev[rows], nearest)
      key_curve <- paste(tab$DOY_DEV, tab$ELEVATION)
    }
    out[rows] <- tab$RELATIVE_ACTIVITY[match(key_record, key_curve)]
  }
  out
}

#' The flight season of each species from its curve: the days where the
#' relative activity is at least `threshold` of the peak.
#'
#' @param curve      flight_curve_table() of both species
#' @param threshold  config.yml `design: flight_window: min_relative_activity`
#' @return one row per species (and elevation): the peak day within the range
#'         of the visits, and the first and last day at or above the
#'         threshold, as DOY_DEV
flight_season_bounds <- function(curve, threshold = CONFIG$design$flight_window$min_relative_activity) {
  curve %>%
    dplyr::group_by(DRUH, ELEVATION) %>%
    dplyr::summarise(
      peak_doy_dev = DOY_DEV[IN_DATA_RANGE][which.max(RELATIVE_ACTIVITY[IN_DATA_RANGE])],
      first_doy_dev = min(DOY_DEV[RELATIVE_ACTIVITY >= threshold]),
      last_doy_dev = max(DOY_DEV[RELATIVE_ACTIVITY >= threshold]),
      data_first_doy_dev = min(DOY_DEV[IN_DATA_RANGE]),
      data_last_doy_dev = max(DOY_DEV[IN_DATA_RANGE]),
      .groups = "drop"
    )
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

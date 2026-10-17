#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: survey-design variables of the revised analysis
#
#----------------------------------------------------------#
#
# The monitoring is essentially one visit per site and year, so the models are
# naive occupancy models: a negative record mixes "absent" with "present but
# not detected" (REVIEW.md, item 11). The revised analysis does not separate
# the two, but it puts the main drivers of detection into every model as
# covariates (P3.3):
#
#   DOY_DEV       the visit's day of year minus the median day of year of the
#                 positive records of the same species in the same year, i.e.
#                 where the visit falls in that year's flight season
#   FLIGHT_ACTIVITY  the relative activity of the species' flight curve at
#                 that day (and the site's elevation; P4.1), which decides
#                 whether the visit is in the flight season
#   OBSERVER      who surveyed, normalised from AUTOR
#   PROTOCOL_ERA  the field form of 2019-2020 or of 2021-2024 (item 20)
#
# and it describes each record by its persistent site, the site's area and
# protection designated for the record's own species (P3.1, P3.4), and its
# connectivity (P3.5, R/functions/connectivity.R).
#
# Used by step 04 when the specification sets design_variables: true.
#
#----------------------------------------------------------#

#--------------------------------------------------#
## Dates and the flight season -----
#--------------------------------------------------#

#' Day of year of a date stored as the number YYYYMMDD.
#'
#' @param datum  numeric or character YYYYMMDD, as DATUM_OD of the exports
#' @return integer day of year, 1-366
day_of_year <- function(datum) {
  as.integer(format(as.Date(as.character(datum), format = "%Y%m%d"), "%j"))
}

#' The flight season of each species and year: the median day of year of the
#' positive records.
#'
#' Only records with an exact date (DATUM_OD = DATUM_DO) count: a record
#' dated by a range of days has no day of year.
#'
#' @param records  occurrence records, with DRUH, DATUM_OD, DATUM_DO, NEGATIV
#'                 and ZDROJ (records.csv of step 01)
#' @param sources  ZDROJ values to use, or NULL for every source
#' @param years    the years to describe
#' @return one row per species and year: positive records used, their median,
#'         minimum and maximum day of year
flight_window_reference <- function(records, sources = NULL,
                                    years = seq(CONFIG$years$first, CONFIG$years$last)) {
  records %>%
    dplyr::filter(
      NEGATIV == 0,
      DATUM_OD == DATUM_DO,
      is.null(sources) | ZDROJ %in% sources
    ) %>%
    dplyr::mutate(
      YEAR = as.integer(substr(DATUM_OD, 1, 4)),
      DOY = day_of_year(DATUM_OD)
    ) %>%
    dplyr::filter(YEAR %in% years) %>%
    dplyr::group_by(DRUH, YEAR) %>%
    dplyr::summarise(
      positive_records = dplyr::n(),
      median_doy = stats::median(DOY),
      min_doy = min(DOY),
      max_doy = max(DOY),
      .groups = "drop"
    )
}

#' Is a visit inside the flight season of its species?
#'
#' @param doy_dev   DOY_DEV of each visit
#' @param activity  the relative activity of the flight curve at each visit
#'                  (relative_activity()), or NA
#' @param settings  config.yml `design: flight_window`: `cutoff`
#'                  ("flight_curve" or "fixed"), `min_relative_activity` and
#'                  `max_abs_deviation_days`
#' @return TRUE inside the season. Under either rule a visit farther than
#'         `max_abs_deviation_days` from the median is outside; under
#'         "flight_curve" so is a visit where the relative activity is below
#'         `min_relative_activity`.
in_flight_season <- function(doy_dev, activity, settings = CONFIG$design$flight_window) {
  cutoff <- match.arg(settings$cutoff, c("flight_curve", "fixed"))
  inside <- !is.na(doy_dev) & abs(doy_dev) <= settings$max_abs_deviation_days
  if (cutoff == "flight_curve") {
    inside <- inside & !is.na(activity) & activity >= settings$min_relative_activity
  }
  inside
}

#' The field-form era of a year.
#'
#' @param year         calendar year
#' @param change_year  first year of the new form, config.yml `design:
#'                     protocol_change_year`
protocol_era <- function(year, change_year = CONFIG$design$protocol_change_year) {
  ifelse(
    year < change_year,
    sprintf("%d-%d", CONFIG$years$first, change_year - 1),
    sprintf("%d-%d", change_year, CONFIG$years$last)
  )
}

#--------------------------------------------------#
## Observers -----
#--------------------------------------------------#

#' One spelling per observer.
#'
#' Keeps the first of several authors, removes academic titles, folds case
#' and diacritics, splits "jmeno.prijmeni" e-mail-style names, and sorts the
#' words, so that "Jiří Švarc", "Švarc Jiří", "Jiří  Švarc" and "jiri.svarc"
#' are one observer. The aliases then merge what remains, e.g. a diminutive.
#'
#' @param x        AUTOR
#' @param aliases  named list, normalised name = normalised name it stands
#'                 for, config.yml `design: observer_aliases`
#' @return character, NA where AUTOR is missing
normalise_observer <- function(x, aliases = CONFIG$design$observer_aliases) {
  first <- trimws(sub("[,;].*$", "", x))
  plain <- tolower(stringi::stri_trans_general(first, "Latin-ASCII"))
  plain <- gsub("\\b(rndr|mgr|ing|bc|phd|ph\\.d|doc|prof|dr)\\.?(?=\\s|$)", " ", plain, perl = TRUE)
  words <- strsplit(trimws(plain), "[[:space:].]+")
  out <- vapply(words, function(w) {
    w <- w[nzchar(w)]
    if (!length(w)) NA_character_ else paste(sort(w), collapse = " ")
  }, character(1))
  out[is.na(x)] <- NA_character_
  if (length(aliases)) {
    hit <- out %in% names(aliases)
    out[hit] <- unlist(aliases[out[hit]], use.names = FALSE)
  }
  out
}

#--------------------------------------------------#
## The design variables -----
#--------------------------------------------------#

#' Add the survey-design variables to the cleaned records.
#'
#' @param data               records after derive_analysis_variables()
#' @param flight_window      flight_window_reference() of step 16
#' @param site_design        site_design_table() of step 16
#' @param connectivity       connectivity_table() of step 16
#' @param protected_area_id  the protected-area membership of step 03
#' @param flight_curve       flight_curve_table() of step 16, or NULL (only
#'                           with config.yml `design: flight_window: cutoff:
#'                           fixed`)
#' @param site_history       site_history_table() of step 16, or NULL
#' @param window             config.yml `design: flight_window`
#' @param plausible_km       config.yml `imputation: plausible_radius_km`
#' @return `data` with DOY, DOY_DEV, PROTOCOL_ERA, OBSERVER, EVL_species, the
#'         site design columns, the connectivity columns, FLIGHT_ACTIVITY and
#'         LOG_ACTIVITY (the relative activity of the flight curve and its
#'         log), IN_FLIGHT_WINDOW and IN_WINDOW_<d> for each of
#'         `window$sensitivity_days`; with the site history also
#'         NEAREST_POSITIVE_KM, HIST_POSITIVE, HIST_LAST_YEAR and
#'         IMPUTED_PLAUSIBLE (an imputed absence within `plausible_km` of a
#'         positive record of its species, P4.3); every row kept
add_design_variables <- function(data, flight_window, site_design, connectivity, protected_area_id,
                                 flight_curve = NULL, site_history = NULL, window = CONFIG$design$flight_window,
                                 plausible_km = CONFIG$imputation$plausible_radius_km) {
  if (is.null(flight_curve) && window$cutoff == "flight_curve") {
    stop("The flight season is cut by the flight curve, but no flight curve was given.", call. = FALSE)
  }
  # An EVL designated for the record's own species (REVIEW.md, item 24).
  designated <- protected_area_id %>%
    dplyr::filter(PA_TYPE == "EVL_target") %>%
    dplyr::distinct(ID_LOKAL, PA_SPECIES)
  designated_key <- paste(designated$ID_LOKAL, designated$PA_SPECIES)

  out <- data %>%
    dplyr::mutate(
      DOY = day_of_year(DATUM_OD),
      PROTOCOL_ERA = protocol_era(YEAR),
      OBSERVER = normalise_observer(AUTOR),
      EVL_species = as.numeric(paste(ID_LOKAL, DRUH) %in% designated_key)
    ) %>%
    dplyr::left_join(
      flight_window %>% dplyr::select(DRUH, YEAR, FLIGHT_MEDIAN_DOY = median_doy),
      by = c("DRUH", "YEAR")
    ) %>%
    dplyr::mutate(DOY_DEV = DOY - FLIGHT_MEDIAN_DOY) %>%
    dplyr::left_join(site_design, by = "ID_LOKAL") %>%
    dplyr::left_join(connectivity, by = c("ID_LOKAL", "YEAR", "DRUH"))

  out$FLIGHT_ACTIVITY <- if (is.null(flight_curve)) {
    NA_real_
  } else {
    elevation <- if ("ELEVATION" %in% names(out)) out$ELEVATION else rep(NA_real_, nrow(out))
    relative_activity(out$DRUH, out$DOY_DEV, elevation, flight_curve)
  }
  out$LOG_ACTIVITY <- log(out$FLIGHT_ACTIVITY)
  out$IN_FLIGHT_WINDOW <- in_flight_season(out$DOY_DEV, out$FLIGHT_ACTIVITY, window)
  for (d in unlist(window$sensitivity_days)) {
    out[[paste0("IN_WINDOW_", d)]] <- !is.na(out$DOY_DEV) & abs(out$DOY_DEV) <= d
  }

  if (!is.null(site_history)) {
    out <- out %>%
      dplyr::left_join(
        dplyr::select(site_history, ID_LOKAL, DRUH, NEAREST_POSITIVE_KM, HIST_POSITIVE, HIST_LAST_YEAR),
        by = c("ID_LOKAL", "DRUH")
      ) %>%
      dplyr::mutate(IMPUTED_PLAUSIBLE = IMPUTED & !is.na(NEAREST_POSITIVE_KM) & NEAREST_POSITIVE_KM <= plausible_km)
  }

  stopifnot(nrow(out) == nrow(data))
  out
}

#--------------------------------------------------#
## Unit of analysis -----
#--------------------------------------------------#

#' Keep one record per species x site x visit (P3.1).
#'
#' A visit is a site geometry (ID_LOKAL) on a date (DATUM_OD). When a species
#' has several records at one visit, the record kept is a positive one if
#' there is any, then an observed one before an imputed one, then the lowest
#' ID_NALEZ.
#'
#' @param data  records with DRUH, ID_LOKAL, DATUM_OD, POSITIVE, IMPUTED and
#'              ID_NALEZ
#' @return `data` without the extra records, in the original order
collapse_to_visits <- function(data) {
  data %>%
    dplyr::mutate(.order = dplyr::row_number()) %>%
    dplyr::group_by(DRUH, ID_LOKAL, DATUM_OD) %>%
    dplyr::arrange(dplyr::desc(POSITIVE), IMPUTED, ID_NALEZ, .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.order) %>%
    dplyr::select(-.order)
}

#--------------------------------------------------#
## Host-plant filter -----
#--------------------------------------------------#

#' Drop the records the specification's host-plant filter excludes.
#'
#' @param data  records with PLANT_QUAL (1 host plant present, 0 recorded
#'              absent, NA no information)
#' @param rule  "absent_or_unknown" (the original: keep only records with the
#'              host plant recorded present), "absent" (drop only the records
#'              with the host plant recorded absent) or "none"
filter_host_plant <- function(data, rule = c("absent_or_unknown", "absent", "none")) {
  rule <- match.arg(rule)
  switch(rule,
    absent_or_unknown = keep_host_plant_present(data),
    absent = dplyr::filter(data, is.na(PLANT_QUAL) | PLANT_QUAL != 0),
    none = data
  )
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

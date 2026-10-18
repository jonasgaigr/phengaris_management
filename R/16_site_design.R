#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#     Step 16 - Persistent sites, flight season and connectivity
#
#
#----------------------------------------------------------#
#
# The survey-design inputs of the revised analysis, which do not depend on
# any analytical choice and so are computed once:
#
#   site_design.csv    every site geometry with its persistent site
#                      (SITE_ID, at config.yml `sites: match_distance_m`,
#                      and at the sensitivity distances), its geometry type,
#                      area, a representative point (P3.1) and its elevation
#   flight_window.csv  per species and year, the median day of year of the
#                      positive records, against which every visit is placed
#                      in the flight season (P3.3)
#   flight_curve.csv   per species, the flight curve: the relative activity
#                      of adults by day from that median (and by elevation),
#                      which places every visit in the season (P4.1)
#   connectivity.csv   per surveyed site, year and species, the
#                      connectivity to the occupied sites around it (P3.5)
#   site_history.csv   per surveyed site and species, the distance to the
#                      nearest positive record of 2012-2024 and whether the
#                      species was found there in 2012-2018 (P4.3, P4.8)
#
# Step 04 joins them onto the records of the specifications with
# design_variables: true.
#
# Reads:  Data/Processed/common/records.csv, sites.gpkg,
#         records_2012_2018.csv, sites_2012_2018.gpkg   (step 01)
#         Data/Input/reference/elevation.tif   (optional)
# Writes: Data/Processed/common/site_design.csv, flight_window.csv,
#         flight_curve.csv, connectivity.csv, site_history.csv
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("16")

records <- read_records()
sites <- read_sites()

report_start(
  "16",
  "Persistent sites, flight season and connectivity",
  paste(
    "The survey-design inputs of the revised analysis: persistent sites",
    "reconstructed across years, the flight season of each species and year,",
    "and the connectivity of every surveyed site to the occupied sites",
    "around it."
  )
)

#----------------------------------------------------------#
# Persistent sites -----
#----------------------------------------------------------#

path_elevation <- file.path(CONFIG$paths$reference, "elevation.tif")
site_design <- site_design_table(
  sites,
  elevation = if (file.exists(path_elevation)) terra::rast(path_elevation) else NULL
)
if (!file.exists(path_elevation)) {
  report_warning(
    "No elevation model (", path_elevation, "): the sites have no elevation and",
    "the flight curve none. Run refresh_reference_layers(layers = \"elevation\")",
    "(R/tools/refresh_reference_layers.R)."
  )
}

report_table(
  summarise_persistent_sites(
    site_design,
    records %>% dplyr::filter(ZDROJ %in% target_mon_zdroj)
  ),
  paste(
    "Persistent sites of the targeted records, by matching distance (SITE_ID",
    "is the one the models use)"
  ),
  "16_persistent_sites"
)

report_table(
  site_design %>%
    dplyr::filter(ID_LOKAL %in% records$ID_LOKAL[records$ZDROJ %in% target_mon_zdroj]) %>%
    dplyr::group_by(GEOMETRY_TYPE) %>%
    dplyr::summarise(
      sites = dplyr::n(),
      median_area_ha = stats::median(SITE_AREA_HA),
      max_area_ha = max(SITE_AREA_HA),
      min_elevation = min(ELEVATION),
      median_elevation = stats::median(ELEVATION),
      max_elevation = max(ELEVATION),
      .groups = "drop"
    ),
  "Site geometries of the targeted records by type, with the polygon areas and the elevation (m)",
  "16_site_geometries"
)

#----------------------------------------------------------#
# Flight season -----
#----------------------------------------------------------#

window_settings <- CONFIG$design$flight_window
flight_window <- flight_window_reference(
  records,
  sources = if (window_settings$reference_sources == "targeted") target_mon_zdroj
)

report_table(
  flight_window,
  sprintf(
    paste(
      "Day of year of the positive records with an exact date, by species and",
      "year (sources: %s). The median is the reference of DOY_DEV."
    ),
    window_settings$reference_sources
  ),
  "16_flight_window"
)

#--------------------------------------------------#
## Flight curve -----
#--------------------------------------------------#

curve_settings <- CONFIG$design$flight_curve
curve_data <- flight_curve_data(records, flight_window, site_design)
curves <- lapply(
  stats::setNames(c(SPECIES_NAU, SPECIES_TEL), c(SPECIES_NAU, SPECIES_TEL)),
  function(species) {
    fit_flight_curve(
      curve_data[curve_data$DRUH == species, ],
      k = curve_settings$k, elevation = curve_settings$elevation
    )
  }
)
flight_curve <- dplyr::bind_rows(lapply(names(curves), function(s) flight_curve_table(curves[[s]], s)))

report_note(sprintf(
  paste(
    "The flight curve of each species (P4.1): a negative-binomial GAM of the",
    "number of adults counted at the targeted visits with an exact date (0 at a",
    "negative visit) against DOY_DEV, the day of year minus the year's median",
    "positive record, s(DOY_DEV, k = %d), with random intercepts for the year",
    "and the mapping grid square; the elevation and its interaction with DOY_DEV",
    "enter when they lower the AIC (`elevation: %s`). The relative activity is",
    "the expected count divided by the peak of the curve at the same elevation.",
    "A visit is in the flight season where it is at least %g (cut-off:",
    "%s). The visits cover the season unevenly: few lie more than 10 days",
    "before the median, so the early side of the curve rests on few visits."
  ),
  curve_settings$k, curve_settings$elevation,
  CONFIG$design$flight_window$min_relative_activity, CONFIG$design$flight_window$cutoff
))

report_table(
  dplyr::bind_rows(lapply(names(curves), function(s) {
    dplyr::mutate(curves[[s]]$comparison, DRUH = s, .before = 1)
  })) %>%
    dplyr::group_by(DRUH) %>%
    dplyr::mutate(delta_AIC = AIC - min(AIC), .after = AIC) %>%
    dplyr::ungroup(),
  "Flight curve with and without the elevation, by species (AIC of the negative-binomial GAMs)",
  "16_flight_curve_models"
)

report_table(
  curve_data %>%
    dplyr::mutate(days = cut(DOY_DEV, seq(-45, 45, by = 5), include.lowest = TRUE)) %>%
    dplyr::group_by(DRUH, days) %>%
    dplyr::summarise(
      visits = dplyr::n(),
      positive = sum(COUNT > 0),
      median_count_positive = stats::median(COUNT[COUNT > 0]),
      .groups = "drop"
    ),
  "Targeted visits the flight curve is fitted on, by species and 5-day class of DOY_DEV",
  "16_flight_curve_visits",
  max_rows = 40
)

# The season at the median, the lower and the upper decile of the sites'
# elevation (one row per species when the curve has no elevation).
elevation_of_sites <- stats::quantile(curve_data$ELEVATION, c(0.1, 0.5, 0.9), na.rm = TRUE)
bounds <- flight_season_bounds(flight_curve)
if (any(!is.na(bounds$ELEVATION))) {
  shown <- unique(unlist(lapply(elevation_of_sites, function(e) {
    levels <- sort(unique(bounds$ELEVATION[!is.na(bounds$ELEVATION)]))
    levels[which.min(abs(levels - e))]
  })))
  bounds <- dplyr::filter(bounds, is.na(ELEVATION) | ELEVATION %in% shown)
}
report_table(
  bounds,
  sprintf(
    paste(
      "Flight season from the curve: the peak and the first and last day (DOY_DEV)",
      "with a relative activity of at least %g, and the range of the visits;",
      "where the curve has an elevation, at the lower decile, the median and the",
      "upper decile of the visits' elevation (m)"
    ),
    CONFIG$design$flight_window$min_relative_activity
  ),
  "16_flight_season"
)

report_figure(
  plot_flight_curve(
    dplyr::filter(flight_curve, is.na(ELEVATION) | ELEVATION %in% bounds$ELEVATION),
    CONFIG$design$flight_window$min_relative_activity
  ),
  paste(
    "Flight curves: the relative activity of adults across the season, solid",
    "over the days the visits cover and dotted where the curve is extrapolated;",
    "where the curve has an elevation term, at the lower decile, the median and",
    "the upper decile of the sites' elevation. The flight season ends where the",
    "curve falls below the horizontal line"
  ),
  "16_flight_curve",
  height = 8.5
)

#----------------------------------------------------------#
# Connectivity -----
#----------------------------------------------------------#

connectivity <- connectivity_table(records, site_design)

conn_settings <- CONFIG$connectivity
report_note(sprintf(
  paste(
    "Connectivity S = sum exp(-alpha d) A^b over the sites occupied by the",
    "same species in the same or the previous year, other than the site's own",
    "persistent site, at every pair of alpha = %s per km and b = %s (P4.2);",
    "step 17 picks the pair (`connectivity: choice: %s`). Sources within %s km,",
    "occupied sites from %s sources. Point and line sources count with the",
    "median area of the species' occupied polygon sites. The 2019 values miss",
    "the 2018 populations, which the 2019-2024 exports do not hold. The table",
    "describes CONN, the fixed pair alpha = %s, b = %s."
  ),
  paste(unlist(conn_settings$alpha_grid_per_km), collapse = ", "),
  paste(unlist(conn_settings$area_exponent_grid), collapse = ", "),
  conn_settings$choice, conn_settings$max_distance_km, conn_settings$sources,
  conn_settings$alpha_per_km, conn_settings$area_exponent
))

report_table(
  connectivity %>%
    dplyr::group_by(DRUH, YEAR) %>%
    dplyr::summarise(
      sites = dplyr::n(),
      isolated = sum(CONN_SOURCES == 0),
      median_sources = stats::median(CONN_SOURCES),
      median_CONN = stats::median(CONN),
      p90_CONN = stats::quantile(CONN, 0.9),
      .groups = "drop"
    ),
  "Connectivity of the surveyed sites by species and year (isolated: no occupied site within reach)",
  "16_connectivity_summary"
)

#----------------------------------------------------------#
# The populations known around each site -----
#----------------------------------------------------------#

records_history <- read_records_history()
site_history <- site_history_table(
  site_design, sites, records, records_history, read_sites_history(),
  surveyed = unique(records$ID_LOKAL[records$ZDROJ %in% target_mon_zdroj])
)

report_note(sprintf(
  paste(
    "The populations known around the surveyed sites (P4.3, P4.8): the distance",
    "to the nearest positive record of the species from any source in",
    "2012-2024 (%d records of 2012-2018, %d of 2019-2024), and whether a site",
    "with a positive record of 2012-2018 lies within %g m. An imputed absence",
    "is plausible within %g km of such a record."
  ),
  sum(records_history$NEGATIV == 0), sum(records$NEGATIV == 0),
  CONFIG$sites$match_distance_m, CONFIG$imputation$plausible_radius_km
))
report_table(
  site_history %>%
    dplyr::group_by(DRUH) %>%
    dplyr::summarise(
      sites = dplyr::n(),
      within_1_km = sum(NEAREST_POSITIVE_KM <= 1),
      within_plausible_radius = sum(NEAREST_POSITIVE_KM <= CONFIG$imputation$plausible_radius_km),
      within_10_km = sum(NEAREST_POSITIVE_KM <= 10),
      median_km = stats::median(NEAREST_POSITIVE_KM),
      known_2012_2018 = sum(HIST_POSITIVE == 1),
      .groups = "drop"
    ),
  paste(
    "Surveyed sites by the distance to the nearest positive record of each species",
    "(2012-2024), and those with the species in 2012-2018"
  ),
  "16_site_history"
)

#----------------------------------------------------------#
# Validate -----
#----------------------------------------------------------#

validate_table(
  site_design, "site_design",
  columns = c(ID_LOKAL = "numeric", SITE_ID = "character", SITE_AREA_HA = "numeric"),
  key = "ID_LOKAL",
  not_na = c("ID_LOKAL", "SITE_ID", "GEOMETRY_TYPE", "SITE_X", "SITE_Y"),
  values = list(GEOMETRY_TYPE = c("point", "line", "polygon")),
  xy = c("SITE_X", "SITE_Y"),
  n_rows = nrow(sites)
)
validate_table(
  flight_window, "flight_window",
  key = c("DRUH", "YEAR"),
  not_na = c("median_doy"),
  values = list(DRUH = c(SPECIES_NAU, SPECIES_TEL)),
  n_rows = 2 * length(seq(CONFIG$years$first, CONFIG$years$last))
)
validate_table(
  flight_curve, "flight_curve",
  key = c("DRUH", "ELEVATION", "DOY_DEV"),
  not_na = c("DRUH", "DOY_DEV", "RELATIVE_ACTIVITY"),
  values = list(DRUH = c(SPECIES_NAU, SPECIES_TEL))
)
checkmate::assert_numeric(flight_curve$RELATIVE_ACTIVITY, lower = 0, upper = 1, .var.name = "RELATIVE_ACTIVITY")
validate_table(
  connectivity, "connectivity",
  key = c("ID_LOKAL", "YEAR", "DRUH"),
  not_na = names(connectivity)
)
validate_table(
  site_history, "site_history",
  key = c("ID_LOKAL", "DRUH"),
  not_na = c("ID_LOKAL", "DRUH", "NEAREST_POSITIVE_KM", "HIST_POSITIVE"),
  values = list(HIST_POSITIVE = c(0, 1))
)

#----------------------------------------------------------#
# Export -----
#----------------------------------------------------------#

readr::write_csv(site_design, processed_path("site_design.csv", "common"))
readr::write_csv(flight_window, processed_path("flight_window.csv", "common"))
readr::write_csv(flight_curve, processed_path("flight_curve.csv", "common"))
readr::write_csv(connectivity, processed_path("connectivity.csv", "common"))
readr::write_csv(site_history, processed_path("site_history.csv", "common"))

report_finish()

message("  ", nrow(site_design), " sites, ", nrow(connectivity), " site x species connectivities")
step_end("16")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

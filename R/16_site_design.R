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
#                      area and a representative point (P3.1)
#   flight_window.csv  per species and year, the median day of year of the
#                      positive records, against which every visit is placed
#                      in the flight season (P3.3)
#   connectivity.csv   per surveyed site, year and species, the
#                      connectivity to the occupied sites around it (P3.5)
#
# Step 04 joins them onto the records of the specifications with
# design_variables: true.
#
# Reads:  Data/Processed/common/records.csv, sites.gpkg   (step 01)
# Writes: Data/Processed/common/site_design.csv, flight_window.csv,
#         connectivity.csv
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

site_design <- site_design_table(sites)

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
      .groups = "drop"
    ),
  "Site geometries of the targeted records by type, with the polygon areas",
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

#----------------------------------------------------------#
# Connectivity -----
#----------------------------------------------------------#

connectivity <- connectivity_table(records, site_design)

conn_settings <- CONFIG$connectivity
report_note(sprintf(
  paste(
    "Connectivity S = sum exp(-alpha d) A^b over the sites occupied by the",
    "same species in the same or the previous year, other than the site's own",
    "persistent site: alpha = %s per km (sensitivity %s), b = %s, sources",
    "within %s km, occupied sites from %s sources. Point and line sources count",
    "with the median area of the species' occupied polygon sites. The 2019",
    "values miss the 2018 populations, which the 2019-2024 exports do not hold."
  ),
  conn_settings$alpha_per_km,
  paste(unlist(conn_settings$alpha_sensitivity_per_km), collapse = " and "),
  conn_settings$area_exponent, conn_settings$max_distance_km, conn_settings$sources
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
  connectivity, "connectivity",
  key = c("ID_LOKAL", "YEAR", "DRUH"),
  not_na = names(connectivity)
)

#----------------------------------------------------------#
# Export -----
#----------------------------------------------------------#

readr::write_csv(site_design, processed_path("site_design.csv", "common"))
readr::write_csv(flight_window, processed_path("flight_window.csv", "common"))
readr::write_csv(connectivity, processed_path("connectivity.csv", "common"))

report_finish()

message("  ", nrow(site_design), " sites, ", nrow(connectivity), " site x species connectivities")
step_end("16")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

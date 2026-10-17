#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#          Step 07 - Descriptive summary tables
#
#
#----------------------------------------------------------#
#
# The descriptive part of the results: how much was recorded, by whom, where,
# in what habitat, under what management and under what protection. Every table
# here goes into the manuscript or supports a number quoted in its text.
#
# Reads:  Data/Processed/<spec>/data_analysis.csv   (step 06)
# Writes: Outputs/<spec>/Tables/07_*.csv
#         Outputs/<spec>/Reports/07_descriptive_summaries.md
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("07")

report_start(
  "07",
  "Descriptive summaries",
  paste(
    "Extent of the monitoring effort and the distribution of the analysis",
    "variables across records, species, years, observers, habitats,",
    "management and protection categories."
  )
)

data <- read_data_analysis()

#----------------------------------------------------------#
# The records of the revised models -----
#----------------------------------------------------------#

# Under a specification with the survey-design variables, the records as the
# revised models see them: in the flight season, by outcome (positive,
# observed negative, imputed negative, plausible or not, P4.3), by position
# in the season, and the share of positive records at each value of the
# management model's variables in each of the three samples. Step 08 plots
# these tables.
if (isTRUE(spec_settings()$design_variables)) {
  report_section("The records of the revised models")

  report_table(
    summarise_records_by_outcome(data) %>%
      tidyr::pivot_wider(names_from = outcome, values_from = records, values_fill = 0),
    paste(
      "Records in the flight season by species, year and outcome. The imputed",
      "negatives are plausible when the visit falls in the species' flight",
      "season and a positive record of it lies within the plausibility radius",
      "(P4.3); the three samples of the models keep all of them, only the",
      "plausible ones, or none"
    ),
    "07_records_by_outcome"
  )

  visit_timing <- summarise_visit_timing(data)
  report_table(
    visit_timing %>%
      tidyr::pivot_wider(names_from = outcome, values_from = records, values_fill = 0),
    sprintf(
      paste(
        "Observed visits by their position in the season (classes of 5 days",
        "from the year's median positive record), outcome and whether they fall",
        "in the flight season of the curve (step 16); %d observed visits fall",
        "outside it and are not modelled"
      ),
      sum(visit_timing$records[!visit_timing$in_flight_season])
    ),
    "07_visit_timing",
    max_rows = 60
  )

  share_positive <- summarise_share_positive(data)
  report_table(
    share_positive,
    paste(
      "Share of the records in the flight season with the species at each value",
      "of the management model's variables, by species and sample, with a",
      "Jeffreys 95% interval; records with the variable unknown left out"
    ),
    "07_share_positive",
    max_rows = nrow(share_positive)
  )
}

#----------------------------------------------------------#
# General extent of the data -----
#----------------------------------------------------------#

report_section("General extent")

occurrence_extent <- data.frame(
  measure = c(
    "records",
    "distinct localities",
    "distinct mapping fields",
    "distinct observers",
    "years covered"
  ),
  value = c(
    nrow(data),
    dplyr::n_distinct(data$ID_LOKAL),
    dplyr::n_distinct(data$SITMAP),
    dplyr::n_distinct(data$AUTOR),
    dplyr::n_distinct(data$YEAR)
  ),
  stringsAsFactors = FALSE
)

report_table(
  occurrence_extent,
  "Extent of the analysed data set",
  "07_data_extent"
)

data_sum <- summarise_occupancy(data)
report_table(
  data_sum,
  "Records by species and site occupancy",
  "07_occupancy_by_species"
)

positivity <- summarise_positivity(data)
report_table(
  positivity,
  "Share of positive records by species and year",
  "07_positivity_by_year"
)

roky <- summarise_years_per_locality(data)
report_table(
  roky,
  "Number of monitored years per locality",
  "07_years_per_locality"
)

#----------------------------------------------------------#
# Temporal coverage -----
#----------------------------------------------------------#

report_section("Temporal coverage")

year_stats <- summarise_year_stats(data)
report_table(
  year_stats,
  "Records by year and site occupancy",
  "07_records_by_year"
)

#----------------------------------------------------------#
# Observers -----
#----------------------------------------------------------#

report_section("Observers")

observer_stats <- summarise_observers(data)
report_table(
  observer_stats,
  "Records per observer",
  "07_records_per_observer"
)

report_note(sprintf(
  "%d observers contributed, with a mean of %.1f and a median of %.0f records each.",
  nrow(observer_stats),
  mean(observer_stats$obs_num),
  stats::median(observer_stats$obs_num)
))

report_table(
  summarise_fields_per_observer(data),
  "Mapping fields covered per observer and year",
  "07_fields_per_observer"
)

report_table(
  summarise_sites_per_field(data),
  "Monitored sites per mapping field",
  "07_sites_per_field"
)

#----------------------------------------------------------#
# Occurrence and abundance -----
#----------------------------------------------------------#

report_section("Occurrence and abundance")

data_spe_sum <- summarise_species_cooccurrence(data)
report_table(
  data_spe_sum,
  "Records by occupancy, species and co-occurrence of the other species",
  "07_species_cooccurrence"
)

report_table(
  summarise_abundance(data),
  "Counted specimens per species",
  "07_abundance_by_species"
)

report_table(
  summarise_grid_coverage(data),
  "Mapping fields covered, overall and per occupied species",
  "07_grid_coverage"
)

#----------------------------------------------------------#
# Habitat and host plant -----
#----------------------------------------------------------#

report_section("Habitat and host plant")

habitat_counts <- summarise_habitat_counts(data)
report_table(
  habitat_counts,
  "Recorded habitat types by species and occupancy",
  "07_habitat_type_counts"
)

sumob_phenau <- summarise_recorded_habitats(data, SPECIES_NAU)
sumob_phetel <- summarise_recorded_habitats(data, SPECIES_TEL)

report_table(
  sumob_phenau,
  "Recorded habitats at occupied P. nausithous sites",
  "07_recorded_habitats_nausithous"
)
report_table(
  sumob_phetel,
  "Recorded habitats at occupied P. teleius sites",
  "07_recorded_habitats_teleius"
)

data_plant_sum <- summarise_host_plant(data)
report_table(
  data_plant_sum,
  "Host plant abundance by occupancy and species",
  "07_host_plant_abundance"
)

#----------------------------------------------------------#
# Management -----
#----------------------------------------------------------#

report_section("Management")

data_method_sum <- summarise_method(data)
report_table(
  data_method_sum,
  "Records by mowing method",
  "07_management_method"
)

data_time_sum <- summarise_timing(data)
report_table(
  data_time_sum,
  "Records by mowing timing",
  "07_management_timing"
)

data_man_sum <- summarise_management(data)
report_table(
  data_man_sum,
  "Records by the combination of mowing method and timing",
  "07_management_combined"
)

summan_phenau <- summarise_management_types(data, SPECIES_NAU)
summan_phetel <- summarise_management_types(data, SPECIES_TEL)

report_table(
  summan_phenau,
  "Management recorded at occupied P. nausithous sites",
  "07_management_types_nausithous"
)
report_table(
  summan_phetel,
  "Management recorded at occupied P. teleius sites",
  "07_management_types_teleius"
)

#----------------------------------------------------------#
# Protected areas -----
#----------------------------------------------------------#

report_section("Protected areas")

data_evl_sum <- summarise_protection(data)
report_table(
  data_evl_sum,
  "Records by all protection variables",
  "07_protection_full",
  max_rows = 40
)

report_table(
  summarise_evl(data),
  "Records by Natura 2000 membership",
  "07_protection_evl"
)

report_table(
  summarise_evl_target(data),
  "Records by Natura 2000 designation for Phengaris",
  "07_protection_evl_target"
)

report_table(
  summarise_evl_combined(data),
  "Records by combined Natura 2000 status",
  "07_protection_evl_combined"
)

report_table(
  summarise_mzchu(data),
  "Records by small-scale protected area membership",
  "07_protection_mzchu"
)

report_table(
  summarise_mapping_fields(data),
  "Occupancy per species and range polygon",
  "07_mapping_fields"
)

report_finish()

step_end("07")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

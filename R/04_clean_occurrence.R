#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#        Step 04 - Build the cleaned occurrence table
#
#
#----------------------------------------------------------#
#
# Turns the raw monitoring records into the analysis variables: occupancy,
# year, protection status, host plant abundance, habitat type, threats and
# pressures, and the recorded management.
#
# Most variables are read out of two free-text fields the surveyors fill in:
#
#   POP_BIOT     habitat description  -> TTP, PRIKOP, ZARUST, JINY
#   STRUKT_POZN  structured notes     -> host plant, threats, management
#
# Which records the host-plant filter drops, and how the threat list is
# turned into threat classes, are set per specification (config.yml). A
# specification with design_variables: true also gets the survey-design
# variables of the revised analysis (observer, day of year in the flight
# season, protocol era, persistent site, site area, protection designated for
# the record's own species, connectivity), and keeps one record per species x
# site x visit (R/functions/design_variables.R).
#
# Reads:  Data/Processed/<spec>/data_with_imputed.csv   (step 02)
#         Data/Processed/common/protected_area_id.csv   (step 03)
#         Data/Processed/common/site_design.csv, flight_window.csv,
#         connectivity.csv   (step 16; design_variables only)
# Writes: Data/Processed/<spec>/data_clean.csv
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("04")

settings <- spec_settings()
host_plant_rule <- if (is.null(settings$host_plant_filter)) "absent_or_unknown" else settings$host_plant_filter
threat_vocabulary <- if (is.null(settings$threat_vocabulary)) "legacy" else settings$threat_vocabulary
design_variables <- isTRUE(settings$design_variables)

report_start(
  "04",
  "Cleaned occurrence data",
  paste(
    "Derivation of the analysis variables from the targeted monitoring",
    "records. Only records from the targeted monitoring campaigns are kept,",
    switch(host_plant_rule,
      absent_or_unknown = "and only records with the host plant present.",
      absent = "and the records with the host plant recorded absent are dropped.",
      none = "whatever their host-plant information."
    )
  )
)

#----------------------------------------------------------#
# Load -----
#----------------------------------------------------------#

# Every row is used to guess the column types: a free-text column empty in
# the first rows would otherwise be read as logical and lose its values.
data_with_imputed <-
  readr::read_csv(
    processed_path("data_with_imputed.csv"),
    show_col_types = FALSE,
    guess_max = Inf
  )

protected_area_id <-
  readr::read_csv(
    processed_path("protected_area_id.csv", "common"),
    show_col_types = FALSE
  )

#----------------------------------------------------------#
# Keep targeted monitoring only -----
#----------------------------------------------------------#

data_new_source <-
  data_with_imputed %>%
  dplyr::filter(
    ZDROJ %in% target_mon_zdroj
  )

#----------------------------------------------------------#
# Visits covering both species -----
#----------------------------------------------------------#

# Both species monitored on the visit, regardless of the outcome.
phengaris_both_monitored <- both_species_visits(data_new_source)

# Both species actually found on the visit.
phengaris_both_present <-
  both_species_visits(
    data_new_source %>%
      dplyr::filter(
        NEGATIV == 0 # drop negative occurrence records
      )
  )

#----------------------------------------------------------#
# Derive the analysis variables -----
#----------------------------------------------------------#

# Every targeted record with its analysis variables, before the host-plant
# filter.
data_derived <-
  derive_analysis_variables(
    data_new_source,
    protected_area_id,
    both_present = phengaris_both_present,
    threat_vocabulary = threat_vocabulary
  )

data_clean <- filter_host_plant(data_derived, host_plant_rule)

#----------------------------------------------------------#
# Survey design and unit of analysis -----
#----------------------------------------------------------#

if (design_variables) {
  require_inputs(c(
    "{processed}/common/site_design.csv",
    "{processed}/common/flight_window.csv",
    "{processed}/common/flight_curve.csv",
    "{processed}/common/connectivity.csv",
    "{processed}/common/site_history.csv"
  ))
  n_before_collapse <- nrow(data_clean)
  data_clean <-
    data_clean %>%
    add_design_variables(
      flight_window = readr::read_csv(processed_path("flight_window.csv", "common"), show_col_types = FALSE),
      site_design = readr::read_csv(processed_path("site_design.csv", "common"), show_col_types = FALSE),
      connectivity = readr::read_csv(processed_path("connectivity.csv", "common"), show_col_types = FALSE),
      protected_area_id = protected_area_id,
      flight_curve = readr::read_csv(processed_path("flight_curve.csv", "common"), show_col_types = FALSE),
      site_history = readr::read_csv(processed_path("site_history.csv", "common"), show_col_types = FALSE),
      # Written by step 21 only when the landscape rasters are there.
      landscape = if (file.exists(processed_path("landscape.csv", "common"))) {
        readr::read_csv(processed_path("landscape.csv", "common"), show_col_types = FALSE)
      }
    ) %>%
    collapse_to_visits()
  # A threat class absent from the threat list of one field form is unknown
  # there, not absent.
  if (threat_vocabulary == "harmonised") {
    unlisted <- mask_unlisted_threats(data_clean)
    data_clean <- unlisted$data
  }
}

#----------------------------------------------------------#
# Results -----
#----------------------------------------------------------#

cleaning_summary <- data.frame(
  stage = c(
    "records with imputed absences (step 02)",
    "targeted monitoring only",
    switch(host_plant_rule,
      absent_or_unknown = "host plant recorded present",
      absent = "host plant not recorded absent",
      none = "host-plant filter not applied"
    )
  ),
  records = c(
    nrow(data_with_imputed),
    nrow(data_new_source),
    if (design_variables) n_before_collapse else nrow(data_clean)
  ),
  stringsAsFactors = FALSE
)
if (design_variables) {
  cleaning_summary <- rbind(
    cleaning_summary,
    data.frame(stage = "one record per species x site x visit", records = nrow(data_clean))
  )
}

report_table(
  cleaning_summary,
  "Records retained at each filtering stage",
  "04_cleaning_summary"
)

# What the host-plant filter drops: records with the host plant recorded
# absent (a site without Sanguisorba cannot hold the species) and records
# without any host-plant information.
host_plant_filter <-
  data_derived %>%
  dplyr::mutate(
    host_plant = dplyr::case_when(
      is.na(PLANT_QUAL) ~ paste(
        "no host-plant information",
        if (host_plant_rule == "absent_or_unknown") "(dropped)" else "(kept)"
      ),
      PLANT_QUAL == 0 ~ paste(
        "host plant recorded absent",
        if (host_plant_rule == "none") "(kept)" else "(dropped)"
      ),
      TRUE ~ "host plant present (kept)"
    )
  ) %>%
  dplyr::count(DRUH, YEAR, host_plant, name = "records")

report_table(
  host_plant_filter,
  "Records by host-plant information, species and year",
  "04_host_plant_filter",
  max_rows = 40
)

protection_counts <-
  data_clean %>%
  dplyr::summarise(
    dplyr::across(
      c(EVL, EVL_target, MZCHU, PROTECT, SPEC_NUM),
      ~ sum(.x == 1, na.rm = TRUE)
    )
  ) %>%
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to  = "variable",
    values_to = "records_flagged"
  ) %>%
  dplyr::mutate(
    records_total = nrow(data_clean),
    percent       = round(100 * records_flagged / records_total, 1)
  )

report_table(
  protection_counts,
  "Records flagged by each binary site attribute",
  "04_flag_counts"
)

report_table(
  phengaris_both_monitored %>%
    dplyr::count(name = "visits") %>%
    dplyr::mutate(description = "visits at which both species were monitored") %>%
    dplyr::bind_rows(
      phengaris_both_present %>%
        dplyr::count(name = "visits") %>%
        dplyr::mutate(description = "visits at which both species were found")
    ) %>%
    dplyr::select(description, visits),
  "Visits covering both species",
  "04_both_species_visits"
)

#--------------------------------------------------#
## Corrections applied in this step -----
#--------------------------------------------------#

report_section("Corrections applied to the original code")

report_warning(
  "MZCHU and SPEC_NUM were previously derived with `ID %in% <data frame>`",
  "rather than `ID %in% <vector of ids>`. Comparing against a data frame made",
  "both variables constant: in the previous `data_clean.csv` all 4540 records",
  "had MZCHU = 0 and SPEC_NUM = 0, and PROTECT was therefore an exact copy of",
  "EVL. Every model and figure using MZCHU, PROTECT or SPEC_NUM was fitted on",
  "a constant. Both now compare against the identifier vector."
)

report_note(paste(
  "The MZCHU flag now marks",
  sum(data_clean$MZCHU == 1),
  "of", nrow(data_clean),
  "records as lying inside a small-scale specially protected area, and",
  "PROTECT is no longer identical to EVL."
))

# SPEC_NUM inherits one ID_NALEZ per visit from the summarise above, so it
# marks one record per both-species visit rather than every record of such a
# visit. The visit-level alternative is reported here so that the difference is
# visible before it is used in the manuscript.
spec_num_visit_level <-
  data_clean %>%
  dplyr::mutate(
    .visit = paste(NAZ_LOKAL, DATUM_OD),
    .both = .visit %in% paste(
      phengaris_both_present$NAZ_LOKAL,
      phengaris_both_present$DATUM_OD
    )
  ) %>%
  dplyr::summarise(records = sum(.both)) %>%
  dplyr::pull(records)

report_warning(
  "SPEC_NUM keeps the original definition, which takes one ID_NALEZ per",
  "both-species visit, and so now flags", sum(data_clean$SPEC_NUM == 1),
  "records. Defining it at the visit level instead, by matching site and date,",
  "would flag", spec_num_visit_level, "records. Decide which of the two the",
  "co-occurrence model should use before the manuscript is finalised."
)

# EVL_target keeps the original, species-blind definition: a record is flagged
# when its site lies in a Natura 2000 site designated for *either* Phengaris
# species. Step 03 now records which species each site was designated for, so
# the species-specific alternative can be quantified without changing the flag.
# PA_SPECIES is only written by step 03 from this version onwards. When an
# older protected_area_id.csv is in place the comparison is simply skipped:
# it is a diagnostic, and it must not stop the cascade.
if ("PA_SPECIES" %in% names(protected_area_id)) {
  designated_site_species <-
    protected_area_id %>%
    dplyr::filter(PA_TYPE == "EVL_target") %>%
    dplyr::transmute(key = paste(ID_LOKAL, PA_SPECIES)) %>%
    dplyr::pull(key) %>%
    unique()

  evl_target_species_specific <- sum(
    paste(data_clean$ID_LOKAL, data_clean$DRUH) %in% designated_site_species
  )

  report_warning(
    "EVL_target keeps the original definition, under which a record is flagged",
    "when its site lies in a Natura 2000 site designated for either Phengaris",
    "species. It flags", sum(data_clean$EVL_target == 1), "records. Matching",
    "the designation to the species of the record instead would flag",
    evl_target_species_specific, "records. This was left unchanged because it",
    "is an interpretation of the variable, not a coding error."
  )
} else {
  report_warning(
    "EVL_target keeps the original definition, under which a record is flagged",
    "when its site lies in a Natura 2000 site designated for either Phengaris",
    "species. It flags", sum(data_clean$EVL_target == 1), "records. The",
    "species-specific alternative could not be quantified because",
    "protected_area_id.csv predates the PA_SPECIES column; re-run step 03 to",
    "get that comparison."
  )
}

#--------------------------------------------------#
## Survey-design variables -----
#--------------------------------------------------#

if (threat_vocabulary == "harmonised") {
  report_section("Threat classes from the harmonised vocabulary")

  coverage <- threat_vocabulary_coverage(parse_strukt_pozn(data_new_source$STRUKT_POZN)$threats)
  report_table(
    coverage$items %>% dplyr::filter(records > 0),
    paste(
      "Items of both threat lists, the class each is mapped to and the",
      "targeted records listing it (config.yml `strukt_pozn: threat_items`)"
    ),
    "04_threat_items",
    max_rows = 60
  )
  report_table(
    coverage$unrecognised,
    "Text of the threat lists that matches no known item (free-text notes, mostly)",
    "04_threat_items_unrecognised"
  )
  if (design_variables) {
    report_table(
      unlisted$masked,
      sprintf(
        paste(
          "Threat classes off the threat list of a field form: recorded at fewer",
          "than %d records of the era, so set to NA there (unknown, not absent)"
        ),
        CONFIG$strukt_pozn$threat_min_records_per_era
      ),
      "04_threat_classes_off_form"
    )
  }
}

if (design_variables) {
  report_section("Survey-design variables")

  report_note(sprintf(
    paste(
      "%d records of one species at the same site and visit were merged into",
      "one, keeping a positive record before a negative one (P3.1)."
    ),
    n_before_collapse - nrow(data_clean)
  ))

  window_settings <- CONFIG$design$flight_window
  sensitivity_windows <- unlist(window_settings$sensitivity_days)
  report_table(
    data_clean %>%
      dplyr::group_by(DRUH, PROTOCOL_ERA) %>%
      dplyr::summarise(
        records = dplyr::n(),
        positive = sum(POSITIVE == 1),
        imputed = sum(IMPUTED),
        outside_flight_window = sum(!IN_FLIGHT_WINDOW),
        positive_outside = sum(!IN_FLIGHT_WINDOW & POSITIVE == 1),
        dplyr::across(
          dplyr::all_of(paste0("IN_WINDOW_", sensitivity_windows)),
          ~ sum(!.x),
          .names = "outside_{sub('IN_WINDOW_', '', .col)}_days"
        ),
        min_doy_dev = min(DOY_DEV),
        max_doy_dev = max(DOY_DEV),
        median_activity = stats::median(FLIGHT_ACTIVITY, na.rm = TRUE),
        .groups = "drop"
      ),
    if (window_settings$cutoff == "flight_curve") {
      sprintf(
        paste(
          "Records by species and field-form era; DOY_DEV is the day of year",
          "minus the median of the year's positive records. A record is outside",
          "the flight season where the flight curve of its species is below %g",
          "of its peak, or more than %d days from the median, and is left out of",
          "the models. outside_<d>_days: outside the fixed windows of the",
          "sensitivity analysis."
        ),
        window_settings$min_relative_activity, window_settings$max_abs_deviation_days
      )
    } else {
      sprintf(
        paste(
          "Records by species and field-form era; DOY_DEV is the day of year",
          "minus the median of the year's positive records. Records more than",
          "%d days from it lie outside the flight season and are left out of the",
          "models."
        ),
        window_settings$max_abs_deviation_days
      )
    },
    "04_design_by_era"
  )

  report_table(
    data_clean %>%
      dplyr::filter(IMPUTED) %>%
      dplyr::group_by(DRUH, PROTOCOL_ERA) %>%
      dplyr::summarise(
        imputed = dplyr::n(),
        in_flight_season = sum(IN_FLIGHT_WINDOW),
        plausible = sum(IMPUTED_PLAUSIBLE & IN_FLIGHT_WINDOW),
        median_km_to_positive = stats::median(NEAREST_POSITIVE_KM),
        .groups = "drop"
      ),
    sprintf(
      paste(
        "Imputed absences: those in the imputed species' flight season, and of",
        "these the plausible ones, within %g km of a positive record of the",
        "species (any source, 2012-2024; P4.3)"
      ),
      CONFIG$imputation$plausible_radius_km
    ),
    "04_imputed_plausible"
  )

  report_table(
    data_clean %>%
      dplyr::filter(!IN_FLIGHT_WINDOW) %>%
      dplyr::select(ID_RECORD, DRUH, DATUM_OD, DOY, DOY_DEV, ELEVATION, FLIGHT_ACTIVITY, POSITIVE, IMPUTED, AUTOR),
    "Records outside the flight season",
    "04_outside_flight_window",
    max_rows = 60
  )

  observers <- data_clean %>%
    dplyr::filter(!is.na(OBSERVER)) %>%
    dplyr::group_by(OBSERVER) %>%
    dplyr::summarise(
      spellings = paste(sort(unique(AUTOR)), collapse = " / "),
      records = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(records))
  report_note(sprintf(
    paste(
      "%d spellings of AUTOR give %d observers (median %g records, at most",
      "%d); %d records have no author."
    ),
    dplyr::n_distinct(data_clean$AUTOR, na.rm = TRUE), nrow(observers),
    stats::median(observers$records), max(observers$records), sum(is.na(data_clean$OBSERVER))
  ))
  report_table(
    observers %>% dplyr::filter(grepl(" / ", spellings, fixed = TRUE)),
    "Observers written in more than one way, merged by the normalisation",
    "04_observer_spellings"
  )

  report_table(
    data_clean %>%
      dplyr::group_by(DRUH) %>%
      dplyr::summarise(
        EVL_any = sum(EVL == 1),
        EVL_either_species = sum(EVL_target == 1),
        EVL_own_species = sum(EVL_species == 1),
        polygon_sites = sum(GEOMETRY_TYPE == "polygon"),
        isolated = sum(CONN_SOURCES == 0),
        .groups = "drop"
      ),
    paste(
      "Records inside an EVL, one designated for either Phengaris species",
      "(EVL_target, the original flag) and one designated for the record's own",
      "species (EVL_species, used by the revised models); records on polygon",
      "sites; records with no occupied site of the species within reach"
    ),
    "04_design_flags"
  )
}

#----------------------------------------------------------#
# Validate -----
#----------------------------------------------------------#

flag_values <- c(0, 1)
validate_table(
  data_clean, "data_clean",
  columns = c(
    POSITIVE = "numeric", YEAR = "numeric", PLANT_QUANT = "numeric",
    EVL = "numeric", EVL_target = "numeric", MZCHU = "numeric",
    METHOD = "numeric", TIMING = "numeric", SUM_THREATS = "numeric"
  ),
  key = "ID_RECORD",
  # The host plant is known on every record only when the filter drops the
  # records without host-plant information.
  not_na = c(
    "POSITIVE", "YEAR", "EVL", "EVL_target", "MZCHU", "PROTECT",
    if (host_plant_rule == "absent_or_unknown") "PLANT_QUANT"
  ),
  values = list(
    POSITIVE = flag_values,
    YEAR = seq(CONFIG$years$first, CONFIG$years$last),
    PLANT_QUANT = 1:3,
    EVL = flag_values, EVL_target = flag_values, MZCHU = flag_values,
    PROTECT = flag_values, METHOD = flag_values, TIMING = flag_values,
    GRAZE = flag_values, GRAZE_MET = flag_values,
    TTP = flag_values, PRIKOP = flag_values, ZARUST = flag_values, JINY = flag_values
  ),
  xy = c("X", "Y"),
  max_rows = nrow(data_with_imputed)
)
if (design_variables) {
  validate_table(
    data_clean, "data_clean (design variables)",
    columns = c(DOY_DEV = "numeric", SITE_ID = "character", CONN = "numeric", EVL_species = "numeric"),
    key = c("DRUH", "ID_LOKAL", "DATUM_OD"),
    not_na = c("DOY", "DOY_DEV", "PROTOCOL_ERA", "SITE_ID", "SITE_X", "SITE_Y", "GEOMETRY_TYPE", "CONN"),
    values = list(EVL_species = flag_values, IN_FLIGHT_WINDOW = c(TRUE, FALSE)),
    xy = c("SITE_X", "SITE_Y")
  )
  # Every record in the flight season has a relative activity, whichever
  # rule cuts the season.
  checkmate::assert_true(
    !anyNA(data_clean$LOG_ACTIVITY[data_clean$IN_FLIGHT_WINDOW]),
    .var.name = "LOG_ACTIVITY known for every record in the flight season"
  )
  checkmate::assert_true(
    all(data_clean$EVL_species <= data_clean$EVL_target),
    .var.name = "EVL_species implies EVL_target"
  )
}

#----------------------------------------------------------#
# Export -----
#----------------------------------------------------------#

readr::write_csv(
  data_clean,
  processed_path("data_clean.csv")
)

report_finish()

message("  ", nrow(data_clean), " cleaned records written")
step_end("04")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

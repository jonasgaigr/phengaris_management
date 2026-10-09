#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#     Step 02 - Impute absences inside the reporting ranges
#
#
#----------------------------------------------------------#
#
# The two species are surveyed on the same visits, so a targeted record of one
# species at a place inside the other species' reporting range is evidence that
# the other species was looked for and not found. This step turns that evidence
# into explicit absence records.
#
#   1. Intersect the targeted records of each species with its own reporting
#      range, keeping row_n as the identifier of the range polygon.
#   2. For each species, take the targeted records of the *other* species that
#      fall inside this species' range and have no record of this species yet,
#      and add them as absences (NEGATIV = 1, IMPUTED = TRUE).
#
# Reads:  Data/Processed/common/records.csv, sites.gpkg   (step 01)
#         the reporting range layers named in config.yml
# Writes: Data/Processed/<spec>/data_with_imputed.csv
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("02")

phengaris_lokal_new <- read_records_with_sites()
range_nausithous <- read_range(CONFIG$paths$ranges$nausithous)
range_teleius <- read_range(CONFIG$paths$ranges$teleius)

report_start(
  "02",
  "Imputed absences",
  paste(
    "Absence records inferred from the joint monitoring of the two species.",
    "A targeted record of one species inside the other species' reporting",
    "range, where the other species has no record, is entered as an absence",
    "of that other species."
  )
)

#----------------------------------------------------------#
# Observed targeted records and imputed absences -----
#----------------------------------------------------------#

# The imputation rule and the handling of records outside their species'
# own range are set per specification in config.yml.
settings <- spec_settings()

data_with_imputed <- impute_absences(
  phengaris_lokal_new,
  range_nausithous,
  range_teleius,
  sources = target_mon_zdroj,
  rule = settings$imputation_rule,
  keep_outside_range = settings$keep_outside_range
)

#----------------------------------------------------------#
# Results -----
#----------------------------------------------------------#

imputation_summary <- summarise_imputation(data_with_imputed)

imputation_totals <-
  data_with_imputed %>%
  dplyr::count(IMPUTED, name = "records")

report_table(
  imputation_totals,
  "Observed and imputed records",
  "02_imputation_totals"
)

report_table(
  imputation_summary,
  "Records by species, presence/absence and imputation status",
  "02_imputation_by_species"
)

report_note(sprintf(
  paste(
    "Imputation rule `%s`, records outside the species' own reporting range",
    "%s. The imputation produced %d absence records out of %d records in total."
  ),
  settings$imputation_rule,
  if (isTRUE(settings$keep_outside_range)) "kept" else "dropped",
  sum(data_with_imputed$IMPUTED), nrow(data_with_imputed)
))

#--------------------------------------------------#
## Imputed absences by reporting cell -----
#--------------------------------------------------#

# Absences are imputed only inside the imputed species' own reporting range,
# the 10 x 10 km cells where the species is expected; this table shows how
# they spread over those cells.
imputation_by_cell <- dplyr::bind_rows(lapply(
  list(list(SPECIES_NAU, range_nausithous), list(SPECIES_TEL, range_teleius)),
  function(x) {
    rows <- data_with_imputed[data_with_imputed$DRUH == x[[1]], ]
    tibble::tibble(
      DRUH = x[[1]],
      range_cells = nrow(x[[2]]),
      cells_with_observed_records = dplyr::n_distinct(rows$row_n[!rows$IMPUTED], na.rm = TRUE),
      cells_with_imputed_absences = dplyr::n_distinct(rows$row_n[rows$IMPUTED]),
      imputed_absences = sum(rows$IMPUTED),
      observed_records_outside_range = sum(!rows$IMPUTED & is.na(rows$row_n))
    )
  }
))

report_table(
  imputation_by_cell,
  "Reporting-range cells of each species, with observed records and with imputed absences",
  "02_imputation_by_cell"
)

#--------------------------------------------------#
## Targeted records outside their species' own range -----
#--------------------------------------------------#

outside_range <-
  dplyr::bind_rows(
    targeted_records_outside_range(
      phengaris_lokal_new,
      targeted_records_in_range(phengaris_lokal_new, range_nausithous, SPECIES_NAU, target_mon_zdroj),
      SPECIES_NAU, target_mon_zdroj
    ),
    targeted_records_outside_range(
      phengaris_lokal_new,
      targeted_records_in_range(phengaris_lokal_new, range_teleius, SPECIES_TEL, target_mon_zdroj),
      SPECIES_TEL, target_mon_zdroj
    )
  ) %>%
  dplyr::count(DRUH, NEGATIV, name = "records") %>%
  dplyr::mutate(kept = isTRUE(settings$keep_outside_range))

report_table(
  outside_range,
  "Targeted records outside their species' own reporting range",
  "02_outside_range"
)

#--------------------------------------------------#
## The same records under every imputation rule -----
#--------------------------------------------------#

# How many absences each rule would impute, so the choice of rule can be
# judged from the numbers. Only the rule of this specification is used
# further.
rules_compared <- dplyr::bind_rows(lapply(c("legacy_cell", "visit", "none"), function(rule) {
  impute_absences(
    phengaris_lokal_new, range_nausithous, range_teleius,
    sources = target_mon_zdroj, rule = rule,
    keep_outside_range = settings$keep_outside_range
  ) %>%
    dplyr::group_by(DRUH) %>%
    dplyr::summarise(
      observed_present = sum(!IMPUTED & NEGATIV == 0),
      observed_absent = sum(!IMPUTED & NEGATIV == 1),
      imputed_absent = sum(IMPUTED),
      .groups = "drop"
    ) %>%
    dplyr::mutate(rule = rule, .before = 1)
}))

report_table(
  rules_compared,
  "Records by species under each imputation rule",
  "02_imputation_rules_compared"
)

#----------------------------------------------------------#
# Validate -----
#----------------------------------------------------------#

validate_table(
  data_with_imputed, "data_with_imputed",
  columns = c(
    ID_NALEZ = "numeric", DRUH = "character", ID_LOKAL = "numeric",
    NEGATIV = "numeric", IMPUTED = "logical", row_n = "numeric"
  ),
  # One row per record and species. An imputed absence shares the ID_NALEZ
  # of the record it was imputed from, so the key adds the species.
  key = "ID_RECORD",
  # Records kept from outside their species' range have no range cell.
  not_na = c(
    "ID_NALEZ", "DRUH", "ID_LOKAL", "NEGATIV", "IMPUTED",
    if (!isTRUE(settings$keep_outside_range)) "row_n"
  ),
  values = list(
    DRUH = c(SPECIES_NAU, SPECIES_TEL),
    NEGATIV = c(0, 1),
    ZDROJ = target_mon_zdroj
  ),
  xy = c("X", "Y")
)
checkmate::assert_true(
  all(data_with_imputed$NEGATIV[data_with_imputed$IMPUTED] == 1),
  .var.name = "every imputed record is an absence"
)
# No absence is ever imputed outside the imputed species' own reporting range:
# every imputed record has a range cell, and its site touches that species'
# range.
checkmate::assert_true(
  !anyNA(data_with_imputed$row_n[data_with_imputed$IMPUTED]),
  .var.name = "every imputed record lies in a reporting-range cell"
)
for (x in list(list(SPECIES_NAU, range_nausithous), list(SPECIES_TEL, range_teleius))) {
  imputed_ids <- data_with_imputed$ID_LOKAL[data_with_imputed$IMPUTED & data_with_imputed$DRUH == x[[1]]]
  imputed_sites <- phengaris_lokal_new %>%
    dplyr::filter(ID_LOKAL %in% imputed_ids) %>%
    dplyr::distinct(ID_LOKAL, .keep_all = TRUE)
  checkmate::assert_true(
    all(lengths(sf::st_intersects(imputed_sites, x[[2]])) > 0),
    .var.name = paste("every imputed", x[[1]], "absence lies inside its reporting range")
  )
}
checkmate::assert_true(
  all(is.na(data_with_imputed$POCET[data_with_imputed$IMPUTED])),
  .var.name = "no imputed absence carries a count"
)
# Never more observed records than there are targeted records.
checkmate::assert_true(
  dplyr::n_distinct(data_with_imputed$ID_NALEZ[!data_with_imputed$IMPUTED]) <=
    sum(phengaris_lokal_new$ZDROJ %in% target_mon_zdroj),
  .var.name = "observed records are a subset of the targeted records"
)

#----------------------------------------------------------#
# Export -----
#----------------------------------------------------------#

readr::write_csv(
  data_with_imputed,
  processed_path("data_with_imputed.csv")
)

report_finish()

message("  ", nrow(data_with_imputed), " records written")
step_end("02")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

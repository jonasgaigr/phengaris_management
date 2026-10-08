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

data_with_imputed <- impute_absences_legacy(
  phengaris_lokal_new,
  range_nausithous,
  range_teleius,
  sources = target_mon_zdroj
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
  "The imputation produced %d absence records out of %d records in total.",
  sum(data_with_imputed$IMPUTED), nrow(data_with_imputed)
))

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
  not_na = c("ID_NALEZ", "DRUH", "ID_LOKAL", "NEGATIV", "IMPUTED", "row_n"),
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

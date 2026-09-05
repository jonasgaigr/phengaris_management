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
# Reads:  objects from step 01 (phengaris_lokal_new, range_*, target_mon_zdroj)
# Writes: Data/Processed/data_with_imputed.csv
#
#----------------------------------------------------------#

message("Step 02: imputing absences")

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
# Targeted records inside each reporting range -----
#----------------------------------------------------------#

data_report_intersection_Pnau <-
  sf::st_intersection(
    phengaris_lokal_new %>%
      dplyr::filter(DRUH == SPECIES_NAU),
    range_nausithous
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(
    ZDROJ %in% target_mon_zdroj             # use only targeted monitoring
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    range_Pnau = dplyr::case_when(
      row_n %in% range_nausithous$row_n ~ 1,
      TRUE ~ 0
    )
  )

data_report_intersection_Ptel <-
  sf::st_intersection(
    phengaris_lokal_new %>%
      dplyr::filter(DRUH == SPECIES_TEL),
    range_teleius
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(
    ZDROJ %in% target_mon_zdroj             # use only targeted monitoring
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    range_Ptel = dplyr::case_when(
      row_n %in% range_teleius$row_n ~ 1,
      TRUE ~ 0
    )
  )

data_report_intersection <-
  dplyr::bind_rows(
    data_report_intersection_Pnau,
    data_report_intersection_Ptel
  ) %>%
  dplyr::mutate(
    range_both = sum(
      range_Pnau,
      range_Ptel,
      na.rm = TRUE
    )
  )

#--------------------------------------------------#
## Places already covered for each species -----
#--------------------------------------------------#

data_mon_Pnau <-
  data_report_intersection %>%
  dplyr::filter(
    DRUH == SPECIES_NAU
  )

data_mon_Ptel <-
  data_report_intersection %>%
  dplyr::filter(
    DRUH == SPECIES_TEL
  )

#----------------------------------------------------------#
# Imputed absences -----
#----------------------------------------------------------#
#--------------------------------------------------#
## P. nausithous absences from P. teleius surveys -----
#--------------------------------------------------#

imputed_pnau <-
  sf::st_intersection(
    # 1. Take ALL unfiltered P. teleius target records as proof of survey
    phengaris_lokal_new %>%
      dplyr::filter(DRUH == SPECIES_TEL, ZDROJ %in% target_mon_zdroj),
    # 2. Intersect with the target range (this also grabs range attributes)
    range_nausithous
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(
    # 3. Ensure no data for P. nausithous already exists here
    !row_n %in% data_mon_Pnau$row_n
  ) %>%
  dplyr::mutate(
    DRUH = SPECIES_NAU,
    NEGATIV = 1,
    IMPUTED = TRUE,
    # 4. Rebuild the range flags for consistency in the final bind
    range_Pnau = 1,
    range_Ptel = dplyr::case_when(row_n %in% range_teleius$row_n ~ 1, TRUE ~ 0),
    range_both = range_Pnau + range_Ptel
  ) %>%
  dplyr::select(
    ID_LOKAL, row_n, DRUH, NEGATIV, IMPUTED,
    range_Pnau, range_Ptel, range_both, dplyr::everything()
  )

#--------------------------------------------------#
## P. teleius absences from P. nausithous surveys -----
#--------------------------------------------------#

imputed_ptel <-
  sf::st_intersection(
    # 1. Take ALL unfiltered P. nausithous target records as proof of survey
    phengaris_lokal_new %>%
      dplyr::filter(DRUH == SPECIES_NAU, ZDROJ %in% target_mon_zdroj),
    # 2. Intersect with the target range
    range_teleius
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(
    # 3. Ensure no data for P. teleius already exists here
    !row_n %in% data_mon_Ptel$row_n
  ) %>%
  dplyr::mutate(
    DRUH = SPECIES_TEL,
    NEGATIV = 1,
    IMPUTED = TRUE,
    # 4. Rebuild the range flags for consistency in the final bind
    range_Ptel = 1,
    range_Pnau = dplyr::case_when(row_n %in% range_nausithous$row_n ~ 1, TRUE ~ 0),
    range_both = range_Pnau + range_Ptel
  ) %>%
  dplyr::select(
    ID_LOKAL, row_n, DRUH, NEGATIV, IMPUTED,
    range_Pnau, range_Ptel, range_both, dplyr::everything()
  )

#----------------------------------------------------------#
# Combine observed and imputed records -----
#----------------------------------------------------------#

data_with_imputed <-
  data_report_intersection %>%
  dplyr::mutate(
    IMPUTED = FALSE
  ) %>%
  dplyr::bind_rows(
    imputed_ptel,
    imputed_pnau
  )

#----------------------------------------------------------#
# Results -----
#----------------------------------------------------------#

imputation_summary <-
  data_with_imputed %>%
  dplyr::group_by(
    DRUH,
    NEGATIV,
    IMPUTED
  ) %>%
  dplyr::reframe(
    number = dplyr::n()
  )

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
# Export -----
#----------------------------------------------------------#

readr::write_csv(
  data_with_imputed,
  file.path(PATHS$processed, "data_with_imputed.csv")
)

report_finish()

message("Step 02 done: ", nrow(data_with_imputed), " records written")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

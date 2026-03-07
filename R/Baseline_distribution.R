#----------------------------------------------------------#
# REPORTING APPROACH -----
#----------------------------------------------------------#

#----------------------------------------------------------#
# Get targeted monitoring occurrence -----
#----------------------------------------------------------#

data_report_intersection_Pnau <-
  sf::st_intersection(
    phengaris_lokal_new %>%
      dplyr::filter(DRUH == "Phengaris nausithous"),
    range_nausithous
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(
    ZDROJ %in% target_mon_zdroj             # use only target monitoring efforts
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
      dplyr::filter(DRUH == "Phengaris teleius"),
    range_teleius
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(
    ZDROJ %in% target_mon_zdroj             # use only target monitoring efforts
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
## Get mapping fields with monitoring of P. nausithous -----
#--------------------------------------------------#

data_mon_Pnau <-
  data_report_intersection %>%
  dplyr::filter(
    DRUH == "Phengaris nausithous"
  )

#--------------------------------------------------#
## Get mapping fields with monitoring of P. teleius -----
#--------------------------------------------------#

data_mon_Ptel <-
  data_report_intersection %>%
  dplyr::filter(
    DRUH == "Phengaris teleius"
  )

#--------------------------------------------------#
## Impute P. nausithous (Logic Corrected) -----
#--------------------------------------------------#

imputed_pnau <- 
  sf::st_intersection(
    # 1. Take ALL unfiltered P. teleius target records as proof of survey
    phengaris_lokal_new %>% 
      dplyr::filter(DRUH == "Phengaris teleius", ZDROJ %in% target_mon_zdroj),
    # 2. Intersect with the target range (this also grabs range attributes)
    range_nausithous 
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(
    # 3. Ensure no data for P. nausithous already exists here
    !row_n %in% data_mon_Pnau$row_n     
  ) %>%
  dplyr::mutate(
    DRUH = "Phengaris nausithous",
    NEGATIV = 1,
    IMPUTED = TRUE,
    # 4. Rebuild the range flags for consistency in the final bind
    range_Pnau = 1,
    range_Ptel = dplyr::case_when(row_n %in% range_teleius$row_n ~ 1, TRUE ~ 0),
    range_both = range_Pnau + range_Ptel
  ) %>%
  dplyr::select(
    ID_LOKAL, row_n, DRUH, NEGATIV, IMPUTED, range_Pnau, range_Ptel, range_both, dplyr::everything()
  )

#--------------------------------------------------#
## Impute P. teleius (Logic Corrected) -----
#--------------------------------------------------#

imputed_ptel <- 
  sf::st_intersection(
    # 1. Take ALL unfiltered P. nausithous target records as proof of survey
    phengaris_lokal_new %>% 
      dplyr::filter(DRUH == "Phengaris nausithous", ZDROJ %in% target_mon_zdroj),
    # 2. Intersect with the target range
    range_teleius 
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(
    # 3. Ensure no data for P. teleius already exists here
    !row_n %in% data_mon_Ptel$row_n     
  ) %>%
  dplyr::mutate(
    DRUH = "Phengaris teleius",
    NEGATIV = 1,
    IMPUTED = TRUE,
    # 4. Rebuild the range flags for consistency in the final bind
    range_Ptel = 1,
    range_Pnau = dplyr::case_when(row_n %in% range_nausithous$row_n ~ 1, TRUE ~ 0),
    range_both = range_Pnau + range_Ptel
  ) %>%
  dplyr::select(
    ID_LOKAL, row_n, DRUH, NEGATIV, IMPUTED, range_Pnau, range_Ptel, range_both, dplyr::everything()
  )

#----------------------------------------------------------#
# Combine original data with imputed values -----
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

#--------------------------------------------------#
## Check original data with imputed values -----
#--------------------------------------------------#

# Check number of imputed records
table(data_with_imputed$IMPUTED)

data_with_imputed %>%
  dplyr::group_by(
    DRUH, 
    NEGATIV,
    IMPUTED
  ) %>%
  dplyr::reframe(
    number = dplyr::n()
  )

#----------------------------------------------------------#
# Export imputed data -----
#----------------------------------------------------------#

readr::write_csv(
  data_with_imputed,
  "Data/Processed/data_with_imputed.csv"
)

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#
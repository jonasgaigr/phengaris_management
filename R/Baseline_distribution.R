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
## Impute P. nausithous -----
#--------------------------------------------------#

imputed_pnau <- 
  data_report_intersection %>%
  dplyr::filter(
    range_Pnau == 1,                         # P. nausithous present in the past
    !row_n %in% data_mon_Pnau$row_n     # no recent *positive* record
  ) %>%
  dplyr::mutate(
    DRUH = "Phengaris nausithous",
    NEGATIV = 1,
    IMPUTED = TRUE
  ) %>%
  dplyr::select(
    ID_LOKAL, 
    row_n, 
    DRUH, 
    NEGATIV, 
    IMPUTED,
    dplyr::everything()
  )

#--------------------------------------------------#
## Impute P. teleius -----
#--------------------------------------------------#

imputed_ptel <- 
  data_mon_Pnau %>%
  dplyr::filter(
    range_Ptel == 1,                         # P. teleius present in the past
    !row_n %in% data_mon_Ptel$row_n     # no recent *positive* record
  ) %>%
  dplyr::mutate(
    DRUH = "Phengaris teleius",
    NEGATIV = 1,
    IMPUTED = TRUE
  ) %>%
  dplyr::select(
    ID_LOKAL, 
    row_n, 
    DRUH, 
    NEGATIV, 
    IMPUTED,
    dplyr::everything()
  )

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
  group_by(
    DRUH, 
    NEGATIV
  ) %>%
  reframe(
    number = n()
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
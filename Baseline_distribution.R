#----------------------------------------------------------#
# Set baseline (2012 - 2018) occurrence -----
#----------------------------------------------------------#
data_old_sitmap_intersection <-
  sf::st_intersection(
    phengaris_lokal_old,
    sitmap
  ) %>%
  sf::st_drop_geometry()

#----------------------------------------------------------#
# Get mapping fields with occurrence -----
#----------------------------------------------------------#
#--------------------------------------------------#
## Get mapping fields with occurrence of P. nausithous -----
#--------------------------------------------------#
data_old_sitmap_Pnau <-
  data_old_sitmap_intersection %>%
  dplyr::filter(
    DRUH == "Phengaris nausithous"
    ) %>%
  dplyr::group_by(POLE) %>%
  dplyr::arrange(
    desc(
      as.numeric(
        DATUM_DO
      )
    )
  ) %>%
  dplyr::slice(1) %>%
  dplyr::filter(
    NEGATIV == 0
  )

#--------------------------------------------------#
## Get mapping fields with occurrence of P. teleius -----
#--------------------------------------------------#
data_old_sitmap_Ptel <-
  data_old_sitmap_intersection %>%
  dplyr::filter(
    DRUH == "Phengaris teleius"
  ) %>%
  dplyr::group_by(POLE) %>%
  dplyr::arrange(
    desc(
      as.numeric(
        DATUM_DO
      )
    )
  ) %>%
  dplyr::slice(1) %>%
  dplyr::filter(
    NEGATIV == 0
  )

#----------------------------------------------------------#
# Set recent (2019 - 2019) occurrence -----
#----------------------------------------------------------#

data_new_sitmap_intersection <-
  sf::st_intersection(
    phengaris_lokal_new,
    sitmap
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(
    ZDROJ %in% target_mon_zdroj
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    old_Pnau = dplyr::case_when(
      POLE %in% data_old_sitmap_Pnau$POLE ~ 1,
      TRUE ~ 0
    ),
    old_Ptel = dplyr::case_when(
      POLE %in% data_old_sitmap_Ptel$POLE ~ 1,
      TRUE ~ 0
    ),
    old_both = sum(
      old_Pnau,
      old_Ptel,
      na.rm = TRUE
      )
  )

#----------------------------------------------------------#

#--------------------------------------------------#
## Get mapping fields with monitoring of P. nausithous -----
#--------------------------------------------------#

data_new_mon_Pnau <-
  data_new_sitmap_intersection %>%
  dplyr::filter(
    DRUH == "Phengaris nausithous"
  )

#--------------------------------------------------#
## Get mapping fields with monitoring of P. teleius -----
#--------------------------------------------------#

data_new_mon_Ptel <-
  data_new_sitmap_intersection %>%
  dplyr::filter(
    DRUH == "Phengaris teleius"
  )

#--------------------------------------------------#
## Get mapping fields with occurrence of P. nausithous -----
#--------------------------------------------------#

data_new_occ_Pnau <-
  data_new_sitmap_intersection %>%
  dplyr::filter(
    DRUH == "Phengaris nausithous"
  ) %>%
  dplyr::filter(
    NEGATIV == 0
  )

#--------------------------------------------------#
## Get mapping fields with occurrence of P. teleius -----
#--------------------------------------------------#

data_new_occ_Ptel <-
  data_new_sitmap_intersection %>%
  dplyr::filter(
    DRUH == "Phengaris teleius"
  ) %>%
  dplyr::filter(
    NEGATIV == 0
  )

#----------------------------------------------------------#
# Get recent (2019 - 2019) number of species per field -----
#----------------------------------------------------------#

data_new_sitmap_intersection %>%
  group_by(
    POLE
  ) %>%
  reframe(
    species_number = DRUH %>%
      unique() %>%
      length()
  )

#----------------------------------------------------------#
# Impute recent (2019 - 2019) negative records -----
#----------------------------------------------------------#

#--------------------------------------------------#
## Impute P. teleius -----
#--------------------------------------------------#

# Get POLEs where P. teleius was present historically
# but missing from recent monitoring, while P. nausithous was monitored
imputed_ptel <- 
  data_new_mon_Pnau %>%
  dplyr::filter(
    old_Ptel == 1,                         # P. teleius present in the past
    !POLE %in% data_new_mon_Ptel$POLE     # no recent *positive* record
  ) %>%
  dplyr::mutate(
    DRUH = "Phengaris teleius",
    NEGATIV = 1,
    IMPUTED = TRUE
  ) %>%
  dplyr::select(
    ID_LOKAL, 
    POLE, 
    DRUH, 
    NEGATIV, 
    IMPUTED,
    dplyr::everything()
  )

#--------------------------------------------------#
## Impute P. nausithous -----
#--------------------------------------------------#
imputed_pnau <- 
  data_new_mon_Ptel %>%
  dplyr::filter(
    old_Pnau == 1,                         # P. nausithous present in the past
    !POLE %in% data_new_mon_Pnau$POLE     # no recent *positive* record
  ) %>%
  dplyr::mutate(
    DRUH = "Phengaris nausithous",
    NEGATIV = 1,
    IMPUTED = TRUE
  ) %>%
  dplyr::select(
    ID_LOKAL, 
    POLE, 
    DRUH, 
    NEGATIV, 
    IMPUTED,
    dplyr::everything()
  )

#----------------------------------------------------------#
# Combine original data with imputed values -----
#----------------------------------------------------------#

data_new_with_imputed <- 
  data_new_sitmap_intersection %>%
  dplyr::mutate(
    IMPUTED = FALSE
    ) %>%
  dplyr::bind_rows(
    imputed_ptel, 
    imputed_pnau
    )

#----------------------------------------------------------#
# Export imputed data -----
#----------------------------------------------------------#

# Check number of imputed records
table(data_new_with_imputed$IMPUTED)

# Export
readr::write_csv(
  data_new_with_imputed,
  "Data/Processed/data_new_with_imputed.csv"
  )

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
  sf::st_drop_geometry()

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

#----------------------------------------------------------#
# Export imputed data -----
#----------------------------------------------------------#
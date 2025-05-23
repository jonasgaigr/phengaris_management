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
  dplyr::group_by(SITMAP) %>%
  dplyr::arrange(
    desc(
      DATUM_DO
    )
  ) %>%
  dplyr::slice(1) %>%
  dplyr::filter(
    NEGATIV == 0
  ) %>%
  pull(SITMAP)

#--------------------------------------------------#
## Get mapping fields with occurrence of P. teleius -----
#--------------------------------------------------#

#----------------------------------------------------------#
# Export imputed data -----
#----------------------------------------------------------#
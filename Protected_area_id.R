#----------------------------------------------------------#
# Identify occurrences within protected areas -----
#----------------------------------------------------------#
#--------------------------------------------------#
## Natura 2000 both species -----
#--------------------------------------------------#
phengaris_evl_id <- 
  phengaris_lokal_new %>%
  sf::st_intersection(
    ., 
    evl
    ) %>%
  sf::st_make_valid() %>%
  dplyr::pull(ID_LOKAL)

#--------------------------------------------------#
## Natura 2000 - P. nausithous target feature  -----
#--------------------------------------------------#
evl_id_Pnau <- 
  phengaris_lokal_new %>%
  sf::st_intersection(
    ., 
    evl %>%
      dplyr::filter(
        SITECODE %in% filter(
          sites_subjects, 
          nazev_lat == "Phengaris nausithous"
          )$site_code
        )
    ) %>%
  sf::st_make_valid() %>%
  dplyr::pull(ID_LOKAL)

#--------------------------------------------------#
## Natura 2000 - P. teleius target feature -----
#--------------------------------------------------#
evl_id_Ptel <- 
  phengaris_lokal_new %>%
  sf::st_intersection(
    ., 
    evl %>%
      dplyr::filter(
        SITECODE %in% filter(
          sites_subjects, 
          nazev_lat == "Phengaris teleius"
        )$site_code
      )
  ) %>%
  sf::st_make_valid() %>%
  dplyr::pull(ID_LOKAL)

#--------------------------------------------------#
## National small scale protected areas -----
#--------------------------------------------------#
phengaris_mzchu_id <- 
  phengaris_lokal_new %>%
  sf::st_intersection(
    ., 
    mzchu
    ) %>%
  sf::st_make_valid() %>%
  dplyr::pull(ID_LOKAL)




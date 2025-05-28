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
sf::st_drop_geometry() %>%
  dplyr::mutate(
    PA_TYPE = "EVL_both",
    TARGET_GROUP = DRUH
  ) %>%
  dplyr::select(
    ID_LOKAL,
    DRUH,
    PA_TYPE
  )

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
  sf::st_drop_geometry() %>%
  dplyr::mutate(
    PA_TYPE = "EVL_target",
    TARGET_GROUP = DRUH
  ) %>%
  dplyr::select(
    ID_LOKAL,
    DRUH,
    PA_TYPE
  )

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
  sf::st_make_valid()  %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(
    PA_TYPE = "EVL_target",
    TARGET_GROUP = DRUH
  ) %>%
  dplyr::select(
    ID_LOKAL,
    DRUH,
    PA_TYPE
    )

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
  sf::st_drop_geometry() %>%
  dplyr::mutate(
    PA_TYPE = "MZCHU",
    TARGET_GROUP = DRUH
  ) %>%
  dplyr::select(
    ID_LOKAL,
    DRUH,
    PA_TYPE
  )

#----------------------------------------------------------#
# Export data -----
#----------------------------------------------------------#

protected_area_id <-
  bind_rows(
    evl_id_Pnau,
    evl_id_Ptel,
    phengaris_mzchu_id
  )

readr::write_csv(
  protected_area_id,
  "Data/Processed/protected_area_id.csv"
)

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

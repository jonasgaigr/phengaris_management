
phengaris_evl_id <- phengaris_lokal %>%
  sf::st_intersection(., evl) %>%
  sf::st_make_valid() %>%
  dplyr::pull(ID_LOKAL)

phenau_evl_id <- phengaris_lokal %>%
  sf::st_intersection(., evl %>% 
                        dplyr::filter(SITECODE %in% filter(sites_subjects, 
                                                           nazev_lat == "Phengaris nausithous")$site_code)) %>%
  sf::st_make_valid() %>%
  dplyr::pull(ID_LOKAL)

phetel_evl_id <- phengaris_lokal %>%
  sf::st_intersection(., evl %>% 
                        dplyr::filter(SITECODE %in% filter(sites_subjects, 
                                                           nazev_lat == "Phengaris teleius")$site_code)) %>%
  sf::st_make_valid() %>%
  dplyr::pull(ID_LOKAL)

phengaris_mzchu_id <- phengaris_lokal %>%
  sf::st_intersection(., mzchu) %>%
  sf::st_make_valid() %>%
  dplyr::pull(ID_LOKAL)
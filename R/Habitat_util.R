# INTERSECT ----
# VMB
vmb_shp_sjtsk_22_read <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/20220531_Segment.shp")
#vmb_hab_dbf_22 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/Biotop/HAB_BIOTOP.dbf")
#vmb_pb_dbf_22 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/Biotop/PB_BIOTOP.dbf")
vmb_dbf_22 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/Biotop/20220531_BIOTOP.dbf")

vmb_shp_sjtsk_22 <- vmb_shp_sjtsk_22_read %>%
  dplyr::left_join(vmb_dbf_22, by = "SEGMENT_ID") %>%
  dplyr::group_by(SEGMENT_ID) %>%
  dplyr::mutate(moz_num = n(),
                FSB_EVAL_prep = dplyr::case_when(sum(STEJ_PR, na.rm = TRUE) < 50 ~ "X",
                                                 sum(STEJ_PR, na.rm = TRUE) >= 50 &
                                                   sum(STEJ_PR, na.rm = TRUE) < 200 ~ "moz.",
                                                 sum(STEJ_PR, na.rm = TRUE) == 200 ~ NA_character_)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(FSB_EVAL = dplyr::case_when(FSB_EVAL_prep == "moz." ~ "moz.",
                                            FSB_EVAL_prep == "X" ~ "X",
                                            TRUE ~ FSB),
                #HABITAT = dplyr::case_when(HABITAT == 6210 & HABIT_TYP == "p" ~ "6210p",
                #                           TRUE ~ HABITAT)
  )

rm(vmb_shp_sjtsk_22_read, vmb_dbf_22)

lokal_vmb <- lokal %>%
  sf::st_intersection(., vmb_shp_sjtsk_22) %>%
  dplyr::mutate(AREA_real = units::drop_units(sf::st_area(geometry)))

st_write(lokal_vmb,
         "lokal_vmb_2.gpkg")

# OBSERVED HABITATS 
data %>%
  mutate(
    AREA_SITE = STEJ_PR/100*SHAPE_AREA,
    HET_OUT = stringr::str_count(
      BIOTOP_SEZ, 
      "\\("
    )
  )

phengaris %>%
  dplyr::filter(is.na(FSB)) %>%
  plot()

phengaris <- 
  phengaris_edit %>%
  dplyr::mutate(
    NATURAL = dplyr::case_when(
      BIOTOP != "-1" &
        grepl("X", BIOTOP) == FALSE &
        is.na(BIOTOP) == FALSE ~ 1,
      TRUE ~ 0
    )
  ) %>%
  dplyr::group_by(
    ID_NALEZ
  ) %>%
  dplyr::arrange(
    NATURAL, 
    AREA_SITE
  ) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

sumob <- phengaris %>%
  st_drop_geometry() %>%
  filter(POSITIVE == 1) %>%
  group_by(TTP, ZARUST, PRIKOP, JINY) %>%
  summarise(n())

sumob_phenau <- phengaris %>%
  st_drop_geometry() %>%
  dplyr::select(DRUH, POSITIVE, TTP, ZARUST, PRIKOP, JINY) %>%
  pivot_longer(.,
               cols = c(3:6)) %>%
  filter(value == 1) %>%
  dplyr::select(-value) %>%
  filter(POSITIVE == 1) %>%
  filter(DRUH == "Phengaris nausithous") %>%
  group_by(name) %>%
  summarise(COUNT = n(),
            PERC = n()/nrow(phengaris %>%
                              dplyr::filter(DRUH == "Phengaris nausithous") %>%
                              dplyr::filter(POSITIVE == 1))*100) %>%
  mutate(name = case_when(name == "TTP" ~ "managed\ngrassland",
                          name == "ZARUST" ~ "neglected\ngrassland",
                          name == "JINY" ~ "other",
                          name == "PRIKOP" ~ "road verge,\nditch"))

sumob_phetel <- phengaris %>%
  st_drop_geometry() %>%
  dplyr::select(DRUH, POSITIVE, TTP, ZARUST, PRIKOP, JINY) %>%
  pivot_longer(.,
               cols = c(3:6)) %>%
  filter(value == 1) %>%
  dplyr::select(-value) %>%
  filter(POSITIVE == 1) %>%
  filter(DRUH == "Phengaris teleius") %>%
  group_by(name) %>%
  summarise(COUNT = n(),
            PERC = n()/nrow(phengaris %>%
                              dplyr::filter(DRUH == "Phengaris teleius") %>%
                              dplyr::filter(POSITIVE == 1))*100) %>%
  mutate(name = case_when(name == "TTP" ~ "managed\ngrassland",
                          name == "ZARUST" ~ "neglected\ngrassland",
                          name == "JINY" ~ "other",
                          name == "PRIKOP" ~ "road verge,\nditch"))


ggplot(data = sumob_phenau, 
       aes(x = fct_reorder(name, COUNT, .desc = TRUE), 
           y = COUNT)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  xlab("\nrecorded habitats preferred by P. nausithous") +
  ylab("number of sites\n") +
  theme_classic() +
  theme(text = element_text(size = 20))

ggplot(data = sumob_phetel, 
       aes(x = fct_reorder(name, COUNT, .desc = TRUE), 
           y = COUNT)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  xlab("\nrecorded habitats preferred by P. teleius") +
  ylab("number of sites\n") +
  theme_classic() +
  theme(text = element_text(size = 20))


sumlv <- phengaris %>%
  st_drop_geometry() %>%
  dplyr::filter(POSITIVE == 1) %>%
  group_by(BIOTOP, FSB) %>%
  summarise(AREA =  sum(AREA_SITE)/10000,
            mean_area = mean(AREA_SITE),
            median_area = median(AREA_SITE),
            COUNT = n()) %>%
  distinct() %>%
  arrange(-AREA) %>%
  ungroup()

sumlv_nau <- phengaris %>%
  dplyr::filter(DRUH == "Phengaris nausithous") %>%
  dplyr::filter(POSITIVE == 1) %>%
  st_drop_geometry() %>%
  dplyr::mutate(FSB = dplyr::case_when(is.na(FSB) == TRUE ~ "-",
                                       TRUE ~ FSB),
                BIOTOP = dplyr::case_when(is.na(BIOTOP) == TRUE ~ "-1",
                                          TRUE ~ BIOTOP)) %>%
  group_by(BIOTOP) %>%
  summarise(AREA =  sum(AREA_SITE, na.rm = TRUE)/10000,
            mean_area = mean(AREA_SITE, na.rm = TRUE),
            median_area = median(AREA_SITE, na.rm = TRUE),
            COUNT = n()) %>%
  distinct() %>%
  arrange(-COUNT) %>%
  ungroup()

sumlv_tel <- phengaris %>%
  dplyr::filter(DRUH == "Phengaris teleius") %>%
  dplyr::filter(POSITIVE == 1) %>%
  st_drop_geometry() %>%
  dplyr::mutate(FSB = dplyr::case_when(is.na(FSB) == TRUE ~ "-",
                                       TRUE ~ FSB),
                BIOTOP = dplyr::case_when(is.na(BIOTOP) == TRUE ~ "-1",
                                          TRUE ~ BIOTOP)) %>%
  group_by(BIOTOP) %>%
  summarise(AREA =  sum(AREA_SITE, na.rm = TRUE)/10000,
            mean_area = mean(AREA_SITE, na.rm = TRUE),
            median_area = median(AREA_SITE, na.rm = TRUE),
            COUNT = n()) %>%
  distinct() %>%
  arrange(-COUNT) %>%
  ungroup()

sumlv_tel %>% slice(1:10)

#logaritmovat nebo na body

ggplot(data = sumlv %>% slice(1:7) %>%
         mutate(BIOTOP = fct_reorder(BIOTOP, AREA, .desc = TRUE)), 
       aes(x = fct_reorder(BIOTOP, AREA, .desc = TRUE), 
           y = log(AREA))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  xlab("") +
  ylab("habitat area sum (ha)\n") +
  theme_classic() +
  theme(text = element_text(size = 40))

ggplot(data = sumlv_nau %>% slice(1:10) %>%
         mutate(BIOTOP = fct_reorder(BIOTOP, AREA, .desc = TRUE)), 
       aes(x = fct_reorder(BIOTOP, AREA, .desc = TRUE), 
           y = log(AREA))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  xlab("\ncatalogued habitats preferred by P. nausithous") +
  ylab("habitat area sum (ha)\n") +
  theme_classic() +
  theme(text = element_text(size = 20))

ggplot(data = sumlv_tel %>% slice(1:10) %>%
         mutate(BIOTOP = fct_reorder(BIOTOP, AREA, .desc = TRUE)), 
       aes(x = fct_reorder(BIOTOP, AREA, .desc = TRUE), 
           y = log(AREA))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  xlab(expression(paste("\ncatalogued habitats preferred by ", italic("P. teleius")))) +
  ylab("habitat area sum (ha)\n") +
  theme_classic() +
  theme(text = element_text(size = 20))

ggplot(data = sumlv_nau %>% slice(1:10) %>%
         arrange(-COUNT) %>%
         mutate(order = row_number()), 
       aes(x = fct_reorder(BIOTOP, order, .desc = FALSE), 
           y = COUNT)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  xlab("\ncatalogued habitats preferred by P. nausithous") +
  ylab("number of sites\n") +
  theme_classic() +
  theme(text = element_text(size = 20))

ggplot(data = sumlv_tel %>% slice(1:10) %>%
         arrange(-COUNT) %>%
         mutate(order = row_number()), 
       aes(x = fct_reorder(BIOTOP, order), 
           y = COUNT)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  xlab("\ncatalogued habitats preferred by P. teleius") +
  ylab("number of sites\n") +
  theme_classic() +
  theme(text = element_text(size = 20))
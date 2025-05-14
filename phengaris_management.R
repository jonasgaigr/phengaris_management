# SET WD ----
setwd("~/phengaris")

# LOAD PACKAGES ----
if(!isTRUE(require(tidyverse, quietly = TRUE))) {
  install.packages("tidyverse", dependencies = TRUE); library(tidyverse)
} else {
  require(tidyverse)}

if(!isTRUE(require(sf, quietly = TRUE))) {
  install.packages("sf", dependencies = TRUE); library(sf)
} else {
  require(sf)}

if(!isTRUE(require(sp, quietly = TRUE))) {
  install.packages("sp", dependencies = TRUE); library(sp)
} else {
  require(sp)}

if(!isTRUE(require(proj4, quietly = TRUE))) {
  install.packages("proj4", dependencies = TRUE); library(proj4)
} else {
  require(proj4)}

if(!isTRUE(require(leaflet, quietly = TRUE))) {
  install.packages("leaflet", dependencies = TRUE); library(leaflet)
} else {
  require(leaflet)}

if(!isTRUE(require(openxlsx, quietly = TRUE))) {
  install.packages("openxlsx", dependencies = TRUE); library(openxlsx)
} else {
  require(openxlsx)}

if(!isTRUE(require(lme4, quietly = TRUE))) {
  install.packages("lme4", dependencies = TRUE); library(lme4)
} else {
  require(lme4)}
citation()
if(!isTRUE(require(lmerTest, quietly = TRUE))) {
  install.packages("lmerTest", dependencies = TRUE); library(lmerTest)
} else {
  require(lmerTest)}

if(!isTRUE(require(vegan, quietly = TRUE))) {
  install.packages("vegan", dependencies = TRUE); library(vegan)
} else {
  require(vegan)}

if(!isTRUE(require(GLMMadaptive, quietly = TRUE))) {
  install.packages("GLMMadaptive", dependencies = TRUE); library(GLMMadaptive)
} else {
  require(GLMMadaptive)}


# LOAD DATA ----
czechia <- sf::st_read("HraniceCR.shp") %>%
  sf::st_transform(., CRS("+init=epsg:5514"))
evl <- sf::st_read("Evropsky_v%C3%BDznamn%C3%A9_lokality.shp") %>% 
  sf::st_transform(., CRS("+init=epsg:5514"))
mzchu <- sf::st_read("mzchu.shp") %>% 
  sf::st_transform(., CRS("+init=epsg:5514"))
sitmap <- sf::st_read("sitmap_1rad.shp") %>%
  sf::st_transform(., CRS("+init=epsg:5514"))


rn2kcz::load_n2k_sites()
sites_subjects <- openxlsx::read.xlsx("C:/Users/jonas.gaigr/N2K.CZ/seznam_predmetolokalit_Natura2000_440_2021.xlsx")
colnames(sites_subjects) <- c("site_code", "site_name", "site_type", "feature_type", "feature_code", "nazev_cz", "nazev_lat")
phengaris_data <- read.csv2("phengaris_23.csv",
                            stringsAsFactors = FALSE,
                            fileEncoding = "Windows-1250")
lokal_b <- sf::st_read("w03_nd_lokalizace_b.shp") %>% 
  sf::st_transform(., CRS("+init=epsg:5514")) %>%
  sf::st_cast("POINT") %>%
  sf::st_make_valid()
lokal_p1 <- sf::st_read("w03_nd_lokalizace_p1.shp") %>% 
  sf::st_transform(., CRS("+init=epsg:5514")) %>%
  sf::st_cast("POLYGON") %>%
  sf::st_make_valid()
lokal_p <- sf::st_read("w03_nd_lokalizace_p.shp") %>% 
  sf::st_transform(., CRS("+init=epsg:5514")) %>%
  sf::st_cast("POLYGON") %>%
  sf::st_make_valid() %>%
  dplyr::bind_rows(., lokal_p1) %>%
  distinct()
lokal_l  <- sf::st_read("w03_nd_lokalizace_l.shp") %>% 
  sf::st_transform(., CRS("+init=epsg:5514")) %>%
  sf::st_cast("LINESTRING") %>%
  sf::st_make_valid()

lokal <- dplyr::bind_rows(lokal_b, lokal_p, lokal_l) %>%
  dplyr::rename(ID_LOKAL = idx_nd_lok) %>%
  sf::st_as_sf() %>%
  sf::st_make_valid()

lokal_vmb <- sf::st_read("lokal_vmb_2.gpkg") %>%
  #sf::st_drop_geometry() %>%
  dplyr::select(ID_LOKAL, FSB, BIOTOP_SEZ, SHAPE_AREA, BIOTOP, 
                STEJ_PR, KVALITA, AREA_real)

phengaris_lokal <- phengaris_data %>%
  dplyr::full_join(., lokal) %>%
  dplyr::full_join(., lokal_vmb) %>%
  sf::st_as_sf() %>%
  sf::st_make_valid()

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


target_mon_zdroj <- c("Kolektiv autorů (2017) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
                      "Kolektiv autorů (2018) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
                      "Kolektiv autorů (2019) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
                      "Kolektiv autorů (2020) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
                      "Kolektiv autorů (2021) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
                      "Kolektiv autorů (2020) Monitoring motýlů. Monitoring druhů ČR. AOPK ČR.",
                      "Kolektiv autorů (2021) Monitoring motýlů. Monitoring druhů ČR. AOPK ČR.",
                      "Kolektiv autorů (2022) Monitoring motýlů.")

phengaris_both <- phengaris_lokal %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(POSITIVE = dplyr::case_when(NEGATIV == 1 ~ 0,
                                            NEGATIV == 0 ~ 1)) %>%
  dplyr::filter(POSITIVE == 1) %>%
  dplyr::group_by(NAZ_LOKAL, DATUM_OD) %>%
  summarise(ID_NALEZ = unique(ID_NALEZ),
            SPEC_NUM = length(unique(DRUH))) %>%
  dplyr::filter(SPEC_NUM == 2) %>%
  dplyr::pull(ID_NALEZ)

phengaris_edit <- phengaris_lokal %>%
  dplyr::filter(ZDROJ %in% target_mon_zdroj) %>%
  dplyr::mutate(POSITIVE = dplyr::case_when(NEGATIV == 1 ~ 0,
                                            NEGATIV == 0 ~ 1),
                YEAR = as.factor(substr(DATUM_OD, 1, 4)),
                EVL = dplyr::case_when(ID_LOKAL %in% phengaris_evl_id ~ 1,
                                       TRUE ~ 0),
                EVL_target = dplyr::case_when(DRUH == "Phengaris nausithous" &
                                                ID_LOKAL %in% phenau_evl_id ~ 1,
                                              DRUH == "Phengaris teleius" &
                                                ID_LOKAL %in% phetel_evl_id ~ 1,
                                              TRUE ~ 0),
                EVL_comb = dplyr::case_when(EVL_target == 1 ~ 1,
                                           EVL == 1 & EVL_target == 0 ~ 0.5,
                                           TRUE ~ 0),
                MZCHU = dplyr::case_when(ID_LOKAL %in% phengaris_mzchu_id ~ 1,
                                         TRUE ~ 0),
                PROTECT = dplyr::case_when(EVL == 1 | MZCHU == 1 ~ 1,
                                           TRUE ~ 0),
                PLANT_QUANT = dplyr::case_when(grepl("dominantně", STRUKT_POZN) ~ 3/3,
                                               grepl("hojně", STRUKT_POZN) ~ 2/3,
                                               grepl("jednotlivě", STRUKT_POZN) ~ 1/3,
                                               grepl("žádné", STRUKT_POZN) ~ 0),
                PLANT_QUAL = dplyr::case_when(grepl("dominantně", STRUKT_POZN) ~ 1,
                                              grepl("hojně", STRUKT_POZN) ~ 1,
                                              grepl("jednotlivě", STRUKT_POZN) ~ 1,
                                              grepl("žádné", STRUKT_POZN) ~ 0),
                TTP = dplyr::case_when(grepl("TTP s pravidelným managementem", POP_BIOT) ~ 1,
                                       is.na(POP_BIOT) == FALSE ~ 0),
                PRIKOP = dplyr::case_when(grepl("příkop u komunikace", POP_BIOT) ~ 1,
                                          is.na(POP_BIOT) == FALSE ~ 0),
                ZARUST = dplyr::case_when(grepl("zarůstající louka bez managementu", POP_BIOT) ~ 1,
                                          is.na(POP_BIOT) == FALSE ~ 0),
                JINY = dplyr::case_when(grepl("jiný", POP_BIOT) ~ 1,
                                        is.na(POP_BIOT) == FALSE ~ 0),
                LandUseChange = dplyr::case_when(grepl("změna zemědělského využívání půdy a terénní úpravy", STRUKT_POZN) ~ 1,
                                                 TRUE ~ 0),
                Abandonment = dplyr::case_when(grepl("absence či nedostatek péče", STRUKT_POZN) ~ 1,
                                               TRUE ~ 0),
                HarmfulMow = dplyr::case_when(grepl("nevhodná seč", STRUKT_POZN) ~ 1,
                                              TRUE ~ 0),
                HarmfulGrazing = dplyr::case_when(grepl("nevhodná pastva", STRUKT_POZN) ~ 1,
                                                    TRUE ~ 0),
                GrazingByeffects = dplyr::case_when(grepl("dopady chovu dobytka mimo pastvu", STRUKT_POZN) ~ 1,
                                                  TRUE ~ 0),
                FertilizerUse = dplyr::case_when(grepl("aplikace hnojiv", STRUKT_POZN) ~ 1,
                                                 TRUE ~ 0),
                Afforestation = dplyr::case_when(grepl("zalesňování bezlesí", STRUKT_POZN) ~ 1,
                                                 TRUE ~ 0),
                Invasives = dplyr::case_when(grepl("invazní druhy", STRUKT_POZN) ~ 1,
                                             TRUE ~ 0),
                NativeDominants = dplyr::case_when(grepl("expanzní druhy", STRUKT_POZN) ~ 1,
                                             TRUE ~ 0),
                AbioticNaturalProcesses = dplyr::case_when(grepl("abiotické přírodní procesy", STRUKT_POZN) ~ 1,
                                                TRUE ~ 0),
                Encroachment = dplyr::case_when(grepl("sukcese", STRUKT_POZN) ~ 1,
                                                TRUE ~ 0),
                BiomassAccumulation = dplyr::case_when(grepl("hromadění organického materiálu", STRUKT_POZN) ~ 1,
                                                       TRUE ~ 0),
                Eutrophization = dplyr::case_when(grepl("eutrofizace či okyselování", STRUKT_POZN) ~ 1,
                                                  TRUE ~ 0),
                None = dplyr::case_when(grepl("žádné", STRUKT_POZN) ~ 1,
                                        TRUE ~ 0),
                METHOD = dplyr::case_when(grepl("Seč celoplošná: ne", STRUKT_POZN) ~ 1,
                                          grepl("Seč celoplošná: ano", STRUKT_POZN) ~ 0),
                TIMING = dplyr::case_when(grepl("Seč vhodně načasovaná: ano", STRUKT_POZN) ~ 1,
                                          grepl("Seč vhodně načasovaná: ne", STRUKT_POZN) ~ 0),
                MOW = dplyr::case_when(grepl("Seč", STRUKT_POZN) ~ 1,
                                       TRUE ~ 0),
                GRAZE = dplyr::case_when(grepl("Pastva:", STRUKT_POZN) ~ 1,
                                         TRUE ~ 0),
                GRAZE_MET = dplyr::case_when(grepl("Pastva: extenzivní", STRUKT_POZN) ~ 1,
                                             grepl("Pastva: intenzivní", STRUKT_POZN) ~ 0),
                MANAGEMENT_HET = dplyr::case_when(is.na(METHOD) == FALSE & 
                                                    is.na(GRAZE) == FALSE ~ 1,
                                                  TRUE ~ 0),
                OVERALL = dplyr::case_when(METHOD == 1 & TIMING == 1 ~ 1,
                                           TRUE ~ 0),
                SPEC_NUM = dplyr::case_when(ID_NALEZ %in% phengaris_both ~ 1,
                                            TRUE ~ 0),
                SITMAP = as.factor(SITMAP),
                AREA_SITE = STEJ_PR/100*SHAPE_AREA
                ) %>%
  dplyr::rowwise() %>%
  mutate(SUM_THREATS = sum(LandUseChange, Abandonment, HarmfulMow, HarmfulGrazing,
                           GrazingByeffects, FertilizerUse, Afforestation, Invasives,
                           NativeDominants, AbioticNaturalProcesses, Encroachment,
                           BiomassAccumulation, Eutrophization, None, na.rm = TRUE),
         HET_INN = sum(TTP, PRIKOP, ZARUST, JINY, na.rm = TRUE),
         HET_OUT = stringr::str_count(BIOTOP_SEZ, "\\(")) %>%
  dplyr::filter(is.na(PLANT_QUAL) == FALSE & 
                  PLANT_QUAL != 0 & 
                  #(is.na(METHOD) == FALSE | is.na(GRAZE)) &
                  !YEAR %in% c("2014", "2017"))

phengaris <- phengaris_edit %>%
  dplyr::mutate(NATURAL = dplyr::case_when(BIOTOP != "-1" &
                                             grepl("X", BIOTOP) == FALSE & 
                                             is.na(BIOTOP) == FALSE ~ 1,
                                           TRUE ~ 0)) %>%
  dplyr::group_by(ID_NALEZ) %>%
  dplyr::arrange(NATURAL, AREA_SITE) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

phengaris %>%
  dplyr::select(BIOTOP_SEZ, HET_OUT) %>%
  arrange(-HET_OUT)

phengaris %>%
  dplyr::filter(is.na(FSB)) %>%
  plot()
  
phengaris %>% 
  dplyr::filter(DRUH == "Phengaris teleius") %>%
  st_drop_geometry() %>%
  dplyr::mutate(POSITIVE = dplyr::case_when(NEGATIV == 1 ~ 0,
                                            NEGATIV == 0 ~ 1),
                YEAR = as.factor(substr(DATUM_OD, 1, 4))) %>%
  group_by(YEAR) %>%
  summarise(n(),
            mean_positive = mean(POSITIVE))


nrow(phengaris)
nrow(phengaris %>%
       filter(AREA_SITE > 0))
nrow(phengaris %>%
       filter(is.na(PROTECT) == FALSE))
     
sum(st_geometry_type(phengaris) == "POLYGON")

phengaris$AUTOR %>% 
  unique() %>%
  length()

phengaris %>%
  st_drop_geometry() %>%
  group_by(YEAR) %>%
  summarise(length(unique(SITMAP)))

phengaris %>%
  st_drop_geometry() %>%
  dplyr::filter(DRUH == "Phengaris nausithous") %>%
  group_by(YEAR) %>%
  summarise(mean(POSITIVE))

phengaris %>%
  st_drop_geometry() %>%
  dplyr::filter(DRUH == "Phengaris teleius") %>%
  group_by(YEAR) %>%
  summarise(mean(POSITIVE))


# SUMMARY
nrow(phengaris)

nrow(phengaris %>%
       filter(DRUH == "Phengaris nausithous"))
nrow(phengaris %>%
       filter(DRUH == "Phengaris nausithous" & POSITIVE == 1))
nrow(phengaris %>%
       filter(DRUH == "Phengaris nausithous" & POSITIVE == 0))
nrow(phengaris %>% 
       filter(DRUH == "Phengaris nausithous" & POSITIVE == 1))/
nrow(phengaris %>%
       filter(DRUH == "Phengaris nausithous"))

nrow(phengaris %>%
       filter(DRUH == "Phengaris teleius"))
nrow(phengaris %>%
       filter(DRUH == "Phengaris teleius" & POSITIVE == 1))
nrow(phengaris %>%
       filter(DRUH == "Phengaris teleius" & POSITIVE == 0))
nrow(phengaris %>%
       filter(DRUH == "Phengaris teleius" & POSITIVE == 1))/
nrow(phengaris %>%
       filter(DRUH == "Phengaris teleius"))

# BIOTOPY
nrow(phengaris %>%
       filter(DRUH == "Phengaris nausithous" & 
                POSITIVE == 1 &
                TTP == 1))
nrow(phengaris %>%
       filter(DRUH == "Phengaris nausithous" & 
                POSITIVE == 1 &
                ZARUST == 1))
nrow(phengaris %>%
       filter(DRUH == "Phengaris nausithous" & 
                POSITIVE == 1 &
                PRIKOP == 1))
nrow(phengaris %>%
       filter(DRUH == "Phengaris nausithous" & 
                POSITIVE == 1 &
                JINY == 1))

nrow(phengaris %>%
       filter(DRUH == "Phengaris teleius" & 
                POSITIVE == 1 &
                TTP == 1))
nrow(phengaris %>%
       filter(DRUH == "Phengaris teleius" & 
                POSITIVE == 1 &
                ZARUST == 1))
nrow(phengaris %>%
       filter(DRUH == "Phengaris teleius" & 
                POSITIVE == 1 &
                PRIKOP == 1))
phengaris %>%
  filter(DRUH == "Phengaris teleius" & 
           POSITIVE == 1 &
           JINY == 1) %>%
  nrow()

phengaris %>%
  filter(DRUH == "Phengaris nausithous") %>%
  pull(POCET) %>%
  na.omit() %>%
  median()
length(phengaris %>%
       filter(DRUH == "Phengaris nausithous") %>%
       pull(POCET) %>%
       na.omit())
phengaris %>%
  filter(DRUH == "Phengaris teleius") %>%
  pull(POCET) %>%
  na.omit() %>%
  median()
length(phengaris %>%
         filter(DRUH == "Phengaris teleius") %>%
         pull(POCET) %>%
         na.omit())
  
phengaris %>%
  st_drop_geometry() %>% 
  group_by(YEAR, POSITIVE) %>%
  summarise(pocet = n())

phengaris %>%
  st_drop_geometry() %>%
  group_by(PLANT_QUAL, DRUH, POSITIVE) %>%
  summarise(COUNT = n())

phengaris_evl_sum <- phengaris %>%
  st_drop_geometry() %>%
  group_by(EVL, POSITIVE, DRUH) %>%
  summarise(COUNT = n()) %>%
  ungroup()
phengaris_evltar_sum <- phengaris %>%
  st_drop_geometry() %>%
  group_by(EVL_target, POSITIVE, DRUH) %>%
  summarise(COUNT = n()) %>%
  ungroup()
phengaris_evlcomb_sum <- phengaris %>%
  st_drop_geometry() %>%
  group_by(EVL_comb, POSITIVE, DRUH) %>%
  summarise(COUNT = n()) %>%
  ungroup()
phengaris_mzchu_sum <- phengaris %>%
  st_drop_geometry() %>%
  group_by(MZCHU, POSITIVE, DRUH) %>%
  summarise(COUNT = n()) %>%
  ungroup()
phengaris_plant_sum <- phengaris %>%
  st_drop_geometry() %>%
  group_by(POSITIVE, DRUH, PLANT_QUANT) %>%
  summarise(COUNT = n()) %>%
  ungroup()
phengaris_method_sum <- phengaris %>%
  st_drop_geometry() %>%
  group_by(POSITIVE, DRUH, METHOD) %>%
  summarise(COUNT = n()) %>%
  ungroup()
phengaris_time_sum <- phengaris %>%
  st_drop_geometry() %>%
  group_by(POSITIVE, DRUH, TIMING) %>%
  summarise(COUNT = n()) %>%
  ungroup()
phengaris_man_sum <- phengaris %>%
  st_drop_geometry() %>%
  mutate(MANAGEMENT = case_when(TIMING == 1 & METHOD == 1 ~ "appropriate mow and appropriate timing",
                                TIMING == 0 & METHOD == 1 ~ "appropriate mow only",
                                TIMING == 1 & METHOD == 0 ~ "appropriate timing only",
                                TIMING == 0 & METHOD == 0 ~ "inappropriate mow and inappropriate timing")) %>%
  group_by(POSITIVE, DRUH, MANAGEMENT) %>%
  summarise(COUNT = n()) %>%
  ungroup()
phengaris_spe_sum <- phengaris %>%
  st_drop_geometry() %>%
  group_by(POSITIVE, DRUH, SPEC_NUM) %>%
  summarise(COUNT = n()) %>%
  ungroup()
phengaris_sum <- phengaris %>%
  st_drop_geometry() %>%
  group_by(DRUH, POSITIVE) %>%
  summarise(COUNT = n()) %>%
  ungroup()
sum(phengaris_sum$COUNT)
sum(phengaris_evl_sum$COUNT)

phengaris_sum %>%
  filter(DRUH == "Phengaris teleius")

phengaris %>%
  dplyr::filter(DRUH == "Phengaris nausithous")  %>%
  pull(POCET) %>%
  na.omit() %>%  
  mean()
phengaris %>%
  dplyr::filter(DRUH == "Phengaris nausithous")  %>%
  pull(POCET) %>%
  na.omit() %>%  
  median()
phengaris %>%
  dplyr::filter(DRUH == "Phengaris nausithous")  %>%
  pull(POCET) %>%
  na.omit() %>%  
  sd()

phengaris %>%
  dplyr::filter(DRUH == "Phengaris teleius")  %>%
  pull(POCET) %>%
  na.omit() %>%  
  mean()
phengaris %>%
  dplyr::filter(DRUH == "Phengaris teleius")  %>%
  pull(POCET) %>%
  na.omit() %>%  
  median()
phengaris %>%
  dplyr::filter(DRUH == "Phengaris teleius")  %>%
  pull(POCET) %>%
  na.omit() %>%  
  sd()

phengaris$UMIST_NAL %>% unique
phengaris$POP_BIOT %>% unique
phengaris$AREA_SITE %>% log10() %>% hist

roky <- phengaris %>%
  st_drop_geometry() %>%
  group_by(NAZ_LOKAL) %>%
  summarise(roky = length(unique(YEAR))) %>%
  arrange(-roky)
roky

kuk <- phengaris %>%
  filter(DRUH == "Phengaris teleius") %>%
  st_drop_geometry() %>%
  group_by(YEAR) %>%
  summarise(n())

phengaris %>%
  filter(DRUH == "Phengaris nausithous") %>%
  st_drop_geometry() %>%
  group_by(PLANT_QUANT) %>%
  summarise(mean(POSITIVE)) %>%
  plot()

phenpole <- phengaris %>%
  st_intersection(., sitmap)

phenpole %>%
  pull(POLE) %>%
  unique() %>%
  length()

phenpole %>%
  st_drop_geometry() %>%
  group_by(YEAR) %>%
  summarise(n())

phenpole %>%
  st_drop_geometry() %>%
  filter(POSITIVE == 1) %>%
  filter(DRUH == "Phengaris nausithous") %>%
  pull(POLE) %>%
  unique() %>%
  length

phenpole %>%
  st_drop_geometry() %>%
  filter(POSITIVE == 1) %>%
  filter(DRUH == "Phengaris teleius") %>%
  pull(POLE) %>%
  unique() %>%
  length

phenpole %>%
  st_drop_geometry() %>%
  filter(SPEC_NUM == 1) %>%
  pull(POLE) %>%
  unique() %>%
  length
phengaris %>%
  st_drop_geometry() %>%
  filter(POSITIVE == 1) %>%
  filter(SPEC_NUM == 1) %>%
  nrow()

phenpole %>%
  st_drop_geometry() %>%
  filter(DRUH == "Phengaris nausithous") %>%
  filter(SPEC_NUM == 1) %>%
  nrow()

phenpole_analysis <- phenpole %>%
  dplyr::select(DRUH, POSITIVE, POLE) %>%
  st_drop_geometry() %>%
  group_by(DRUH, POLE) %>%
  arrange(-POSITIVE) %>%
  slice(1) %>%
  ungroup()

kukuk <- phengaris %>%
  dplyr::select(BIOTOP, NATURAL)
kukukuk <- phengaris %>%
  st_drop_geometry() %>%
  filter(DRUH == "Phengaris nausithous") %>%
  group_by(TTP) %>%
  summarise(n())



# NAUSITHOUS ----
# NULL
model_null_phenau <- glmer(data = phengaris %>%
                           filter(DRUH == "Phengaris nausithous"), 
                         as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
                         family ="binomial")
summary(model_null_phenau)

model_null_phetel <- glmer(data = phengaris %>%
                           filter(DRUH == "Phengaris teleius"), 
                         as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
                         family ="binomial")
summary(model_null_phetel)

# TEST THE YEAR EFFECT
model_nau_yf <- glm(data = phengaris %>%
                 filter(DRUH == "Phengaris nausithous"), 
               as.factor(POSITIVE) ~ as.factor(YEAR),
               family ="binomial")
summary(model_nau_yf)

model_nau_yl <- glm(data = phengaris %>%
                  filter(DRUH == "Phengaris nausithous"), 
                as.factor(POSITIVE) ~ as.numeric(YEAR),
                family ="binomial")
summary(model_nau_yl)

model_nau_pyl <- glm(data = phengaris %>%
                  filter(DRUH == "Phengaris nausithous"), 
                as.factor(POSITIVE) ~ poly(as.numeric(YEAR), 2),
                family ="binomial")
summary(model_nau_pyl)

# tel
model_tel_yf <- glm(data = phengaris %>%
                      filter(DRUH == "Phengaris teleius"), 
                    as.factor(POSITIVE) ~ as.factor(YEAR),
                    family ="binomial")
summary(model_tel_yf)

model_tel_yl <- glm(data = phengaris %>%
                      filter(DRUH == "Phengaris teleius"), 
                    as.factor(POSITIVE) ~ as.numeric(YEAR),
                    family ="binomial")
summary(model_tel_yl)

model_tel_pyl <- glm(data = phengaris %>%
                       filter(DRUH == "Phengaris teleius"), 
                     as.factor(POSITIVE) ~ poly(as.numeric(YEAR), 2),
                     family ="binomial")
summary(model_tel_pyl)


# SPATIAL NAUSITHOUS
model_spat_phenau <- glm(data = phengaris %>%
                           filter(DRUH == "Phengaris nausithous"), 
                         as.factor(POSITIVE) ~ X:Y,
                         family ="binomial")
summary(model_spat_phenau)

# SPATIAL TELEIUS
model_spat_phetel <- glm(data = phengaris %>%
                           filter(DRUH == "Phengaris teleius"), 
                         as.factor(POSITIVE) ~ X:Y,
                         family ="binomial")
summary(model_spat_phetel)

# SPATIOTEMT
model_spatemp_phenau <- glm(data = phengaris %>%
                           filter(DRUH == "Phengaris nausithous"), 
                         as.factor(POSITIVE) ~ as.factor(YEAR) + X:Y,
                         family ="binomial")
summary(model_spatemp_phenau)

model_spatemp_phetel <- glm(data = phengaris %>%
                              filter(YEAR != "2018") %>%
                              filter(DRUH == "Phengaris teleius"), 
                            as.factor(POSITIVE) ~ as.numeric(YEAR) + X:Y,
                            family ="binomial")
summary(model_spatemp_phetel)

# AREA NULL 
model_arean_phenau <- glmer(data = phengaris %>%
                             filter(DRUH == "Phengaris nausithous") %>%
                             filter(AREA_SITE > 0), 
                           as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
                           family ="binomial")
summary(model_arean_phenau)

model_arean_phetel <- glmer(data = phengaris %>%
                              filter(DRUH == "Phengaris teleius") %>%
                              filter(AREA_SITE > 0), 
                            as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_arean_phetel)


# AREA
model_area_phenau <- glmer(data = phengaris %>%
                         filter(DRUH == "Phengaris nausithous") %>%
                         filter(AREA_SITE > 0), 
                       as.factor(POSITIVE) ~ log10(AREA_SITE) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_area_phenau)

model_area_phetel <- glmer(data = phengaris %>%
                             filter(DRUH == "Phengaris teleius") %>%
                             filter(AREA_SITE > 0), 
                           as.factor(POSITIVE) ~ log10(AREA_SITE) + (1 | YEAR) + (1 | X:Y),
                           family ="binomial")
summary(model_area_phetel)


model_arean_phenau <- glm(data = phengaris %>%
                             filter(DRUH == "Phengaris nausithous") %>%
                             filter(is.na(AREA_SITE) == FALSE), 
                           as.factor(POSITIVE) ~ 1,
                           family ="binomial")
summary(model_arean_phenau)

model_arean_phetel <- glm(data = phengaris %>%
                             filter(DRUH == "Phengaris teleius") %>%
                             filter(AREA_SITE > 0), 
                           as.factor(POSITIVE) ~ 1,
                           family ="binomial")
summary(model_arean_phetel)

ggplot(data = phengaris %>%
         filter(DRUH == "Phengaris nausithous") %>%
         filter(AREA_SITE > 0), 
       aes(x = as.factor(POSITIVE), 
           y = log10(AREA_SITE),
           fill = as.factor(PLANT_QUANT))) +
  #geom_violin() +
  geom_boxplot() +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_x_discrete(labels = c("vacant sites", "occupied sites")) +
  scale_fill_discrete(labels = c("single plants", "abundant", "dominant"),
                      name=NULL) +
  xlab("\noccupancy of P. nausithous sites") +
  ylab("log10(site area)\n") +
  theme_classic() +
  theme(text = element_text(size = 20))

ggplot(data = phengaris %>%
         filter(DRUH == "Phengaris teleius") %>%
         filter(AREA_SITE > 0), 
       aes(x = as.factor(POSITIVE), 
           y = log10(AREA_SITE),
           fill = as.factor(PLANT_QUANT))) +
  #geom_violin() +
  geom_boxplot() +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_x_discrete(labels = c("vacant sites", "occupied sites")) +
  scale_fill_discrete(labels = c("single plants", "abundant", "dominant"),
                      name=NULL) +
  xlab("\noccupancy of P. teleius sites") +
  ylab("log10(site area)\n") +
  theme_classic() +
  theme(text = element_text(size = 20))

# AREA POLY
model_spatar_phenau <- glmer(data = phengaris %>%
                         filter(DRUH == "Phengaris nausithous") %>%
                         filter(AREA_SITE > 0), 
                       as.factor(POSITIVE) ~ poly(log10(AREA_SITE), 2) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_spatar_phenau)


# GRAF HABITATU 



# PLANT QUANTITY
model_plant_phenau <- glmer(data = phengaris %>%
                  filter(DRUH == "Phengaris nausithous") %>%
                    filter(AREA_SITE > 0), 
                  as.factor(POSITIVE) ~ PLANT_QUANT + (1 | YEAR) + (1 | X:Y),
                family ="binomial")
summary(model_plant_phenau)

model_plant_phetel <- glmer(data = phengaris %>%
                              filter(DRUH == "Phengaris teleius") %>%
                              filter(AREA_SITE > 0), 
                            as.factor(POSITIVE) ~ PLANT_QUANT + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_plant_phetel)

ggplot(data = phengaris_plant_sum %>%
         filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(PLANT_QUANT), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("single plants", "abundant", "dominant")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()


# PLANT QUANTITY POLY
model_plantpoly_phenau <- glmer(data = phengaris %>%
                              filter(DRUH == "Phengaris nausithous"), 
                            as.factor(POSITIVE) ~ poly(PLANT_QUANT, 2) + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_plantpoly_phenau)

model_plantpoly_phetel <- glmer(data = phengaris %>%
                                  filter(DRUH == "Phengaris teleius"), 
                                as.factor(POSITIVE) ~ poly(PLANT_QUANT, 2) + (1 | YEAR) + (1 | X:Y),
                                family ="binomial")
summary(model_plantpoly_phetel)

# RESOURCE DENSITY
model_dens_phenau <- glmer(data = phengaris %>%
                             filter(DRUH == "Phengaris nausithous") %>%
                             filter(AREA_SITE > 0), 
                             as.factor(POSITIVE) ~ log(AREA_SITE)*as.numeric(PLANT_QUANT) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_dens_phenau)

model_dens_phetel <- glmer(data = phengaris %>%
                             filter(DRUH == "Phengaris teleius"), 
                           as.factor(POSITIVE) ~ log(AREA_SITE)*as.numeric(PLANT_QUANT) + (1 | YEAR) + (1 | X:Y),
                           family ="binomial")
summary(model_dens_phetel)

# MOW NULL
model_mann_phenau <- glmer(data = phengaris %>%
                            filter(DRUH == "Phengaris nausithous") %>%
                            filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                          as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_mann_phenau)

model_mann_phetel <- glmer(data = phengaris %>%
                             filter(DRUH == "Phengaris teleius") %>%
                             filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                           as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
                           family ="binomial")
summary(model_mann_phetel)

# TIMING
model_tim_phenau <- glmer(data = phengaris %>%
                  filter(DRUH == "Phengaris nausithous") %>%
                    filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                  as.factor(POSITIVE) ~ as.factor(TIMING) + (1 | YEAR) + (1 | X:Y),
                family ="binomial")
summary(model_tim_phenau)

model_tim_phetel <- glmer(data = phengaris %>%
                            filter(DRUH == "Phengaris teleius") %>%
                            filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                          as.factor(POSITIVE) ~ as.factor(TIMING) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_tim_phetel)

# METHOD
model_met_phenau <- glmer(data = phengaris %>%
                            filter(DRUH == "Phengaris nausithous") %>%
                            filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                          as.factor(POSITIVE) ~ as.factor(METHOD) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_met_phenau)
summary(model_met_phenau)$AIC

model_met_phetel <- glmer(data = phengaris %>%
                            filter(DRUH == "Phengaris teleius") %>%
                            filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                          as.factor(POSITIVE) ~ as.factor(METHOD) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_met_phetel)

# METHOD & TIMING - SELECTED
model_timmet_phenau <- glmer(data = phengaris %>%
                            filter(DRUH == "Phengaris nausithous") %>%
                              filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                          as.factor(POSITIVE) ~ as.factor(METHOD)*as.factor(TIMING) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_timmet_phenau)
summary(model_timmet_phenau)$AIC

model_timmet_phetel <- glmer(data = phengaris %>%
                               filter(DRUH == "Phengaris teleius") %>%
                               filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                             as.factor(POSITIVE) ~ as.factor(METHOD)*as.factor(TIMING) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_timmet_phetel)

ggplot(data = phengaris_man_sum %>%
         filter(DRUH == "Phengaris nausithous") %>%
         filter(is.na(MANAGEMENT) == FALSE), 
       aes(x = as.factor(MANAGEMENT), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("appropriate mow and\nappropriate timing", 
                              "appropriate mow only", 
                              "appropriate timing only",
                              "inappropriate mow and\ninappropriate timing")) +
  xlab("\nrecorded management at managed sites with P. nausithous") +
  ylab("number of findings\n") +
  theme_classic()

ggplot(data = phengaris_man_sum %>%
         filter(DRUH == "Phengaris teleius") %>%
         filter(is.na(MANAGEMENT) == FALSE), 
       aes(x = as.factor(MANAGEMENT), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("appropriate mow and\nappropriate timing", 
                              "appropriate mow only", 
                              "appropriate timing only",
                              "inappropriate mow and\ninappropriate timing")) +
  xlab("\nrecorded management at managed sites with P. teleius") +
  ylab("number of findings\n") +
  theme_classic()

# NULL MOW
model_nullm_phenau <- glm(data = phengaris %>%
                           filter(DRUH == "Phengaris nausithous") %>%
                            filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                         as.factor(POSITIVE) ~ 1,
                         family ="binomial")
summary(model_nullm_phenau)

model_nullm_phetel <- glm(data = phengaris %>%
                           filter(DRUH == "Phengaris teleius") %>%
                            filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                         as.factor(POSITIVE) ~ 1,
                         family ="binomial")
summary(model_nullm_phetel)

# PASTVA
model_grazen_phenau <- glmer(data = phengaris %>%
                              filter(DRUH == "Phengaris nausithous") %>%
                             filter(is.na(GRAZE_MET) == FALSE), 
                            as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_grazen_phenau)
model_graze_phenau <- glmer(data = phengaris %>%
                               filter(DRUH == "Phengaris nausithous"), 
                             as.factor(POSITIVE) ~ as.factor(GRAZE) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_graze_phenau)
summary(model_timmet_phenau)$AIC
model_grazemet_phenau <- glmer(data = phengaris %>%
                              filter(DRUH == "Phengaris nausithous"), 
                            as.factor(POSITIVE) ~ as.factor(GRAZE_MET) + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_grazemet_phenau)

model_grazen_phetel <- glmer(data = phengaris %>%
                             filter(DRUH == "Phengaris teleius") %>%
                             filter(is.na(GRAZE_MET) == FALSE), 
                           as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
                           family ="binomial")
summary(model_grazen_phetel)

model_graze_phetel <- glmer(data = phengaris %>%
                              filter(DRUH == "Phengaris teleius"), 
                            as.factor(POSITIVE) ~ as.factor(GRAZE) + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_graze_phetel)
model_grazemet_phetel <- glmer(data = phengaris %>%
                              filter(DRUH == "Phengaris teleius"), 
                            as.factor(POSITIVE) ~ as.factor(GRAZE_MET) + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_grazemet_phetel)

model_manage_phenau <- glmer(data = phengaris %>%
                               filter(DRUH == "Phengaris teleius"), 
                             as.factor(POSITIVE) ~ as.factor(GRAZE_MET) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_grazemet_phetel)

# TYP MANAGEMENTU
model_graze_both <- glmer(data = phengaris, 
                            as.factor(POSITIVE) ~ as.factor(DRUH) + as.factor(MOW) + as.factor(GRAZE) + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")


# TIMING METHOD HETEROGENITA COMBINATION
model_mancom_phenau <- glmer(data = phengaris %>%
                  filter(DRUH == "Phengaris nausithous"), 
                  as.factor(POSITIVE) ~ as.factor(TIMING)*as.factor(METHOD)*HET_INN + (1 | YEAR) + (1 | X:Y),
                family ="binomial")
summary(model_mancom_phenau)

summary(model_tim_phenau)$AIC
summary(model_met_phenau)$AIC
summary(model_timmet_phenau)$AIC
summary(model_mancom_phenau)$AIC

ggplot(data = phengaris_time_sum %>%
         filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(TIMING), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("harmful mowing time", "correct mowing time")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

ggplot(data = phengaris_method_sum %>%
         filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(METHOD), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("whole area mowing method", "partial mowing method")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

# PROTECTION
model_pro_phenau <- glmer(data = phengaris %>%
                            filter(DRUH == "Phengaris nausithous"), 
                          as.factor(POSITIVE) ~ as.factor(PROTECT) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_pro_phenau)

model_pro_phetel <- glmer(data = phengaris %>%
                            filter(DRUH == "Phengaris teleius"), 
                          as.factor(POSITIVE) ~ as.factor(PROTECT) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_pro_phetel)

# EVL
model_evl_phenau <- glmer(data = phengaris %>%
                         filter(DRUH == "Phengaris nausithous"), 
                       as.factor(POSITIVE) ~ as.factor(EVL) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_evl_phenau)

ggplot(data = phengaris_evl_sum %>%
         filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(EVL), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside Natura 2000", "within Natura 2000")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

# EVL TARGET
model_evltar_phenau <- glmer(data = phengaris %>%
                            filter(DRUH == "Phengaris nausithous"), 
                          as.factor(POSITIVE) ~ as.factor(EVL_target) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_evltar_phenau)

ggplot(data = phengaris_evltar_sum %>%
         filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(EVL_target), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside Natura 2000\nestablished for P. teleius", 
                              "within Natura 2000\nestablished for P. teleius")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

# EVL COMBINATION - SELECTED
model_evlcom_phenau <- glmer(data = phengaris %>%
                               filter(DRUH == "Phengaris nausithous"), 
                             as.factor(POSITIVE) ~ as.factor(EVL) + as.factor(EVL_target) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_evlcom_phenau)

model_evlcom_phetel <- glmer(data = phengaris %>%
                               filter(DRUH == "Phengaris teleius"), 
                             as.factor(POSITIVE) ~ as.factor(EVL) + as.factor(EVL_target) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_evlcom_phetel)

summary(model_evl_phenau)$AIC
summary(model_evltar_phenau)$AIC
summary(model_evlcom_phenau)$AIC

ggplot(data = phengaris_evlcomb_sum %>%
         filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(EVL_comb), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside Natura 2000", 
                              "Natura 2000 NOT established\nfor P. nausithous",
                              "Natura 2000 established\nfor P. nausithous")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

ggplot(data = phengaris_evlcomb_sum %>%
         filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(EVL_comb), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside Natura 2000", 
                              "Natura 2000 NOT established\nfor P. teleius",
                              "Natura 2000 established\nfor P. teleius")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

# EVL*ROK
model_evly_phenau <- glmer(data = phengaris %>%
                            filter(DRUH == "Phengaris nausithous"), 
                          as.factor(POSITIVE) ~ as.factor(EVL)*as.numeric(YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_evly_phenau)

# MZCHU
model_mzchu_phenau <- glmer(data = phengaris %>%
                         filter(DRUH == "Phengaris nausithous"), 
                       as.factor(POSITIVE) ~ as.factor(MZCHU) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_mzchu_phenau)

model_mzchu_phetel <- glmer(data = phengaris %>%
                              filter(DRUH == "Phengaris teleius"), 
                            as.factor(POSITIVE) ~ as.factor(MZCHU) + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_mzchu_phetel)

ggplot(data = phengaris_mzchu_sum %>%
         filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(MZCHU), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside small-scale\nprotected site", "within small-scale\nprotected sites")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

# MZCHU & EVL
model_mzevl_phenau <- glmer(data = phengaris %>%
                         filter(DRUH == "Phengaris nausithous"), 
                       as.factor(POSITIVE) ~ as.factor(MZCHU) * as.factor(EVL) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_mzevl_phenau)

# TTP
model6_phenau <- glmer(data = phengaris %>%
                         filter(DRUH == "Phengaris nausithous"), 
                       as.factor(POSITIVE) ~ as.factor(TTP) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model6_phenau)

# MANAGEMENT & TTP
model7_phenau <- glmer(data = phengaris %>%
                         filter(DRUH == "Phengaris nausithous"), 
                       as.factor(POSITIVE) ~ as.factor(TIMING)*as.factor(METHOD)*as.factor(TTP) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model7_phenau)
summary(model6_phenau)$AIC
summary(model7_phenau)$AIC

# FSB
model_fsb_phenau <- glmer(data = phengaris %>%
                            filter(DRUH == "Phengaris nausithous"), 
                          as.factor(POSITIVE) ~ as.factor(FSB) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_fsb_phenau)

model_fsb_phetel <- glmer(data = phengaris %>%
                            filter(DRUH == "Phengaris teleius"), 
                          as.factor(POSITIVE) ~ as.factor(FSB) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_fsb_phetel)

# BIOTOPY + DRUH
model_biomap_both <- glmer(data = phengaris, 
                            as.factor(POSITIVE) ~ as.factor(DRUH) + as.factor(BIOTOP) + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_biomap_both)

model_biorec_both <- glmer(data = phengaris, 
                            as.factor(POSITIVE) ~ as.factor(DRUH) + 
                             as.factor(TTP) + as.factor(ZARUST) + as.factor(PRIKOP) + as.factor(JINY) +
                             as.factor(MOW) + as.factor(ZARUST) + 
                             (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_biorec_both)

# VNITŘNÍ HETEROGENITA 
model_hetinn_phenau <- glmer(data = phengaris %>%
                               filter(DRUH == "Phengaris nausithous"), 
                             as.factor(POSITIVE) ~ HET_INN + (1 | YEAR) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_hetinn_phenau)

model_hetinn_phetel <- glmer(data = phengaris %>%
                               filter(DRUH == "Phengaris teleius"), 
                             as.factor(POSITIVE) ~ HET_INN + (1 | YEAR) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_hetinn_phetel)

# VNĚJŠÍ  HETEROGENITA 
model_hetout_phenau <- glmer(data = phengaris %>%
                               filter(DRUH == "Phengaris nausithous"), 
                             as.factor(POSITIVE) ~ as.numeric(HET_OUT) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_hetout_phenau)
model_hetout_phenau <- glmer(data = phengaris %>%
                               filter(DRUH == "Phengaris nausithous"), 
                             as.factor(POSITIVE) ~ as.numeric(HET_OUT) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_hetout_phenau)

model_hetout_phetel <- glmer(data = phengaris %>%
                               filter(DRUH == "Phengaris teleius"), 
                             as.factor(POSITIVE) ~ HET_OUT + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_hetout_phetel)

# THREATS
model_tap_phenau <- glmer(data = phengaris %>%
                               filter(DRUH == "Phengaris nausithous") %>%
                             filter(AREA_SITE > 0), 
                             as.factor(POSITIVE) ~ SUM_THREATS + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_tap_phenau)

# TELEIUS ----
# NULL
model_null_phetel <- glm(data = phengaris %>%
                             filter(DRUH == "Phengaris teleius"), 
                           as.factor(POSITIVE) ~ 1,
                           family ="binomial")
summary(model_null_phetel)

# TEST THE YEAR EFFECT
model_y <- glm(data = phengaris %>%
                 filter(DRUH == "Phengaris teleius"), 
               as.factor(POSITIVE) ~ as.factor(YEAR),
               family ="binomial")
summary(model_y)

model_yl <- glm(data = phengaris %>%
                 filter(DRUH == "Phengaris teleius"), 
               as.factor(POSITIVE) ~ as.numeric(YEAR),
               family ="binomial")
summary(model_yl)

model_pyl <- glm(data = phengaris %>%
                  filter(DRUH == "Phengaris teleius"), 
                as.factor(POSITIVE) ~ poly(as.numeric(YEAR), 2),
                family ="binomial")
summary(model_pyl)

# TEST THE SITE EFFECT
model_p <- glm(data = phengaris %>%
                 filter(DRUH == "Phengaris teleius"), 
               as.factor(POSITIVE) ~ as.factor(SITMAP),
               family ="binomial")
summary(model_p)

model_xy <- glmer(data = phengaris %>%
                 filter(DRUH == "Phengaris teleius"), 
               as.factor(POSITIVE) ~ (1 | YEAR) + (1 | X:Y),
               family ="binomial")
summary(model_xy)

# AREA - SELECTED
model0_phetel <- glmer(data = phengaris %>%
                         filter(DRUH == "Phengaris teleius") %>%
                         filter(AREA_SITE > 0), 
                       as.factor(POSITIVE) ~ log10(AREA_SITE) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model0_phetel)

ggplot(data = phengaris %>%
         filter(DRUH == "Phengaris nausithous") %>%
         filter(AREA_SITE > 0), 
       aes(x = as.factor(POSITIVE), 
           y = log10(AREA_SITE),
           fill = as.factor(PLANT_QUANT))) +
  #geom_violin() +
  geom_boxplot() +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_discrete(labels = c("single plants", "abundant", "dominant")) +
  xlab("") +
  ylab("log10(AREA)\n") +
  theme_classic()

# PLANT
model1_phetel <- glmer(data = phengaris %>%
                         filter(DRUH == "Phengaris teleius"), 
                       as.factor(POSITIVE) ~ PLANT_QUANT + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model1_phetel)
summary(model0_phetel)$AIC
summary(model1_phetel)$AIC

ggplot(data = phengaris_plant_sum %>%
         filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(PLANT_QUANT), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("single plants", "abundant", "dominant")) +
  xlab("") +
  ylab("number of sites\n") +
  theme_classic()

# PLANT QUANTITY POLY
model_plantpoly_phetel <- glmer(data = phengaris %>%
                                  filter(DRUH == "Phengaris teleius"), 
                                as.factor(POSITIVE) ~ poly(PLANT_QUANT, 2) + (1 | YEAR) + (1 | X:Y),
                                family ="binomial")
summary(model_plantpoly_phetel)

# RESOURCE DENSITY
model_dens_phetel <- glmer(data = phengaris %>%
                             filter(DRUH == "Phengaris teleius"), 
                           as.factor(POSITIVE) ~ log(AREA_SITE)*as.numeric(PLANT_QUANT) + (1 | YEAR) + (1 | X:Y),
                           family ="binomial")
summary(model_dens_phetel)

# METHOD
model2_phetel <- glmer(data = phengaris %>%
                         filter(DRUH == "Phengaris teleius"), 
                       as.factor(POSITIVE) ~ as.factor(METHOD) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model2_phetel)

# TIMING
model3_phetel <- glmer(data = phengaris %>%
                         filter(DRUH == "Phengaris teleius"), 
                       as.factor(POSITIVE) ~ as.factor(TIMING) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model3_phetel)

# METHOD AND TIMING
model_mancom_phetel <- glmer(data = phengaris %>%
                         filter(DRUH == "Phengaris teleius"), 
                       as.factor(POSITIVE) ~ as.factor(METHOD)*as.factor(TIMING) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_mancom_phetel)

summary(model3_phetel)$AIC
summary(model2_phetel)$AIC
summary(model_mancom_phetel)$AIC

ggplot(data = phengaris_time_sum %>%
         filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(TIMING), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("harmful mowing time", "correct mowing time")) +
  xlab("") +
  ylab("number of sites\n") +
  theme_classic()

ggplot(data = phengaris_method_sum %>%
         filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(METHOD), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("whole area mowing method", "partial mowing method")) +
  xlab("") +
  ylab("number of sites\n") +
  theme_classic()

phengaris %>%
  filter(DRUH == "Phengaris teleius") %>%
  filter(POSITIVE == 1) %>%
  filter(METHOD == 1 & TIMING == 1) %>%
  filter(EVL == 1) %>%
  nrow()

# EVL
model_evl_phetel <- glmer(data = phengaris %>%
                            filter(DRUH == "Phengaris teleius"), 
                          as.factor(POSITIVE) ~ as.factor(EVL) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_evl_phetel)

ggplot(data = phengaris_evl_sum %>%
         filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(EVL), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside Natura 2000", "within Natura 2000")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

# EVL TARGET
model_evltar_phetel <- glmer(data = phengaris %>%
                               filter(DRUH == "Phengaris teleius"), 
                             as.factor(POSITIVE) ~ as.factor(EVL_target) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_evltar_phetel)

ggplot(data = phengaris_evltar_sum %>%
         filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(EVL_target), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside Natura 2000\nestablished for P. teleius", 
                              "within Natura 2000\nestablished for P. teleius")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

# EVL COMBINATION
model_evlcom_phetel <- glmer(data = phengaris %>%
                               filter(DRUH == "Phengaris teleius"), 
                             as.factor(POSITIVE) ~ as.factor(EVL) + as.factor(EVL_target) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_evlcom_phetel)

summary(model_evl_phetel)$AIC
summary(model_evltar_phetel)$AIC
summary(model_evlcom_phetel)$AIC

ggplot(data = phengaris_evlcomb_sum %>%
         filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(EVL_comb), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside Natura 2000", 
                              "within Natura 2000 not\nestablished for P. teleius",
                              "within Natura 2000\nestablished for P. teleius")) +
  xlab("") +
  ylab("number of sites\n") +
  theme_classic()

model_procom_phetel <- glmer(data = phengaris %>%
                               filter(DRUH == "Phengaris teleius"), 
                             as.factor(POSITIVE) ~ as.factor(EVL) + as.factor(EVL_target) + as.factor(MZCHU)+ (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_procom_phetel)

# MZCHU
model_mzchu_phetel <- glmer(data = phengaris %>%
                         filter(DRUH == "Phengaris teleius"), 
                       as.factor(POSITIVE) ~ as.factor(MZCHU) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_mzchu_phetel)
ggplot(data = phengaris_mzchu_sum %>%
         filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(MZCHU), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside small-scale\nprotected site", "within small-scale\nprotected sites")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()


# FSB
model_fsb_phetel <- glmer(data = phengaris %>%
                            filter(DRUH == "Phengaris teleius") %>%
                            filter(FSB %in% c("T", "X", "moz.")), 
                             as.factor(POSITIVE) ~ as.factor(FSB) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_fsb_phetel)

# HETEROENITY
model_hetinn_phetel <- glmer(data = phengaris %>%
                         filter(DRUH == "Phengaris teleius"), 
                       as.factor(POSITIVE) ~ HET_INN + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_hetinn_phetel)

model_hetout_phetel <- glmer(data = phengaris %>%
                               filter(DRUH == "Phengaris teleius"), 
                             as.factor(POSITIVE) ~ HET_OUT + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_hetout_phetel)


phengaris_sum %>%
  filter(DRUH == "Phengaris teleius") %>%
  pull(COUNT) %>%
  sum()

# selektovat 

# zrandomizovat polohu/lokalitu

# všechny one-way interakce
# heterogenita oběma způsoby, testovat postupně
# Modely pro každý druh zvlášť
# Rozloha * kytka
# heterogenita * rozloha * kytka
# přítomnost 2 druhů 
# způsob seče * načasování seče * heterogenita * rozloha

# procento varience z year random effects

# BOTH SPECIES ----
modelboth <- glmer(data = phengaris, 
                          as.factor(POSITIVE) ~ as.factor(SPEC_NUM)*as.factor(DRUH) + (1 | YEAR) + (1 | (X:Y)),
                          family ="binomial")
summary(modelboth)

ggplot(data = phengaris_spe_sum %>%
         filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(SPEC_NUM), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("without P. teleius", "with P. teleius")) +
  xlab("") +
  ylab("number of sites\n") +
  theme_classic()

ggplot(data = phengaris_spe_sum %>%
         filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(SPEC_NUM), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("without P. nausithous", "with P. nausithous")) +
  xlab("") +
  ylab("number of sites\n") +
  theme_classic()

ggplot(data = phengaris_spe_sum %>%
         filter(SPEC_NUM == 0), 
       aes(x = as.factor(DRUH), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("Phengaris nausithous\nwithout P. teleius",
                              "Phengaris teleius\nwithout P. nausithous")) +
  xlab("") +
  ylab("number of sites\n") +
  theme_classic()

model_species <- glmer(data = phengaris,
                       as.factor(POSITIVE) ~ as.factor(DRUH) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_species)

ggplot(data = phengaris_sum, 
       aes(x = as.factor(DRUH), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("without P. nausithous", "with P. nausithous")) +
  xlab("") +
  ylab("number of sites\n") +
  theme_classic()

phengaris %>%
  filter(DRUH == "Phengaris nausithous" & POSITIVE == 1) %>%
  pull(SITMAP) %>%
  unique() %>%
  length()

phengaris %>%
  filter(DRUH == "Phengaris teleius" & POSITIVE == 1) %>%
  pull(SITMAP) %>%
  unique() %>%
  length()

phengaris %>%
  pull(SITMAP) %>%
  unique() %>%
  length()
phengaris %>%
  pull(AUTOR) %>%
  unique() %>%
  length()

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

# OBSERVED HABITATS 
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

# MANAGEMENT 
summan <- phengaris %>%
  st_drop_geometry() %>%
  filter(POSITIVE == 1) %>%
  group_by(MOW, GRAZE, ZARUST) %>%
  summarise(n())

summan_phenau <- phengaris %>%
  st_drop_geometry() %>%
  dplyr::select(DRUH, POSITIVE, MOW, GRAZE, ZARUST) %>%
  pivot_longer(.,
               cols = c(3:5)) %>%
  filter(value == 1) %>%
  dplyr::select(-value) %>%
  filter(POSITIVE == 1) %>%
  filter(DRUH == "Phengaris nausithous") %>%
  group_by(name) %>%
  summarise(COUNT = n(),
            PERC = n()/nrow(phengaris %>%
                              dplyr::filter(DRUH == "Phengaris nausithous") %>%
                              dplyr::filter(POSITIVE == 1))*100) %>%
  mutate(name = case_when(name == "MOW" ~ "mowing",
                          name == "ZARUST" ~ "neglected\ngrassland",
                          name == "GRAZE" ~ "grazing",
                          name == "PRIKOP" ~ "road verge,\nditch"))

summan_phetel <- phengaris %>%
  st_drop_geometry() %>%
  dplyr::select(DRUH, POSITIVE, MOW, GRAZE, ZARUST) %>%
  pivot_longer(.,
               cols = c(3:5)) %>%
  filter(value == 1) %>%
  dplyr::select(-value) %>%
  filter(POSITIVE == 1) %>%
  filter(DRUH == "Phengaris teleius") %>%
  group_by(name) %>%
  summarise(COUNT = n(),
            PERC = n()/nrow(phengaris %>%
                              dplyr::filter(DRUH == "Phengaris teleius") %>%
                              dplyr::filter(POSITIVE == 1))*100) %>%
  mutate(name = case_when(name == "MOW" ~ "mowing",
                          name == "ZARUST" ~ "neglected\ngrassland",
                          name == "GRAZE" ~ "grazing",
                          name == "PRIKOP" ~ "road verge,\nditch"))

ggplot(data = summan_phenau, 
       aes(x = fct_reorder(name, COUNT, .desc = TRUE), 
           y = COUNT)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  xlab("management recorded at sites with P. nausithous") +
  ylab("number of sites\n") +
  theme_classic() +
  theme(text = element_text(size = 20))

ggplot(data = summan_phetel, 
       aes(x = fct_reorder(name, COUNT, .desc = TRUE), 
           y = COUNT)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  xlab("management recorded at sites with  P. teleius") +
  ylab("number of sites\n") +
  theme_classic() +
  theme(text = element_text(size = 20))

# ABUNDANCE
wilcox.test(phengaris %>%
               filter(DRUH == "Phengaris nausithous") %>%
               pull(POCET),
             phengaris %>%
               filter(DRUH == "Phengaris teleius") %>%
               pull(POCET))
kruskal.test(phengaris %>%
         pull(POCET),
       phengaris %>%
         pull(DRUH))

wilcox.test(phengaris %>%
              filter(DRUH == "Phengaris nausithous") %>%
              pull(POSITIVE),
            phengaris %>%
              filter(DRUH == "Phengaris teleius") %>%
              pull(POSITIVE))
kruskal.test(phenpole_analysis %>%
               pull(POSITIVE),
             phenpole_analysis %>%
               pull(POSITIVE))

ggplot(data = phengaris, 
       aes(x = as.factor(DRUH), 
           y = log(POCET))) +
  #geom_violin() +
  geom_boxplot() +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  #scale_x_discrete(labels = c("vacant sites", "occupied sites")) +
  scale_fill_discrete(labels = c("single plants", "abundant", "dominant"),
                      name=NULL) +
  xlab("") +
  ylab("log10(site area)\n") +
  theme_classic() +
  theme(text = element_text(size = 30))

phenpole %>%
  st_drop_geometry() %>%
  group_by(ID_LOKAL) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(POLE) %>%
  summarise(sitenum = n()) %>%
  pull(sitenum) %>%
  min()
phenpole %>%
  st_drop_geometry() %>%
  group_by(ID_LOKAL) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(YEAR, AUTOR) %>%
  summarise(pocet = length(unique(POLE))) %>%
  pull(pocet) %>%
  max()
phenpole %>%
  st_drop_geometry() %>%
  group_by(ID_LOKAL) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(YEAR, AUTOR) %>%
  summarise(pocet = length(unique(POLE))) %>%
  arrange(-pocet)


# THREATS AND PRESSURES ----
phengaris_tap_all <- phengaris %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(SUM_THREATS > 0) %>%
  dplyr::mutate(DRUH = dplyr::case_when(DRUH == "Phengaris nausithous" ~ "nausithous",
                                        DRUH == "Phengaris teleius" ~ "teleius")) %>%
  dplyr::select(DRUH, POSITIVE,
                PLANT_QUANT, TTP, ZARUST, PRIKOP, HET_INN, NATURAL, EVL, EVL_target, MZCHU,
                LandUseChange, Abandonment, HarmfulMow, HarmfulGrazing,
                GrazingByeffects, FertilizerUse, Afforestation, Invasives,
                NativeDominants, AbioticNaturalProcesses,
                Encroachment, BiomassAccumulation, Eutrophization, None) %>%
  dplyr::group_by(DRUH, POSITIVE) %>%
  dplyr::summarise(ID = paste0(unique(DRUH), unique(POSITIVE)),
                   PLANT_QUANT = mean(PLANT_QUANT), 
                   TTP = mean(TTP), 
                   ZARUST = mean(ZARUST), 
                   PRIKOP = mean(PRIKOP), 
                   HET_INN = mean(HET_INN),
                   NATURAL = mean(NATURAL),
                   EVL = mean(EVL), 
                   EVL_TAR = mean(EVL_target),
                   MZCHU = mean(MZCHU),
                   LandUseChange_sum = sum(LandUseChange), 
                   Abandonment_sum = sum(Abandonment), 
                   HarmfulMow_sum = sum(HarmfulMow), 
                   HarmfulGrazing_sum = sum(HarmfulGrazing),
                   GrazingByeffects_sum = sum(GrazingByeffects), 
                   FertilizerUse_sum = sum(FertilizerUse), 
                   Afforestation_sum = sum(Afforestation), 
                   Invasives_sum = sum(Invasives),
                   NativeDominants_sum = sum(NativeDominants), 
                   AbioticNaturalProcesses_sum = sum(AbioticNaturalProcesses),
                   Encroachment_sum = sum(Encroachment), 
                   BiomassAccumulation_sum = sum(BiomassAccumulation), 
                   Eutrophization_sum = sum(Eutrophization), 
                   None_sum = sum(None),
                   LandUseChange = mean(LandUseChange), 
                   Abandonment = mean(Abandonment), 
                   HarmfulMow = mean(HarmfulMow), 
                   HarmfulGrazing = mean(HarmfulGrazing),
                   GrazingByeffects = mean(GrazingByeffects), 
                   FertilizerUse = mean(FertilizerUse), 
                   Afforestation = mean(Afforestation), 
                   Invasives = mean(Invasives),
                   NativeDominants = mean(NativeDominants), 
                   AbioticNaturalProcesses = mean(AbioticNaturalProcesses),
                   Encroachment = mean(Encroachment), 
                   BiomassAccumulation = mean(BiomassAccumulation), 
                   Eutrophization = mean(Eutrophization), 
                   None = mean(None)) %>%
  dplyr::ungroup()
phengaris_tap <- phengaris_tap_all %>%
  tibble::column_to_rownames('ID') %>%
  dplyr::select(PLANT_QUANT, 
                TTP, 
                ZARUST, 
                PRIKOP, 
                HET_INN, 
                NATURAL,
                EVL, MZCHU,
                LandUseChange, 
                Abandonment, 
                HarmfulMow, 
                HarmfulGrazing,
                GrazingByeffects, 
                FertilizerUse, 
                Afforestation, 
                Invasives,
                NativeDominants, 
                AbioticNaturalProcesses,
                Encroachment, 
                BiomassAccumulation, 
                Eutrophization, 
                None
                )

write.csv2(phengaris_tap_all,
           "phengaris_pca.csv",
           fileEncoding = "Windows-1250")
write.csv2(phengaris %>%
             st_drop_geometry(),
           "phengaris_data_export.csv",
           fileEncoding = "Windows-1250",
           row.names = FALSE)

PCA <- rda(phengaris_tap, scale = FALSE)
plot(PCA, display = "species", type = "text")
speciesPCA <- PCA$CA$v
speciesPCA
biplot(PCA, choices = c(1,2))
biplot(PCA, xlim = c(-0.3, 0.3), ylim = c(-0.45, 0.6))

# MAPS ----
hypso_read <- stars::read_stars("CRhypso100.tif") 


phengaris_map <- ggplot2::ggplot(data = phengaris %>%
                                   dplyr::mutate(geometry = sf::st_centroid(geometry))) +
  #ggplot2::geom_raster(data = hypso_read) +
  ggplot2::geom_sf(aes(color = as.factor(POSITIVE)),
                   size = 0.5, show.legend = FALSE) +
  ggplot2::geom_sf(data = czechia, fill = NA) +
  ggplot2::scale_y_continuous(expand = expand_scale(mult = c(0.05, 0.05))) +
  ggplot2::scale_x_continuous(expand = expand_scale(mult = c(0.05, 0.05))) +
  ggplot2::theme_void()
phengaris_map

phenau_dist <- phengaris %>%
  filter(DRUH == "Phengaris nausithous" & POSITIVE == 1) %>%
  pull(SITMAP) %>%
  unique()
phetel_dist <- phengaris %>%
  filter(DRUH == "Phengaris teleius" & POSITIVE == 1) %>%
  pull(SITMAP) %>%
  unique()
sample_dist <- phengaris %>%
  pull(SITMAP) %>%
  unique()

phengaris_dist_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = czechia, fill = NA, size = 1.5) +
  ggplot2::geom_sf(data = sitmap %>%
                     dplyr::filter(POLE %in% sample_dist),
                   fill = "light grey") +
  ggplot2::geom_sf(data = sitmap %>%
                     dplyr::filter(POLE %in% phenau_dist),
                   fill = "blue",
                   alpha = .5) +
  ggplot2::geom_sf(data = sitmap %>%
                     dplyr::filter(POLE %in% phetel_dist),
                   fill = "red",
                   alpha = .5) +
  ggplot2::scale_y_continuous(expand = expand_scale(mult = c(0.05, 0.05))) +
  ggplot2::scale_x_continuous(expand = expand_scale(mult = c(0.05, 0.05))) +
  ggplot2::theme_void()
phengaris_dist_map

phetel_dist

both_dist

# ZSUTIS SEMIKVANTATIVNĚ
phengaris %>%
  filter(is.na(POCET) == FALSE) %>%
  pull(POCET) %>%
  hist

# POSTER ----
ggplot(data = phengaris_evl_sum %>%
         filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(EVL), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside Natura 2000", "Natura 2000 sites")) +
  xlab("\nPhengaris nausithous") +
  ylab("number of sites\n") +
  theme_classic() +
  theme(text = element_text(size = 26),
        legend.position="none")
ggplot(data = phengaris_evl_sum %>%
         filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(EVL), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside Natura 2000", "Natura 2000 sites")) +
  xlab("\nPhengaris teleius") +
  ylab("") +
  theme_classic() +
  theme(text = element_text(size = 26))

ggplot(data = phengaris_spe_sum %>%
         filter(SPEC_NUM == 0), 
       aes(x = as.factor(DRUH), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("Phengaris nausithous\nwithout P. teleius",
                              "Phengaris teleius\nwithout P. nausithous")) +
  xlab("") +
  ylab("number of sites\n") +
  theme_classic() +
  theme(text = element_text(size = 26))

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

phengaris_edit %>%
  #filter(YEAR == 2022) %>%
  nrow

kukukuk <- phengaris %>%
  dplyr::select(geom)

plot(filter(phengaris, YEAR == 2022)$geom)

# APPENDIX ----
write.csv2(phengaris,
           )


# SET WD ----
setwd("~/phengaris_management")

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
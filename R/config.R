#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#                   Config file
#
#
#                   Jonáš Gaigr
#                       2025
#
#----------------------------------------------------------#

# --- 1. Install and activate renv --------------------------------------------
# setup_minimal.R — minimal, idempotent package loader (Windows: binary-first)
pkgs <- c("tidyverse","sf","sp","proj4","openxlsx","lmerTest","vegan",
          "ggplot2", "ggforce", "ggrepel", "grid", "ggpubr", "officer", "flextable",
          "GLMMadaptive","RCzechia","rvest","httr","xml2","Matrix","lme4","remotes")
type <- if (.Platform$OS.type=="windows") "binary" else "source"

inst_if_missing <- function(pkg){
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, dependencies = TRUE, type = type)
  ok <- tryCatch({ suppressPackageStartupMessages(library(pkg, character.only=TRUE)); TRUE },
                 error = function(e) FALSE)
  if (!ok) {
    message("Broken install detected for ", pkg, " → reinstalling (", type, ").")
    try(remove.packages(pkg), silent = TRUE)
    install.packages(pkg, dependencies = TRUE, type = type)
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
  }
}

invisible(lapply(pkgs, inst_if_missing))

# GitHub packages (install only if missing)
if (!requireNamespace("rn2kcz", quietly = TRUE)) remotes::install_github("jonasgaigr/rn2kcz")
if (!requireNamespace("rndop", quietly = TRUE)) remotes::install_github("kalab-oto/rndop")
suppressPackageStartupMessages(library(rn2kcz)); suppressPackageStartupMessages(library(rndop))

# quick lme4 smoke test; if it fails, reinstall Matrix+lme4 (binary on Windows)
test_ok <- tryCatch({
  suppressMessages(lme4::glmer(cbind(incidence, size - incidence) ~ 1 + (1|group),
                               data = lme4::cbpp, family = binomial)); TRUE
}, error = function(e) {
  message("lme4 test failed: ", conditionMessage(e), "\nReinstalling Matrix + lme4 (", type, ")...")
  try(remove.packages(c("lme4","Matrix")), silent = TRUE)
  install.packages(c("Matrix","lme4"), dependencies = TRUE, type = type)
  suppressPackageStartupMessages(library(Matrix)); suppressPackageStartupMessages(library(lme4))
  TRUE
})

message("Packages installed & loaded. (Binary on Windows: ", type == "binary", ")")

#----------------------------------------------------------#
# Load data -----
#----------------------------------------------------------#
#--------------------------------------------------#
## Load remote data -----
#--------------------------------------------------#
# Borders of Czechia
czechia_border <- 
  RCzechia::republika(
    resolution = "high"
    ) %>%
  sf::st_transform(
    ., 
    st_crs("+init=epsg:5514")
  ) 

# Data on protected areas and mapping fields
endpoint <- "http://gis.nature.cz/arcgis/services/Aplikace/Opendata/MapServer/WFSServer?"
caps_url <- base::paste0(endpoint, "request=GetCapabilities&service=WFS")

layer_name_evl <- "Opendata:Evropsky_vyznamne_lokality"
layer_name_mzchu <- "Opendata:Maloplosna_zvlaste_chranena_uzemi__MZCHU_"
layer_name_sitmap1rad <- "Opendata:Mapovaci_sit_-_deleni_1.radu"

getfeature_url_evl <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_evl
)
getfeature_url_mzchu <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_mzchu
)
getfeature_url_sitmap1rad <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_sitmap1rad
)


evl <- sf::st_read(getfeature_url_evl) %>%
  sf::st_transform(
    ., 
    st_crs("+init=epsg:5514")
    ) 
mzchu <- sf::st_read(getfeature_url_mzchu) %>%
  sf::st_transform(
    ., 
    st_crs("+init=epsg:5514")
    )
sitmap <- sf::st_read(getfeature_url_sitmap1rad) %>%
  sf::st_transform(
    ., 
    st_crs("+init=epsg:5514")
    ) %>%
  sf::st_crop(
    .,
    czechia_border  # crop by the border of Czechia
    )

# Sites where the species are target features
rn2kcz::load_n2k_sites()

#--------------------------------------------------#
## Load range layers -----
#--------------------------------------------------#
range_nausithous <- 
  sf::st_read(
    "Data/Input/Reporting_range/nausithous.shp"
  ) %>%
  sf::st_transform(5514) %>%
  sf::st_make_valid() %>%
  dplyr::mutate(
    row_n = dplyr::row_number()
    )

range_teleius <- 
  sf::st_read(
    "Data/Input/Reporting_range/teleius.shp"
  ) %>%
  sf::st_transform(5514) %>%
  sf::st_make_valid() %>%
  dplyr::mutate(
    row_n = dplyr::row_number()
  )

#--------------------------------------------------#
## Load species data 2012 - 2018 -----
#--------------------------------------------------#

data_Pnau_old <- 
  readr::read_csv2(
    "Data/Input/Phengaris_nausithous_2012_2018.csv",
    locale = locale(encoding = "Windows-1250")
  )
data_Ptel_old <- 
  readr::read_csv2(
    "Data/Input/Phengaris_teleius_2012_2018.csv",
    locale = locale(encoding = "Windows-1250")
  )

data_old <-
  dplyr::bind_rows(
    data_Pnau_old,
    data_Ptel_old
  )

lokal_Pnau_b_old <- 
  sf::st_read(
    "Data/Input/Phengaris_nausithous_2012_2018/w03_nd_lokalizace_b.shp"
    )  %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("POINT") %>%
  sf::st_make_valid()

lokal_Pnau_p_old <- sf::st_read(
  "Data/Input/Phengaris_nausithous_2012_2018/w03_nd_lokalizace_p.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("POLYGON") %>%
  sf::st_make_valid()

lokal_Pnau_l_old <- sf::st_read(
  "Data/Input/Phengaris_nausithous_2012_2018/w03_nd_lokalizace_l.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("LINESTRING") %>%
  sf::st_make_valid()

lokal_Ptel_b_old <- sf::st_read(
  "Data/Input/Phengaris_teleius_2012_2018/w03_nd_lokalizace_b.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("POINT") %>%
  sf::st_make_valid()

lokal_Ptel_p_old <- sf::st_read(
  "Data/Input/Phengaris_teleius_2012_2018/w03_nd_lokalizace_p.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("POLYGON") %>%
  sf::st_make_valid()

lokal_Ptel_l_old <- sf::st_read(
  "Data/Input/Phengaris_teleius_2012_2018/w03_nd_lokalizace_l.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("LINESTRING") %>%
  sf::st_make_valid()

lokal_old <- dplyr::bind_rows(
  lokal_Pnau_b_old, 
  lokal_Pnau_p_old, 
  lokal_Pnau_l_old,
  lokal_Ptel_b_old, 
  lokal_Ptel_p_old, 
  lokal_Ptel_l_old
) %>%
  dplyr::rename(
    ID_LOKAL = idx_nd_lok
  ) %>%
  sf::st_as_sf() %>%
  sf::st_make_valid()

phengaris_lokal_old <- 
  data_old %>%
  dplyr::left_join(
    ., 
    lokal_old
  ) %>%
  sf::st_as_sf() %>%
  sf::st_make_valid()

#--------------------------------------------------#
## Load species data 2019 - 2024 -----
#--------------------------------------------------#
data_Pnau_new <- 
  readr::read_csv2(
    "Data/Input/Phengaris_nausithous_2019_2024.csv",
    locale = locale(encoding = "Windows-1250")
    )
data_Ptel_new <- 
  readr::read_csv2(
    "Data/Input/Phengaris_teleius_2019_2024.csv",
    locale = locale(encoding = "Windows-1250")
    )

data_new <-
  dplyr::bind_rows(
    data_Pnau_new,
    data_Ptel_new
  )

lokal_Pnau_b_new <- sf::st_read(
  "Data/Input/Phengaris_nausithous_2019_2024/w03_nd_lokalizace_b.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("POINT") %>%
  sf::st_make_valid()

lokal_Pnau_p_new <- sf::st_read(
  "Data/Input/Phengaris_nausithous_2019_2024/w03_nd_lokalizace_p.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_make_valid()

lokal_Pnau_l_new <- sf::st_read(
  "Data/Input/Phengaris_nausithous_2019_2024/w03_nd_lokalizace_l.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_make_valid()

lokal_Ptel_b_new <- sf::st_read(
  "Data/Input/Phengaris_teleius_2019_2024/w03_nd_lokalizace_b.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_make_valid()

lokal_Ptel_p_new <- sf::st_read(
  "Data/Input/Phengaris_teleius_2019_2024/w03_nd_lokalizace_p.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_make_valid()

lokal_Ptel_l_new <- sf::st_read(
  "Data/Input/Phengaris_teleius_2019_2024/w03_nd_lokalizace_l.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_make_valid()

lokal_new <- dplyr::bind_rows(
  lokal_Pnau_b_new, 
  lokal_Pnau_p_new, 
  lokal_Pnau_l_new,
  lokal_Ptel_b_new, 
  lokal_Ptel_p_new, 
  lokal_Ptel_l_new
  ) %>%
  dplyr::rename(
    ID_LOKAL = idx_nd_lok
    ) %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  dplyr::distinct()

phengaris_lokal_new <- 
  data_new %>%
  dplyr::left_join(
    ., 
    lokal_new,
    by = "ID_LOKAL"
    ) %>%
  sf::st_as_sf() %>%
  sf::st_make_valid()

sf::st_write(
  lokal_new,
  "Data/Processed/lokal_new.gpkg"
)

#----------------------------------------------------------#
# List ZDROJ for analyis -----
#----------------------------------------------------------#
target_mon_zdroj <- c(
  "Kolektiv autorů (2017) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2018) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2019) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2020) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2021) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2020) Monitoring motýlů. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2021) Monitoring motýlů. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2022) Monitoring motýlů.",
  "Kolektiv autorů (2023) Monitoring motýlů.",
  "Kolektiv autorů (2024) Monitoring motýlů."
  )

#----------------------------------------------------------#
# Delete Temp files -----
#----------------------------------------------------------#
temp_dir <- "Data/Temp"

# List all files and folders inside, full paths
files_to_delete <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)

# Delete all files and folders
file.remove(files_to_delete[!dir.exists(files_to_delete)])  # Remove files only

# Remove directories (empty after deleting files inside)
dirs_to_delete <- list.dirs(temp_dir, recursive = TRUE, full.names = TRUE)
# Sort in decreasing order to delete subfolders before parents
dirs_to_delete <- dirs_to_delete[order(nchar(dirs_to_delete), decreasing = TRUE)]

# Remove directories
for (d in dirs_to_delete) {
  unlink(d, recursive = TRUE, force = TRUE)
}

# load habitats ----
load_vmb <- function(vmb_x = 1, clean = TRUE) {
  
  # Inicializace výstupního listu
  output <- list()
  
  # -------------------------------------------------------------------------#
  # VMBX = 0: Aktuální vrstva
  # -------------------------------------------------------------------------#
  if(vmb_x == 0) {
    
    # Načtení dat
    vmb_shp_sjtsk_akt_read <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_AKTUALNI/Aktualni_Segment.shp", 
        options = "ENCODING=WINDOWS-1250"
      )
    vmb_hab_dbf_akt <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_AKTUALNI/Biotop/HAB_BIOTOP.dbf", 
        options = "ENCODING=WINDOWS-1250"
      )
    vmb_pb_dbf_akt <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_AKTUALNI/Biotop/PB_BIOTOP.dbf", 
        options = "ENCODING=WINDOWS-1250"
      ) 
    vmb_x_dbf_akt <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_AKTUALNI/Biotop/X_biotop.dbf", 
        options = "ENCODING=WINDOWS-1250"
      )
    
    # Zpracování PB a X biotopů
    vmb_pb_x_dbf_akt <-
      dplyr::bind_rows(
        vmb_pb_dbf_akt,
        vmb_x_dbf_akt
      ) %>%
      dplyr::distinct()
    
    vmb_pb_x_akt <- 
      dplyr::inner_join(
        vmb_shp_sjtsk_akt_read, 
        vmb_pb_x_dbf_akt,
        by = "SEGMENT_ID"
      )
    
    # Příprava HAB a PB tabulek pro join a výpočet hodnocení
    vmb_hab_pb_dbf_akt <- 
      dplyr::bind_rows(
        vmb_hab_dbf_akt,
        vmb_pb_dbf_akt %>%
          dplyr::filter(
            !OBJECTID %in% vmb_hab_dbf_akt$OBJECTID
          )
      ) %>%
      dplyr::group_by(
        SEGMENT_ID
      ) %>%
      dplyr::mutate(
        moz_num = dplyr::n(),
        FSB_EVAL_prep = dplyr::case_when(
          sum(STEJ_PR, na.rm = TRUE) < 50 ~ "X",
          sum(STEJ_PR, na.rm = TRUE) >= 50 & sum(STEJ_PR, na.rm = TRUE) < 200 ~ "moz.",
          sum(STEJ_PR, na.rm = TRUE) == 200 ~ NA_character_
        )
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::select(
        SEGMENT_ID,
        FSB_EVAL_prep
      ) %>%
      dplyr::distinct()
    
    # Finální spojení do shapefilu
    vmb_shp_sjtsk_akt <- 
      vmb_shp_sjtsk_akt_read %>%
      dplyr::left_join(
        ., 
        vmb_hab_dbf_akt, 
        by = "SEGMENT_ID"
      ) %>%
      dplyr::left_join(
        ., 
        vmb_hab_pb_dbf_akt, 
        by = "SEGMENT_ID"
      ) %>%
      dplyr::mutate(
        FSB_EVAL = dplyr::case_when(
          FSB_EVAL_prep == "X" ~ "X",
          TRUE ~ FSB
        ),
        HABITAT = dplyr::case_when(
          HABITAT == 6210 & HABIT_TYP == "p" ~ "6210p",
          TRUE ~ HABITAT),
        REGION_ID = REGION_ID.x
      ) %>%
      dplyr::rename(
        DATUM = DATUM.x
      )
    
    paseky_23 <- utils::read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/paseky_results_20220927.csv")
    
    # Uložení do výstupu
    output <- list(
      vmb_shp_sjtsk_akt = vmb_shp_sjtsk_akt,
      vmb_pb_x_akt = vmb_pb_x_akt,
      paseky = paseky_23
    )
    
    # -------------------------------------------------------------------------#
    # VMBX = 1: Základní mapování (VMB1)
    # -------------------------------------------------------------------------#
  } else if(vmb_x == 1) {
    
    vmb_shp_sjtsk_orig_read <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_20060501/20060501_Segment.shp", 
        options = "ENCODING=WINDOWS-1250"
      )
    vmb_hab_dbf_orig <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_20060501/Biotop/HAB20060501_BIOTOP.dbf", 
        options = "ENCODING=WINDOWS-1250"
      )
    vmb_pb_dbf_orig <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_20060501/Biotop/PB20060501_BIOTOP.dbf", 
        options = "ENCODING=WINDOWS-1250"
      ) %>%
      dplyr::filter(
        !OBJECTID %in% vmb_hab_dbf_orig$OBJECTID
      )
    
    vmb_hab_pb_dbf_orig <- 
      dplyr::bind_rows(
        vmb_hab_dbf_orig, 
        vmb_pb_dbf_orig
      ) %>%
      dplyr::group_by(
        SEGMENT_ID
      ) %>%
      dplyr::mutate(
        moz_num = dplyr::n(),
        FSB_EVAL_prep = dplyr::case_when(
          sum(STEJ_PR, na.rm = TRUE) < 50 ~ "X",
          sum(STEJ_PR, na.rm = TRUE) >= 50 & sum(STEJ_PR, na.rm = TRUE) < 200 ~ "moz.",
          sum(STEJ_PR, na.rm = TRUE) == 200 ~ NA_character_)
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::select(
        SEGMENT_ID,
        FSB_EVAL_prep
      ) %>%
      dplyr::distinct()
    
    vmb_shp_sjtsk_orig <- 
      vmb_shp_sjtsk_orig_read %>%
      dplyr::left_join(
        vmb_hab_dbf_orig, 
        by = "SEGMENT_ID"
      ) %>%
      dplyr::left_join(
        vmb_hab_pb_dbf_orig,
        by = "SEGMENT_ID"
      ) %>%
      dplyr::mutate(
        FSB_EVAL = dplyr::case_when(
          FSB_EVAL_prep == "X" ~ "X",
          TRUE ~ FSB
        ),
        HABITAT = dplyr::case_when(
          HABITAT == 6210 & HABIT_TYP == "p" ~ "6210p",
          TRUE ~ HABITAT
        )
      )
    
    output <- list(
      vmb_shp_sjtsk_orig = vmb_shp_sjtsk_orig
    )
    
    # -------------------------------------------------------------------------#
    # VMBX = 2: Aktualizace 1 (VMBa1)
    # -------------------------------------------------------------------------#
  } else if(vmb_x == 2) {
    
    vmb_shp_sjtsk_a1_read <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_Aktualizace1/Aktualizace1_Segment.shp", 
        options = "ENCODING=WINDOWS-1250"
      )
    
    vmb_hab_dbf_a1 <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_Aktualizace1/Biotop/Aktualizace1_Hab_biotop.dbf", 
        options = "ENCODING=WINDOWS-1250"
      )
    
    vmb_pb_dbf_a1 <-
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_Aktualizace1/Biotop/Aktualizace1_Biotop.dbf",
        options = "ENCODING=WINDOWS-1250"
      )
    
    vmb_x_dbf_a1 <-
      vmb_pb_dbf_a1 %>%
      dplyr::filter(
        BIOTOP == "X"
      )
    
    # Spojení PB a X (Opraveno: definice chybějící proměnné)
    vmb_pb_x_dbf_a1 <- 
      dplyr::bind_rows(
        vmb_pb_dbf_a1,
        vmb_x_dbf_a1
      ) %>% 
      dplyr::distinct()
    
    vmb_pb_x_a1 <- 
      dplyr::inner_join(
        vmb_shp_sjtsk_a1_read, 
        vmb_pb_x_dbf_a1,
        by = "SEGMENT_ID"
      )
    
    vmb_hab_pb_dbf_a1 <- 
      dplyr::bind_rows(
        vmb_hab_dbf_a1, 
        vmb_pb_dbf_a1 %>%
          dplyr::filter(
            !OBJECTID_1 %in% vmb_hab_dbf_a1$OBJECTID_1
          )
      ) %>%
      dplyr::group_by(SEGMENT_ID
      ) %>%
      dplyr::mutate(
        moz_num = dplyr::n(),
        FSB_EVAL_prep = dplyr::case_when(
          sum(STEJ_PR, na.rm = TRUE) < 50 ~ "X",
          sum(STEJ_PR, na.rm = TRUE) >= 50 & sum(STEJ_PR, na.rm = TRUE) < 200 ~ "moz.",
          sum(STEJ_PR, na.rm = TRUE) == 200 ~ NA_character_
        )
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::select(
        SEGMENT_ID,
        FSB_EVAL_prep
      ) %>%
      dplyr::distinct()
    
    vmb_shp_sjtsk_a1 <- 
      vmb_shp_sjtsk_a1_read %>%
      dplyr::left_join(
        vmb_hab_dbf_a1, 
        by = "SEGMENT_ID"
      ) %>%
      dplyr::left_join(
        vmb_hab_pb_dbf_a1, 
        by = "SEGMENT_ID"
      ) %>%
      dplyr::mutate(
        FSB_EVAL = dplyr::case_when(
          FSB_EVAL_prep == "X" ~ "X",
          TRUE ~ FSB
        ),
        HABITAT = dplyr::case_when(
          HABITAT == 6210 & HABIT_TYP == "p" ~ "6210p",
          TRUE ~ HABITAT
        )
      )
    
    paseky_a1 <- utils::read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/paseky_a1_results_20240814.csv")
    
    output <- list(
      vmb_shp_sjtsk_a1 = vmb_shp_sjtsk_a1,
      vmb_pb_x_a1 = vmb_pb_x_a1,
      paseky_a1 = paseky_a1
    )
    
  } 
  
  # -------------------------------------------------------------------------#
  # Čištění paměti (Cleanup)
  # -------------------------------------------------------------------------#
  if(vmb_x == 1 & clean == TRUE) {
    
    rm(
      vmb_shp_sjtsk_orig_read, 
      vmb_hab_dbf_orig, 
      vmb_pb_dbf_orig,
      vmb_hab_pb_dbf_orig
    )
    
  } else if(vmb_x == 2 & clean == TRUE) {
    
    rm(
      vmb_shp_sjtsk_a1_read, 
      vmb_hab_dbf_a1, 
      vmb_pb_dbf_a1, 
      vmb_hab_pb_dbf_a1,
      vmb_x_dbf_a1,      # Přidáno pro úplnost
      vmb_pb_x_dbf_a1    # Přidáno pro úplnost
    )
    
  } else if(vmb_x == 0 & clean == TRUE) {
    
    # Opravený rm() bez zdvojené čárky
    rm(
      vmb_shp_sjtsk_akt_read, 
      vmb_hab_dbf_akt, 
      vmb_pb_dbf_akt, 
      vmb_hab_pb_dbf_akt,
      vmb_x_dbf_akt,
      vmb_pb_x_dbf_akt
    )
    
  }
  
  return(output)
}

# DEV ----
#--------------------------------------------------#
## Load habitat layer -----
#--------------------------------------------------#
library(tools)
rndop::isop_login()

library(httr)
library(tools)

# Your login info (replace with your actual username and password)
username <- "your_username"
password <- "your_password"

# Login URL (adjust if needed)
login_url <- "https://cas.nature.cz/cas/login"

# Log in and get cookies
res_login <- POST(login_url, body = list(
  username = username,
  password = password
), encode = "form")

# Extract cookies from login response
login_cookies <- cookies(res_login)

# Base URL for ZIP files
url_base <- "https://data.nature.cz/ds/21/download/kraj/"

regions <- c(
  "Jihocesky.zip", 
  "Jihomoravsky.zip", 
  "Karlovarsky.zip",
  "Kralovehradecky.zip", 
  "Liberecky.zip", 
  "Moravskoslezsky.zip",
  "Olomoucky.zip", 
  "Pardubicky.zip",
  "Plzensky.zip", 
  "Praha.zip",
  "Stredocesky.zip", 
  "Ustecky.zip", 
  "Vysocina.zip", 
  "Zlinsky.zip"
)

# Destination directory
dest_dir <- "Data/Temp/Habitats"
if (!dir.exists(dest_dir)) dir.create(dest_dir)

for (region in regions) {
  file_url <- paste0(url_base, region)
  dest_file <- file.path(dest_dir, region)
  unzip_dir <- file.path(dest_dir, tools::file_path_sans_ext(region))
  if (!dir.exists(unzip_dir)) dir.create(unzip_dir)
  
  cat("Downloading:", region, "...\n")
  
  # Download with cookies
  res <- GET(
    file_url, 
    set_cookies(
      .cookies = setNames(
        login_cookies$value, 
        login_cookies$name
      )
    ),
    write_disk(
      dest_file, 
      overwrite = TRUE
    )
  )
  
  if (status_code(res) == 200) {
    cat("Downloaded", region, "\nUnzipping...\n")
    unzip(dest_file, exdir = unzip_dir)
    cat("Done\n")
  } else {
    cat("Failed to download", region, "- HTTP status:", status_code(res), "\n")
  }
}

#----------------------------------------------------------#
# End config -----
#----------------------------------------------------------#
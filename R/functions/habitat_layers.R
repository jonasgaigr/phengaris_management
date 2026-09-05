#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: national habitat mapping layer (VMB)
#
#----------------------------------------------------------#
#
# load_vmb() reads the AOPK habitat mapping layer from the internal network
# share //bali.nature.cz. It is moved here unchanged from the old config.R so
# that config no longer mixes a large function definition with data loading.
#
#   vmb_x = 0   current layer (CR_AKTUALNI)      <- used by step 05
#   vmb_x = 1   original mapping (CR_20060501)
#   vmb_x = 2   first update (CR_Aktualizace1)
#
# Requires access to the AOPK network. Step 05 checks for that and skips the
# intersection with a recorded reason when the share is unreachable.
#
#----------------------------------------------------------#

#' Is the habitat mapping network share reachable?
vmb_available <- function(path = "//bali.nature.cz/du/Mapovani/Biotopy") {
  isTRUE(dir.exists(path))
}

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
    
    # Side table on clearings. It lives on a personal network folder and is not
    # used anywhere downstream, so a missing file must not abort the habitat
    # step.
    paseky_23 <- tryCatch(
      utils::read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/paseky_results_20220927.csv"),
      error = function(e) {
        warning("paseky_results_20220927.csv unavailable: ",
                conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    
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
    
    paseky_a1 <- tryCatch(
      utils::read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/paseky_a1_results_20240814.csv"),
      error = function(e) {
        warning("paseky_a1_results_20240814.csv unavailable: ",
                conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    
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

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

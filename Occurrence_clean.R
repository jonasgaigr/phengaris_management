#--------------------------------------------------#
## Load data with imputed negative records -----
#--------------------------------------------------#

data_new_with_imputed <- 
  readr::read_csv(
    data_new_with_imputed,
    "Data/Processed/data_new_with_imputed.csv"
    )

#--------------------------------------------------#
## Filter only targeted monitoring -----
#--------------------------------------------------#
data_new_source <-
  data_new_with_imputed %>%
  dplyr::filter(
    ZDROJ %in% target_mon_zdroj
  )

#--------------------------------------------------#
## Sites with both species monitored -----
#--------------------------------------------------#
phengaris_both_monitored <- 
  data_new_source %>%
  sf::st_drop_geometry() %>% # drop geometry
  dplyr::group_by(
    NAZ_LOKAL, 
    DATUM_OD
  ) %>%
  summarise(ID_NALEZ = unique(ID_NALEZ),
            SPEC_NUM = length(unique(DRUH))) %>%
  dplyr::filter(SPEC_NUM == 2)

#--------------------------------------------------#
## Sites with both species present -----
#--------------------------------------------------#
phengaris_both_present <- 
  data_new_source %>%
  sf::st_drop_geometry() %>% # drop geometry
  dplyr::filter(
    NEGATIV == 0 # drop negative occurrence records
    ) %>%
  dplyr::group_by(
    NAZ_LOKAL, 
    DATUM_OD
    ) %>%
  summarise(ID_NALEZ = unique(ID_NALEZ),
            SPEC_NUM = length(unique(DRUH))) %>%
  dplyr::filter(SPEC_NUM == 2)

data <- 
  data_new_source %>%
  dplyr::filter(
    ZDROJ %in% target_mon_zdroj
    ) %>%
  dplyr::mutate(
    YEAR = as.factor(
      substr(
        DATUM_OD, 
        1, 
        4
        )
      ),
    EVL = dplyr::case_when(
      ID_LOKAL %in% phengaris_evl_id ~ 1,
      TRUE ~ 0
      ),
    EVL_target = dplyr::case_when(
      # N2K site protected for Pnau
      DRUH == "Phengaris nausithous" &
        ID_LOKAL %in% evl_id_Pnau ~ 1,
      # N2K site protected for Ptel
      DRUH == "Phengaris teleius" &
        ID_LOKAL %in% evl_id_Ptel ~ 1,
      TRUE ~ 0
      ),
    EVL_comb = dplyr::case_when(
      EVL_target == 1 ~ 1,
      EVL == 1 &
        EVL_target == 0 
      ~ 0.5,
      TRUE ~ 0
      ),
    MZCHU = dplyr::case_when(
      ID_LOKAL %in% phengaris_mzchu_id ~ 1,
      TRUE ~ 0
      ),
    PROTECT = dplyr::case_when(
      EVL == 1 
      | MZCHU == 1 
      ~ 1,
      TRUE ~ 0
      ),
    PLANT_QUANT = dplyr::case_when(
      grepl(
        "dominantně",
        STRUKT_POZN
        ) 
      ~ 3,
      grepl(
        "hojně",
        STRUKT_POZN
        ) 
      ~ 2,
      grepl(
        "jednotlivě",
        STRUKT_POZN
        ) 
      ~ 1,
      grepl(
        "žádné",
        STRUKT_POZN
        ) 
      ~ 0
      ),
    PLANT_QUAL = dplyr::case_when(
      PLANT_QUANT %in% c(1, 2, 3) ~ 1,
      PLANT_QUANT == 0 ~ 0
      ),
    TTP = dplyr::case_when(
      grepl(
        "TTP s pravidelným managementem",
        POP_BIOT
        ) 
      ~ 1,
      is.na(POP_BIOT) == FALSE ~ 0
      ),
    PRIKOP = dplyr::case_when(
      grepl(
        "příkop u komunikace",
        POP_BIOT
        ) 
      ~ 1,
      is.na(POP_BIOT) == FALSE 
      ~ 0
      ),
    ZARUST = dplyr::case_when(
      grepl(
        "zarůstající louka bez managementu",
        POP_BIOT
        )
      ~ 1,
      is.na(POP_BIOT) == FALSE ~ 0
      ),
    JINY = dplyr::case_when(
      grepl(
        "jiný",
        POP_BIOT
        )
      ~ 1,
      is.na(POP_BIOT) == FALSE ~ 0
      ),
    LandUseChange = dplyr::case_when(
      grepl(
        "změna zemědělského využívání půdy a terénní úpravy",
        STRUKT_POZN
      ) ~ 1,
      TRUE ~ 0
    ),
    Abandonment = dplyr::case_when(
      grepl(
        "absence či nedostatek péče",
        STRUKT_POZN
      ) ~ 1,
      TRUE ~ 0
    ),
    HarmfulMow = dplyr::case_when(
      grepl(
        "nevhodná seč", 
        STRUKT_POZN
      ) ~ 1,
      TRUE ~ 0
    ),
    HarmfulGrazing = dplyr::case_when(
      grepl(
        "nevhodná pastva",
        STRUKT_POZN
      ) ~ 1,
      TRUE ~ 0
    ),
    GrazingByeffects = dplyr::case_when(
      grepl(
        "dopady chovu dobytka mimo pastvu",
        STRUKT_POZN
      ) ~ 1,
      TRUE ~ 0
    ),
    FertilizerUse = dplyr::case_when(
      grepl(
        "aplikace hnojiv",
        STRUKT_POZN
      ) ~ 1,
      TRUE ~ 0
    ),
    Afforestation = dplyr::case_when(
      grepl(
        "zalesňování bezlesí",
        STRUKT_POZN
      ) ~ 1,
      TRUE ~ 0
    ),
    Invasives = dplyr::case_when(
      grepl(
        "invazní druhy",
        STRUKT_POZN
      ) ~ 1,
      TRUE ~ 0
    ),
    NativeDominants = dplyr::case_when(
      grepl(
        "expanzní druhy",
        STRUKT_POZN
      ) ~ 1,
      TRUE ~ 0
    ),
    AbioticNaturalProcesses = dplyr::case_when(
      grepl(
        "abiotické přírodní procesy",
        STRUKT_POZN
      ) ~ 1,
      TRUE ~ 0
    ),
    Encroachment = dplyr::case_when(
      grepl(
        "sukcese",
        STRUKT_POZN
      ) ~ 1,
      TRUE ~ 0
    ),
    BiomassAccumulation = dplyr::case_when(
      grepl(
        "hromadění organického materiálu",
        STRUKT_POZN
      ) ~ 1,
      TRUE ~ 0
    ),
    Eutrophization = dplyr::case_when(
      grepl(
        "eutrofizace či okyselování",
        STRUKT_POZN
      ) ~ 1,
      TRUE ~ 0
    ),
    None = dplyr::case_when(
      grepl(
        "žádné",
        STRUKT_POZN
      ) ~ 1,
      TRUE ~ 0
    ),
    METHOD = dplyr::case_when(
      grepl(
        "Seč celoplošná: ne",
        STRUKT_POZN
      ) ~ 1,
      grepl(
        "Seč celoplošná: ano",
        STRUKT_POZN
      ) ~ 0
    ),
    TIMING = dplyr::case_when(
      grepl(
        "Seč vhodně načasovaná: ano",
        STRUKT_POZN
      ) ~ 1,
      grepl(
        "Seč vhodně načasovaná: ne",
        STRUKT_POZN
      ) ~ 0
    ),
    MOW = dplyr::case_when(
      grepl(
        "Seč",
        STRUKT_POZN
      ) ~ 1,
      TRUE ~ 0
    ),
    GRAZE = dplyr::case_when(
      grepl(
        "Pastva:",
        STRUKT_POZN
        ) 
      ~ 1,
      TRUE ~ 0
      ),
    GRAZE_MET = dplyr::case_when(
      grepl(
        "Pastva: extenzivní",
        STRUKT_POZN
        ) 
      ~ 1,
      grepl(
        "Pastva: intenzivní",
        STRUKT_POZN
        ) 
      ~ 0
      ),
    MANAGEMENT_HET = dplyr::case_when(
      is.na(METHOD) == FALSE &
        is.na(GRAZE) == FALSE 
      ~ 1,
      TRUE ~ 0
      ),
    OVERALL = dplyr::case_when(
      METHOD == 1 
      & TIMING == 1 
      ~ 1,
      TRUE ~ 0
      ),
    SPEC_NUM = dplyr::case_when(
      ID_NALEZ %in% phengaris_both ~ 1,
      TRUE ~ 0
      ),
    SITMAP = as.factor(SITMAP)
  ) %>%
  dplyr::rowwise() %>%
  mutate(
    SUM_THREATS = sum(
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
      None, 
      na.rm = TRUE
      ),
    HET_INN = sum(
      TTP, 
      PRIKOP, 
      ZARUST, 
      JINY, 
      na.rm = TRUE
      )
    ) %>%
  dplyr::filter(
    is.na(PLANT_QUAL) == FALSE &
      PLANT_QUAL != 0 & 
      #(is.na(METHOD) == FALSE | is.na(GRAZE)) &
      !YEAR %in% c(
        "2014", 
        "2017"
        )
    )


#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: national habitat mapping layer (VMB)
#
#----------------------------------------------------------#
#
# NOT VALIDATED ON REAL DATA: the habitat input contract and the segment
# rules below were tested only on a small synthetic fixture
# (tests/testthat/test-habitat.R).
#
# Where the habitat layer comes from, in order of preference (step 05):
#
#   1. the file named in config.yml `habitat: layer`, a GeoPackage that must
#      satisfy the input contract checked by read_habitat_layer();
#   2. the AOPK network share (config.yml `habitat: network_share`), read by
#      load_vmb(), which only works inside the AOPK network:
#        vmb_x = 0   current layer (CR_AKTUALNI)      <- used by step 05
#        vmb_x = 1   original mapping (CR_20060501)
#        vmb_x = 2   first update (CR_Aktualizace1)
#   3. the frozen site x segment intersection of the original analysis.
#
# How a site is described by the segments it overlaps is chosen in step 06
# by select_habitat_segment(), with a rule set per specification.
#
#----------------------------------------------------------#

#--------------------------------------------------#
## The habitat input contract -----
#--------------------------------------------------#

#' Read the habitat layer and check it against the input contract.
#'
#' @param path  GeoPackage of habitat segments, config.yml `habitat: layer`
#' @return the layer as an sf object, in the configured CRS
read_habitat_layer <- function(path = CONFIG$habitat$layer) {
  if (!file.exists(path)) {
    stop("Habitat layer not found: ", path, call. = FALSE)
  }
  x <- sf::st_read(path, quiet = TRUE)
  missing <- setdiff(CONFIG$habitat$required_columns, names(x))
  if (length(missing)) {
    stop(
      "The habitat layer ", path, " lacks the required column(s): ",
      paste(missing, collapse = ", "), " (config.yml habitat: required_columns).",
      call. = FALSE
    )
  }
  if (is.na(sf::st_crs(x))) {
    stop("The habitat layer ", path, " has no CRS; expected EPSG:", CONFIG$habitat$crs_epsg, call. = FALSE)
  }
  if (sf::st_crs(x) != sf::st_crs(CONFIG$habitat$crs_epsg)) {
    stop(
      "The habitat layer ", path, " is not in EPSG:", CONFIG$habitat$crs_epsg,
      " (config.yml habitat: crs_epsg).",
      call. = FALSE
    )
  }
  x
}

#' Is the habitat mapping network share reachable?
vmb_available <- function(path = CONFIG$habitat$network_share) {
  isTRUE(dir.exists(path))
}

#--------------------------------------------------#
## Intersection of the sites with the habitat segments -----
#--------------------------------------------------#

#' Intersect the site geometries with the habitat segments.
#'
#' Point and line sites have no area of their own. When a buffer width is
#' given they are buffered by it first, so that their overlap with each
#' segment has an area; when it is NULL they are intersected as they are.
#'
#' @param sites           site geometries with ID_LOKAL (sf)
#' @param habitats        habitat segments (sf)
#' @param point_buffer_m  buffer width for point sites, or NULL
#' @param line_buffer_m   buffer width for line sites, or NULL
#' @return one row per site x segment overlap, with AREA_real and LENGTH_real
#'         (m2 and m) of the overlap
intersect_sites_with_habitats <- function(sites, habitats,
                                          point_buffer_m = CONFIG$habitat$point_buffer_m,
                                          line_buffer_m = CONFIG$habitat$line_buffer_m) {
  type <- as.character(sf::st_geometry_type(sites))
  geometry <- sf::st_geometry(sites)
  if (!is.null(point_buffer_m)) {
    is_point <- type %in% c("POINT", "MULTIPOINT")
    geometry[is_point] <- sf::st_buffer(geometry[is_point], point_buffer_m)
  }
  if (!is.null(line_buffer_m)) {
    is_line <- type %in% c("LINESTRING", "MULTILINESTRING")
    geometry[is_line] <- sf::st_buffer(geometry[is_line], line_buffer_m)
  }
  sf::st_geometry(sites) <- geometry

  overlaps <- sf::st_intersection(
    sites,
    sf::st_transform(habitats, sf::st_crs(sites))
  )
  overlaps$AREA_real <- units::drop_units(sf::st_area(overlaps))
  overlaps$LENGTH_real <- units::drop_units(sf::st_length(overlaps))
  overlaps
}

#' Read the site x segment overlaps written by step 05.
#'
#' The file is semicolon-separated with a decimal comma, as the original
#' intersection was written.
read_habitat_overlaps <- function(path) {
  readr::read_csv2(path, show_col_types = FALSE, progress = FALSE) %>%
    dplyr::transmute(
      ID_LOKAL,
      SEGMENT_ID,
      BIOTOP      = as.character(BIOTOP),
      BIOTOP_SEZ  = as.character(BIOTOP_SEZ),
      FSB         = as.character(FSB),
      FSB_EVAL    = as.character(FSB_EVAL),
      HABITAT     = as.character(HABITAT),
      STEJ_PR,
      SHAPE_Area,
      AREA_real,
      LENGTH_real,
      REPRE,
      ZACH
    )
}

#--------------------------------------------------#
## Describing each site by its habitat segments -----
#--------------------------------------------------#

#' Is a habitat code a natural habitat?
#'
#' X codes are non-natural habitats; "-1" and NA mean no code.
is_natural_habitat <- function(biotop) {
  !is.na(biotop) & biotop != "-1" & !grepl("X", biotop)
}

#' Attach habitat attributes to the records, one value per record.
#'
#' Two rules:
#'
#'   legacy         the original rule. Each record keeps one overlapping
#'                  segment: the first after sorting by NATURAL and then by
#'                  overlap area, both ascending, i.e. the least natural and
#'                  smallest. AREA_SITE is that one overlap's area.
#'   area_weighted  NOT VALIDATED ON REAL DATA. Each site is summarised over
#'                  all its overlaps: AREA_SITE is the total overlap area,
#'                  NATURAL_SHARE the share of it in natural habitats,
#'                  SANGUISORBA_AREA the area in the habitat codes of
#'                  `sanguisorba_codes` (NA when that list is empty), and
#'                  BIOTOP / FSB / HABITAT those of the dominant habitat by
#'                  area. With `share_column`, each overlap's area is first
#'                  multiplied by that column / 100.
#'
#' @param records   the cleaned records, with ID_LOKAL
#' @param segments  site x segment overlaps, from read_habitat_overlaps()
#' @param rule      "legacy" or "area_weighted"
#' @return `records` with the habitat columns added, one row per record in the
#'         original order
select_habitat_segment <- function(records, segments, rule = c("legacy", "area_weighted"),
                                   sanguisorba_codes = unlist(CONFIG$habitat$sanguisorba_meadow_codes),
                                   share_column = CONFIG$habitat$share_column) {
  rule <- match.arg(rule)
  if (rule == "legacy") {
    out <- records %>%
      dplyr::mutate(
        .row_id = dplyr::row_number()
      ) %>%
      dplyr::left_join(
        segments,
        by = "ID_LOKAL",
        relationship = "many-to-many"
      ) %>%
      dplyr::mutate(
        # Area of the site x habitat segment overlap.
        AREA_SITE = AREA_real,
        # Between-habitat heterogeneity: habitats listed in the segment mosaic.
        HET_OUT = stringr::str_count(
          BIOTOP_SEZ,
          "\\("
        ),
        NATURAL = dplyr::case_when(
          BIOTOP != "-1" &
            grepl("X", BIOTOP) == FALSE &
            is.na(BIOTOP) == FALSE ~ 1,
          TRUE ~ 0
        )
      ) %>%
      dplyr::arrange(
        NATURAL,
        AREA_SITE
      ) %>%
      dplyr::group_by(
        .row_id
      ) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.row_id) %>%
      dplyr::select(-.row_id)
    stopifnot(nrow(out) == nrow(records))
    return(out)
  }

  # Area-weighted summaries per site.
  weighted <- segments %>%
    dplyr::mutate(
      .area = AREA_real * if (is.null(share_column)) 1 else .data[[share_column]] / 100,
      .natural = is_natural_habitat(BIOTOP)
    )

  dominant <- weighted %>%
    dplyr::group_by(ID_LOKAL, BIOTOP, FSB, HABITAT) %>%
    dplyr::summarise(.area = sum(.area, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(ID_LOKAL, dplyr::desc(.area), BIOTOP) %>%
    dplyr::group_by(ID_LOKAL) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(ID_LOKAL, BIOTOP, FSB, HABITAT)

  per_site <- weighted %>%
    dplyr::group_by(ID_LOKAL) %>%
    dplyr::summarise(
      AREA_SITE = sum(.area, na.rm = TRUE),
      NATURAL_SHARE = if (sum(.area, na.rm = TRUE) > 0) {
        sum(.area[.natural], na.rm = TRUE) / sum(.area, na.rm = TRUE)
      } else {
        NA_real_
      },
      SANGUISORBA_AREA = if (length(sanguisorba_codes)) {
        sum(.area[BIOTOP %in% sanguisorba_codes], na.rm = TRUE)
      } else {
        NA_real_
      },
      N_HABITATS = dplyr::n_distinct(BIOTOP[.natural]),
      .groups = "drop"
    ) %>%
    dplyr::left_join(dominant, by = "ID_LOKAL") %>%
    dplyr::mutate(
      NATURAL = as.numeric(is_natural_habitat(BIOTOP))
    )

  out <- records %>%
    dplyr::left_join(per_site, by = "ID_LOKAL")
  stopifnot(nrow(out) == nrow(records))
  out
}

#--------------------------------------------------#
## Optional loader for the AOPK network share -----
#--------------------------------------------------#

#' Read the habitat mapping layer from the AOPK network share.
#'
#' Moved here unchanged from the original config.R, apart from the location
#' of the share, which now comes from config.yml.
load_vmb <- function(vmb_x = 1, clean = TRUE, root = CONFIG$habitat$network_share) {
  
  # Inicializace výstupního listu
  output <- list()
  
  # -------------------------------------------------------------------------#
  # VMBX = 0: Aktuální vrstva
  # -------------------------------------------------------------------------#
  if(vmb_x == 0) {
    
    # Načtení dat
    vmb_shp_sjtsk_akt_read <- 
      sf::st_read(
        paste0(root, "/CR_AKTUALNI/Aktualni_Segment.shp"), 
        options = "ENCODING=WINDOWS-1250"
      )
    vmb_hab_dbf_akt <- 
      sf::st_read(
        paste0(root, "/CR_AKTUALNI/Biotop/HAB_BIOTOP.dbf"), 
        options = "ENCODING=WINDOWS-1250"
      )
    vmb_pb_dbf_akt <- 
      sf::st_read(
        paste0(root, "/CR_AKTUALNI/Biotop/PB_BIOTOP.dbf"), 
        options = "ENCODING=WINDOWS-1250"
      ) 
    vmb_x_dbf_akt <- 
      sf::st_read(
        paste0(root, "/CR_AKTUALNI/Biotop/X_biotop.dbf"), 
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
    
    # Uložení do výstupu. The side table on clearings that used to be read
    # here from a personal folder (S:/) was used nowhere and is dropped.
    output <- list(
      vmb_shp_sjtsk_akt = vmb_shp_sjtsk_akt,
      vmb_pb_x_akt = vmb_pb_x_akt
    )
    
    # -------------------------------------------------------------------------#
    # VMBX = 1: Základní mapování (VMB1)
    # -------------------------------------------------------------------------#
  } else if(vmb_x == 1) {
    
    vmb_shp_sjtsk_orig_read <- 
      sf::st_read(
        paste0(root, "/CR_20060501/20060501_Segment.shp"), 
        options = "ENCODING=WINDOWS-1250"
      )
    vmb_hab_dbf_orig <- 
      sf::st_read(
        paste0(root, "/CR_20060501/Biotop/HAB20060501_BIOTOP.dbf"), 
        options = "ENCODING=WINDOWS-1250"
      )
    vmb_pb_dbf_orig <- 
      sf::st_read(
        paste0(root, "/CR_20060501/Biotop/PB20060501_BIOTOP.dbf"), 
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
        paste0(root, "/CR_Aktualizace1/Aktualizace1_Segment.shp"), 
        options = "ENCODING=WINDOWS-1250"
      )
    
    vmb_hab_dbf_a1 <- 
      sf::st_read(
        paste0(root, "/CR_Aktualizace1/Biotop/Aktualizace1_Hab_biotop.dbf"), 
        options = "ENCODING=WINDOWS-1250"
      )
    
    vmb_pb_dbf_a1 <-
      sf::st_read(
        paste0(root, "/CR_Aktualizace1/Biotop/Aktualizace1_Biotop.dbf"),
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
    
    output <- list(
      vmb_shp_sjtsk_a1 = vmb_shp_sjtsk_a1,
      vmb_pb_x_a1 = vmb_pb_x_a1
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

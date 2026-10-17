#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: habitat mapping (VMB) and ecosystem (KVES) layers
#
#----------------------------------------------------------#
#
# The habitat input contract and the segment rules below are tested on small
# synthetic fixtures (tests/testthat/test-habitat.R). On real data, step 05
# checks the current intersection against the frozen one
# (05_current_vs_frozen).
#
# Step 05 writes up to two site x segment intersections, and step 06 uses the
# one a specification asks for (config.yml `habitat_source`,
# choose_habitat_overlaps()):
#
#   frozen   the site x segment intersection of the original analysis
#   current  the site areas intersected with the current VMB layer, taken from
#            1. the file named in config.yml `habitat: layer`, a GeoPackage
#               made by R/tools/download_habitat_layers.R that must satisfy
#               the input contract checked by read_habitat_layer();
#            2. else the AOPK network share (`habitat: network_share`), read
#               by load_vmb(), which only works inside the AOPK network:
#                 vmb_x = 0   current layer (CR_AKTUALNI)   <- used by step 05
#                 vmb_x = 1   original mapping (CR_20060501)
#                 vmb_x = 2   first update (CR_Aktualizace1)
#
# and the site areas intersected with the KVES ecosystem layer
# (`habitat: kves_layers`, read_kves_layer()). With the current layers a
# record takes its KVES information from the KVES version of its year
# (kves_version_of()), and a point or line site is described over the KVES
# polygon it lies in (site_areas()). Both intersections are therefore keyed
# by ID_LOKAL and KVES_VERSION.
#
# How a site is described by the segments it overlaps is chosen in step 06
# by select_habitat_segment(), with a rule set per specification, and by
# its dominant KVES class, summarise_kves().
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
#' Point and line sites have no area of their own. Step 05 gives them the
#' KVES polygon they lie in (site_areas()); alternatively, when a buffer
#' width is given they are buffered by it first. When it is NULL they are
#' intersected as they are.
#'
#' @param sites           site geometries with ID_LOKAL (sf)
#' @param habitats        habitat segments (sf)
#' @param point_buffer_m  buffer width for point sites, or NULL
#' @param line_buffer_m   buffer width for line sites, or NULL
#' @return one row per site x segment overlap, with AREA_real and LENGTH_real
#'         (m2 and m) of the overlap
intersect_sites_with_habitats <- function(sites, habitats, point_buffer_m = NULL, line_buffer_m = NULL) {
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

  # The attributes are those of the whole site or segment, which saying so
  # tells sf, silencing its warning about them.
  habitats <- sf::st_transform(habitats, sf::st_crs(sites))
  sf::st_agr(sites) <- "constant"
  sf::st_agr(habitats) <- "constant"
  overlaps <- sf::st_intersection(sites, habitats)
  overlaps$AREA_real <- units::drop_units(sf::st_area(overlaps))
  overlaps$LENGTH_real <- units::drop_units(sf::st_length(overlaps))
  overlaps
}

#' Read the site x segment overlaps written by step 05.
#'
#' The file is semicolon-separated with a decimal comma, as the original
#' intersection was written. The current intersection also has
#' KVES_VERSION, the KVES version of the site area, and AREA_SOURCE.
read_habitat_overlaps <- function(path) {
  readr::read_csv2(path, show_col_types = FALSE, progress = FALSE) %>%
    dplyr::transmute(
      ID_LOKAL,
      dplyr::across(dplyr::any_of(c("KVES_VERSION", "AREA_SOURCE")), as.character),
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

#' Is a habitat code an X habitat?
#'
#' X habitats are mapped but not natural (arable land, intensive meadows,
#' ruderal vegetation, ...). They can still hold the species, so they are
#' summarised on their own (X_SHARE), not lumped with the unmapped area.
is_x_habitat <- function(biotop) {
  !is.na(biotop) & biotop != "-1" & grepl("X", biotop)
}

#' Attach habitat attributes to the records, one value per record.
#'
#' Two rules:
#'
#'   legacy         the original rule. Each record keeps one overlapping
#'                  segment: the first after sorting by NATURAL and then by
#'                  overlap area, both ascending, i.e. the least natural and
#'                  smallest. AREA_SITE is that one overlap's area.
#'   area_weighted  Each site is summarised over all its overlaps:
#'                  AREA_SITE is the total overlap area; NATURAL_SHARE and
#'                  X_SHARE the shares of it in natural and in X habitats
#'                  (the rest is unmapped); SANGUISORBA_AREA and
#'                  SANGUISORBA_SHARE the area and share in the habitat codes
#'                  of `sanguisorba_codes` (NA when that list is empty);
#'                  WET_SHARE the share in the wet meadows of `wet_codes`,
#'                  a proxy for the habitat of the Myrmica host ants (P4.6);
#'                  and
#'                  BIOTOP / FSB / HABITAT / NATURAL those of the dominant
#'                  habitat by area (for a site without area, of the one
#'                  habitat it lies in; NA in a mosaic). With
#'                  `share_column`, each overlap's area is first multiplied
#'                  by that column / 100.
#'
#' @param records       the cleaned records, with the `by` columns
#' @param segments      site x segment overlaps, from read_habitat_overlaps()
#' @param rule          "legacy" or "area_weighted"
#' @param share_column  NULL, or the column holding each habitat's share of
#'                      its segment: config.yml `habitat: share_column` for
#'                      the current layer; NULL for the frozen intersection
#' @param by            the columns identifying a site area: ID_LOKAL, and
#'                      KVES_VERSION for the current intersection
#' @return `records` with the habitat columns added, one row per record in the
#'         original order
select_habitat_segment <- function(records, segments, rule = c("legacy", "area_weighted"),
                                   sanguisorba_codes = unlist(CONFIG$habitat$sanguisorba_meadow_codes),
                                   wet_codes = unlist(CONFIG$habitat$wet_meadow_codes),
                                   share_column = NULL, by = "ID_LOKAL") {
  rule <- match.arg(rule)
  if (rule == "legacy") {
    out <- records %>%
      dplyr::mutate(
        .row_id = dplyr::row_number()
      ) %>%
      dplyr::left_join(
        segments,
        by = by,
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

  # A site without area (a point or a line, unbuffered) has no dominant
  # habitat by area: it gets the habitat it lies in when that is a single
  # one, and NA (BIOTOP, FSB, HABITAT and NATURAL) when it lies in a mosaic
  # of several. A segment without a habitat code (BIOTOP NA, "-1") counts as
  # a non-natural habitat, as in the legacy rule.
  dominant <- weighted %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by)), BIOTOP, FSB, HABITAT) %>%
    dplyr::summarise(.area = sum(.area, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::mutate(
      .ambiguous = sum(.area) == 0 & dplyr::n_distinct(BIOTOP) > 1
    ) %>%
    dplyr::arrange(dplyr::desc(.area), BIOTOP, .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      NATURAL = dplyr::if_else(.ambiguous, NA_real_, as.numeric(is_natural_habitat(BIOTOP))),
      dplyr::across(c(BIOTOP, FSB, HABITAT), ~ dplyr::if_else(.ambiguous, NA, .x))
    ) %>%
    dplyr::select(dplyr::all_of(by), BIOTOP, FSB, HABITAT, NATURAL)

  # A share of a site without area is NA, never 0.
  share_of <- function(area, total) if (total > 0) area / total else NA_real_
  per_site <- weighted %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarise(
      AREA_SITE = sum(.area, na.rm = TRUE),
      NATURAL_SHARE = share_of(sum(.area[.natural], na.rm = TRUE), AREA_SITE),
      X_SHARE = share_of(sum(.area[is_x_habitat(BIOTOP)], na.rm = TRUE), AREA_SITE),
      SANGUISORBA_AREA = if (length(sanguisorba_codes)) {
        sum(.area[BIOTOP %in% sanguisorba_codes], na.rm = TRUE)
      } else {
        NA_real_
      },
      SANGUISORBA_SHARE = if (length(sanguisorba_codes)) share_of(SANGUISORBA_AREA, AREA_SITE) else NA_real_,
      WET_SHARE = if (length(wet_codes)) {
        share_of(sum(.area[BIOTOP %in% wet_codes], na.rm = TRUE), AREA_SITE)
      } else {
        NA_real_
      },
      N_HABITATS = dplyr::n_distinct(BIOTOP[.natural]),
      .groups = "drop"
    ) %>%
    dplyr::left_join(dominant, by = by)

  out <- records %>%
    dplyr::left_join(per_site, by = by)
  stopifnot(nrow(out) == nrow(records))
  out
}

#--------------------------------------------------#
## Which intersection a specification uses -----
#--------------------------------------------------#

#' The site x segment overlaps a specification uses.
#'
#' @param source   config.yml `habitat_source` of the specification: "frozen"
#'                 (the intersection of the original analysis) or "current"
#'                 (the current VMB layer)
#' @param current  the current intersection written by step 05
#' @param frozen   the frozen intersection written by step 05
#' @return list(path = the file to read, NA when there is none; source = the
#'         intersection it holds, "none" when there is none). "current" falls
#'         back to the frozen intersection when there is no current one.
choose_habitat_overlaps <- function(source = c("frozen", "current"), current, frozen) {
  source <- match.arg(source)
  if (source == "current" && file.exists(current)) {
    return(list(path = current, source = "current"))
  }
  if (file.exists(frozen)) {
    return(list(path = frozen, source = "frozen"))
  }
  list(path = NA_character_, source = "none")
}

#--------------------------------------------------#
## The KVES ecosystem layer -----
#--------------------------------------------------#

#' Read a KVES layer and check its columns and CRS.
#'
#' @param path  GeoPackage written by R/tools/download_habitat_layers.R, one
#'              of config.yml `habitat: kves_layers`
read_kves_layer <- function(path) {
  if (!file.exists(path)) {
    stop("KVES layer not found: ", path, call. = FALSE)
  }
  x <- sf::st_read(path, quiet = TRUE)
  missing <- setdiff(c("KVES_ID", "KOD", "KATEGORIE"), names(x))
  if (length(missing)) {
    stop("The KVES layer ", path, " lacks the column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (is.na(sf::st_crs(x)) || sf::st_crs(x) != sf::st_crs(CONFIG$habitat$crs_epsg)) {
    stop("The KVES layer ", path, " is not in EPSG:", CONFIG$habitat$crs_epsg, call. = FALSE)
  }
  x
}

#' Read the site x KVES overlaps written by step 05.
read_kves_overlaps <- function(path) {
  readr::read_csv2(path, show_col_types = FALSE, progress = FALSE) %>%
    dplyr::transmute(
      ID_LOKAL,
      dplyr::across(dplyr::any_of("KVES_VERSION"), as.character),
      KVES_ID = as.character(KVES_ID),
      KOD = as.character(KOD),
      KATEGORIE = as.character(KATEGORIE),
      AREA_real
    )
}

#' The KVES version a record takes its KVES information from, by its year.
#'
#' @param year      record years
#' @param versions  config.yml `habitat: kves_version_of_year`
#' @return the versions, e.g. "2021"
kves_version_of <- function(year, versions = CONFIG$habitat$kves_version_of_year) {
  out <- unname(unlist(versions)[as.character(year)])
  if (anyNA(out)) {
    stop(
      "No KVES version for the year(s) ", paste(sort(unique(year[is.na(out)])), collapse = ", "),
      " (config.yml habitat: kves_version_of_year).",
      call. = FALSE
    )
  }
  out
}

#' The area each site is described over.
#'
#' A polygon site is described over its own area. A point or line site has
#' none: it takes the KVES polygon(s) it lies in or crosses, merged and cut
#' to `margin_m` around the site, the extent of the local copies. A point or
#' line outside the KVES layer keeps its own geometry.
#'
#' @param sites     site geometries with ID_LOKAL (sf)
#' @param kves      a KVES layer (sf), or NULL for none
#' @param margin_m  distance from the site (m) the KVES polygon is cut to
#' @return `sites` with the geometry of each point and line site replaced,
#'         and AREA_SOURCE: "site" (a polygon site), "KVES" (the KVES polygon)
#'         or "none" (a point or line left as it is)
site_areas <- function(sites, kves, margin_m = CONFIG$habitat$margin_m) {
  type <- as.character(sf::st_geometry_type(sites))
  no_area <- which(type %in% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING"))
  sites$AREA_SOURCE <- "site"
  sites$AREA_SOURCE[no_area] <- "none"
  if (is.null(kves) || !length(no_area)) {
    return(sites)
  }

  geometry <- sf::st_geometry(sites)
  kves_geometry <- sf::st_transform(sf::st_geometry(kves), sf::st_crs(sites))
  hits <- sf::st_intersects(geometry[no_area], kves_geometry)
  areas <- lapply(seq_along(no_area), function(i) {
    if (!length(hits[[i]])) {
      return(NULL)
    }
    around <- sf::st_buffer(geometry[no_area[i]], margin_m)
    cut <- sf::st_intersection(sf::st_union(kves_geometry[hits[[i]]]), around)
    if (any(sf::st_geometry_type(cut) == "GEOMETRYCOLLECTION")) {
      cut <- sf::st_collection_extract(cut, "POLYGON", warn = FALSE)
    }
    if (!length(cut) || all(sf::st_is_empty(cut))) NULL else sf::st_union(cut)[[1]]
  })
  found <- !vapply(areas, is.null, logical(1))
  geometry[no_area[found]] <- sf::st_sfc(areas[found], crs = sf::st_crs(geometry))
  sf::st_geometry(sites) <- geometry
  sites$AREA_SOURCE[no_area[found]] <- "KVES"
  sites
}

#' The share of each polygon within `width` metres of its boundary.
#'
#' The Myrmica host ants are missing from the interior of large, uniformly
#' mown meadows and live along their edges (Oder 2026), so the share of a
#' site near its edge is a proxy for their habitat (P4.6). Only a site's own
#' polygon has a real edge: the KVES polygon of a point site is cut at 200 m,
#' and that cut is no edge, so it gets NA.
#'
#' @param areas  site_areas(): geometries with AREA_SOURCE
#' @param width  config.yml `habitat: edge_width_m`
#' @return the edge share of each row, NA unless AREA_SOURCE is "site"
edge_share <- function(areas, width = CONFIG$habitat$edge_width_m) {
  out <- rep(NA_real_, nrow(areas))
  own <- which(areas$AREA_SOURCE == "site")
  if (!length(own)) {
    return(out)
  }
  geometry <- sf::st_geometry(areas)[own]
  area <- units::drop_units(sf::st_area(geometry))
  core <- sf::st_buffer(geometry, -width)
  core_area <- units::drop_units(sf::st_area(core))
  core_area[sf::st_is_empty(core)] <- 0
  out[own] <- ifelse(area > 0, pmin(1, pmax(0, 1 - core_area / area)), NA_real_)
  out
}

#' Attach the dominant KVES ecosystem class of each record's site.
#'
#' KVES_KOD and KVES_KATEGORIE are the class covering most of the site. A
#' site without area gets the class it lies in, and NA when it lies on the
#' boundary between two classes. A site outside the layer gets NA. The
#' category name identifies the class: the KVES versions before 2025 have no
#' code for some categories.
#'
#' @param records   records (or sites) with the `by` columns
#' @param overlaps  site x KVES overlaps, from read_kves_overlaps()
#' @param by        the columns identifying a site area: ID_LOKAL, and
#'                  KVES_VERSION for the current intersection
#' @return `records` with KVES_KOD and KVES_KATEGORIE added, one row per
#'         record in the original order
summarise_kves <- function(records, overlaps, by = "ID_LOKAL") {
  dominant <- overlaps %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by)), KATEGORIE, KOD) %>%
    dplyr::summarise(.area = sum(AREA_real, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::mutate(.ambiguous = sum(.area) == 0 & dplyr::n_distinct(KATEGORIE) > 1) %>%
    dplyr::arrange(dplyr::desc(.area), KATEGORIE, .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      dplyr::across(dplyr::all_of(by)),
      KVES_KOD = dplyr::if_else(.ambiguous, NA_character_, KOD),
      KVES_KATEGORIE = dplyr::if_else(.ambiguous, NA_character_, KATEGORIE)
    )

  out <- records %>%
    dplyr::left_join(dominant, by = by)
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
  if (vmb_x == 0) {
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
          TRUE ~ HABITAT
        ),
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
  } else if (vmb_x == 1) {
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
          sum(STEJ_PR, na.rm = TRUE) == 200 ~ NA_character_
        )
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
  } else if (vmb_x == 2) {
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
      dplyr::group_by(SEGMENT_ID) %>%
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
  if (vmb_x == 1 && clean) {
    rm(
      vmb_shp_sjtsk_orig_read,
      vmb_hab_dbf_orig,
      vmb_pb_dbf_orig,
      vmb_hab_pb_dbf_orig
    )
  } else if (vmb_x == 2 && clean) {
    rm(
      vmb_shp_sjtsk_a1_read,
      vmb_hab_dbf_a1,
      vmb_pb_dbf_a1,
      vmb_hab_pb_dbf_a1,
      vmb_x_dbf_a1, # Přidáno pro úplnost
      vmb_pb_x_dbf_a1 # Přidáno pro úplnost
    )
  } else if (vmb_x == 0 && clean) {
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

  output
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

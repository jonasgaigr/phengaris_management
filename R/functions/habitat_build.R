#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: building the local habitat layers
#
#----------------------------------------------------------#
#
# R/tools/download_habitat_layers.R downloads the habitat mapping (VMB) and
# the ecosystem layer (KVES) from data.nature.cz, one archive per kraj, and
# keeps the features near the sites. The functions below put the kraje
# together into the two layers the cascade reads (config.yml `habitat: layer`
# and `habitat: kves_layer`). They touch neither the network nor the disk,
# so that they can be tested on small fixtures (tests/testthat).
#
# The archives are clipped at the kraj boundary. A VMB segment that crosses a
# kraj line is therefore split into pieces with the same SEGMENT_ID, and its
# habitat rows are repeated in the habitat table of both kraje.
#
#----------------------------------------------------------#

#--------------------------------------------------#
## Geometry -----
#--------------------------------------------------#

#' Give a layer the canonical S-JTSK CRS object.
#'
#' The shapefiles carry an ESRI WKT that is equivalent to EPSG:5514 but not
#' identical with it; one canonical CRS avoids spurious mismatches when the
#' layers are combined.
use_working_crs <- function(x, epsg = CONFIG$crs$epsg) {
  if (sf::st_crs(x) == sf::st_crs(epsg)) {
    sf::st_set_crs(sf::st_set_crs(x, NA), epsg)
  } else {
    sf::st_transform(x, epsg)
  }
}

#' Keep only the areal part of a layer, as MULTIPOLYGON.
#'
#' Repairing a geometry can leave points or lines where a polygon collapsed;
#' geometry collections are reduced to their polygons.
keep_polygons <- function(x) {
  x <- x[!sf::st_is_empty(x), ]
  is_collection <- sf::st_geometry_type(x) == "GEOMETRYCOLLECTION"
  if (any(is_collection)) {
    extracted <- sf::st_collection_extract(x[is_collection, ], "POLYGON", warn = FALSE)
    x <- rbind(x[!is_collection, ], extracted)
  }
  x <- x[sf::st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON"), ]
  sf::st_cast(x, "MULTIPOLYGON", warn = FALSE)
}

#' Repair the invalid geometries of a polygon layer, leaving the valid ones
#' untouched.
#'
#' @return list(layer = the repaired layer, repaired = number of geometries
#'         that were invalid)
make_valid_polygons <- function(x) {
  invalid <- !sf::st_is_valid(x)
  invalid[is.na(invalid)] <- TRUE
  if (any(invalid)) {
    geometry <- sf::st_geometry(x)
    geometry[invalid] <- sf::st_make_valid(geometry[invalid])
    sf::st_geometry(x) <- geometry
  }
  list(layer = keep_polygons(x), repaired = sum(invalid))
}

#' Merge the pieces of features that a kraj export cut at the kraj line.
#'
#' @param x       sf layer with one row per piece
#' @param id_col  the column identifying a feature
#' @return one row per feature, with the attributes of its first piece
dissolve_pieces <- function(x, id_col) {
  split_ids <- unique(x[[id_col]][duplicated(x[[id_col]])])
  if (!length(split_ids)) {
    return(x)
  }
  is_split <- x[[id_col]] %in% split_ids
  attributes <- sf::st_drop_geometry(x[is_split, ]) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(id_col)), .keep_all = TRUE)
  merged <- x[is_split, id_col] %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(id_col))) %>%
    dplyr::summarise(.groups = "drop") %>%
    sf::st_cast("MULTIPOLYGON", warn = FALSE) %>%
    dplyr::left_join(attributes, by = id_col)
  dplyr::bind_rows(x[!is_split, ], merged[names(x)])
}

#' Cut a layer to the area around the sites.
#'
#' The spatial filter of the download keeps whole features, and a forest or
#' an unmapped VMB segment can stretch for kilometres beyond the sites. Only
#' the part within the margin is ever intersected with a site, so the rest
#' is cut off. A feature wholly inside the area is left as it is; one outside
#' it is dropped. The attributes are kept, including areas worked out before
#' the cut (SHAPE_Area).
#'
#' @param x     sf polygon layer
#' @param area  the area to keep: disjoint polygons (sfc), such as the
#'              clusters of sites with their margin
#' @return `x` cut to `area`, its rows in their original order
clip_to_area <- function(x, area) {
  area <- sf::st_sf(geometry = area)
  x$.row <- seq_len(nrow(x))
  inside <- lengths(sf::st_within(x, area)) > 0
  crossing <- !inside & lengths(sf::st_intersects(x, area)) > 0
  if (!any(crossing)) {
    return(x[inside, setdiff(names(x), ".row")])
  }

  sf::st_agr(x) <- "constant"
  sf::st_agr(area) <- "constant"
  # The polygons of `area` are disjoint, so a feature crossing several of
  # them is cut into disjoint pieces, which are combined again.
  pieces <- sf::st_intersection(x[crossing, ".row"], area) %>%
    keep_polygons() %>%
    dplyr::group_by(.row) %>%
    dplyr::summarise(do_union = FALSE, .groups = "drop") %>%
    sf::st_cast("MULTIPOLYGON", warn = FALSE)
  cut <- sf::st_drop_geometry(x[x$.row %in% pieces$.row, ]) %>%
    dplyr::left_join(pieces, by = ".row") %>%
    sf::st_as_sf()

  dplyr::bind_rows(x[inside, ], cut) %>%
    dplyr::arrange(.row) %>%
    dplyr::select(-.row)
}

#--------------------------------------------------#
## The VMB layer of the habitat input contract -----
#--------------------------------------------------#

#' Put the VMB segments and habitat rows of the kraje together into the
#' habitat layer of the input contract (config.yml `habitat`).
#'
#' One row per segment x mapped habitat, as the contract asks:
#'
#'   SEGMENT_ID, FSB, BIOTOP_SEZ, ROK_AKT, DATUM   of the segment
#'   SHAPE_Area   area of the whole segment (m2), its pieces merged
#'   NUTS3        the kraje the segment lies in, comma-separated
#'   NATURAL_PCT  per cent of the segment in natural habitats (no X code,
#'                not "-1"); NA for a segment without habitat rows
#'   FSB_EVAL     "X" for a mapped segment (FSB not "-") less than half
#'                natural, otherwise FSB. This is the rule of load_vmb() on
#'                the network share, without two artefacts of its table
#'                joins there: a mosaic of X habitats only kept "moz.", and a
#'                natural habitat with a Natura 2000 record was counted
#'                twice. Step 05 counts the overlaps where the two differ
#'                (05_current_vs_frozen).
#'   B_ID, BIOTOP, STEJ_PR, REPRE, ZACH, KVALITA, DG, RB   of the habitat
#'   HABITAT      the Natura 2000 habitat code. Not in the published data,
#'                so always NA here.
#'
#' The habitat rows repeated across a kraj line are kept once. A segment
#' whose STEJ_PR do not add up to 100 is counted in `share_mismatch`.
#'
#' @param segments  VMB segment pieces of every kraj (sf), with SEGMENT_ID,
#'                  FSB, BIOTOP_SEZ, ROK_AKT, DATUM and NUTS3
#' @param biotopes  habitat rows of every kraj (data frame), with SEGMENT_ID,
#'                  B_ID, BIOTOP, STEJ_PR, REPRE, ZACH, KVALITA, DG and RB
#' @return list(layer = the contract layer (sf), segments = number of
#'         segments, split = number of segments put back together across a
#'         kraj line, share_mismatch = number of segments whose shares do not
#'         add up to 100)
build_habitat_segments <- function(segments, biotopes, unmapped_fsb = "-") {
  biotopes <- dplyr::distinct(tibble::as_tibble(biotopes))
  conflicting <- biotopes %>%
    dplyr::count(SEGMENT_ID, B_ID) %>%
    dplyr::filter(n > 1)
  if (nrow(conflicting)) {
    stop(
      nrow(conflicting), " VMB habitat row(s) differ between the kraje they ",
      "are repeated in (SEGMENT_ID ", conflicting$SEGMENT_ID[1], ", B_ID ",
      conflicting$B_ID[1], ", ...).",
      call. = FALSE
    )
  }

  kraje_of_segment <- sf::st_drop_geometry(segments) %>%
    dplyr::group_by(SEGMENT_ID) %>%
    dplyr::summarise(NUTS3 = paste(sort(unique(NUTS3)), collapse = ","), .groups = "drop")

  whole <- segments %>%
    dplyr::select(SEGMENT_ID, FSB, BIOTOP_SEZ, ROK_AKT, DATUM) %>%
    dissolve_pieces("SEGMENT_ID") %>%
    dplyr::left_join(kraje_of_segment, by = "SEGMENT_ID")
  whole$SHAPE_Area <- units::drop_units(sf::st_area(whole))

  per_segment <- biotopes %>%
    dplyr::group_by(SEGMENT_ID) %>%
    dplyr::summarise(
      NATURAL_PCT = sum(STEJ_PR[is_natural_habitat(BIOTOP)], na.rm = TRUE),
      .share_sum = sum(STEJ_PR, na.rm = TRUE),
      .groups = "drop"
    )

  whole <- whole %>%
    dplyr::left_join(per_segment, by = "SEGMENT_ID") %>%
    dplyr::mutate(
      FSB_EVAL = dplyr::if_else(
        !is.na(NATURAL_PCT) & FSB != unmapped_fsb & NATURAL_PCT < 50, "X", FSB
      )
    )
  share_mismatch <- sum(abs(whole$.share_sum - 100) > 1e-6, na.rm = TRUE)

  layer <- whole %>%
    dplyr::select(-.share_sum) %>%
    dplyr::left_join(
      biotopes %>%
        dplyr::filter(SEGMENT_ID %in% whole$SEGMENT_ID) %>%
        dplyr::select(SEGMENT_ID, B_ID, BIOTOP, STEJ_PR, REPRE, ZACH, KVALITA, DG, RB),
      by = "SEGMENT_ID",
      relationship = "one-to-many"
    ) %>%
    dplyr::mutate(HABITAT = NA_character_) %>%
    dplyr::arrange(SEGMENT_ID, B_ID) %>%
    dplyr::select(
      SEGMENT_ID, FSB, FSB_EVAL, BIOTOP_SEZ, ROK_AKT, DATUM, SHAPE_Area, NUTS3, NATURAL_PCT,
      B_ID, BIOTOP, STEJ_PR, HABITAT, REPRE, ZACH, KVALITA, DG, RB
    )

  list(
    layer = layer,
    segments = nrow(whole),
    split = length(unique(segments$SEGMENT_ID[duplicated(segments$SEGMENT_ID)])),
    share_mismatch = share_mismatch
  )
}

#--------------------------------------------------#
## The KVES layer -----
#--------------------------------------------------#

#' Put the KVES polygons of the kraje together.
#'
#' The feature ids are numbered per kraj file, so a polygon is identified by
#' its kraj and id. A polygon cut by a kraj line stays as two pieces: they do
#' not overlap, so areas can be summed over them.
#'
#' @param kves  KVES polygons of every kraj (sf), with kves_fid, KOD,
#'              KATEGORIE and NUTS3
#' @return sf with KVES_ID, KOD, KATEGORIE, NUTS3
build_kves_layer <- function(kves) {
  kves %>%
    dplyr::mutate(KVES_ID = paste0(NUTS3, ":", kves_fid)) %>%
    dplyr::select(KVES_ID, KOD, KATEGORIE, NUTS3)
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

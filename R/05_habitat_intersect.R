#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#     Step 05 - Intersect sites with the habitat and ecosystem layers
#
#
#----------------------------------------------------------#
#
# Overlays the monitoring sites on the national habitat mapping layer (VMB),
# so that each site can be described by the habitat segments it covers: the
# habitat code (BIOTOP) and its share of the segment (STEJ_PR), the habitat
# quality evaluation (FSB), the mosaic composition (BIOTOP_SEZ) and the area
# of the overlap. It also overlays them on the ecosystem layer KVES 2025,
# which covers the whole country, also where VMB maps no natural habitat.
#
# Each specification chooses its intersection in step 06 (config.yml,
# `habitat_source`). This step writes every one it can:
#
#   frozen   the intersection of the original analysis, copied as it is.
#            It cannot be recomputed and is used by the legacy specification.
#   current  the sites intersected with the current VMB layer: the local copy
#            of config.yml `habitat: layer`, made by
#            R/tools/download_habitat_layers.R, or else the AOPK network
#            share //bali.nature.cz (AOPK network only).
#   KVES     the sites intersected with the local copy of KVES 2025
#            (`habitat: kves_layer`), made by the same tool.
#
# A missing layer is skipped cleanly, and step 06 falls back as it describes.
#
# Reads:  Data/Processed/common/sites.gpkg   (step 01)
#         the frozen intersection, the VMB layer, the KVES layer
# Writes: Data/Processed/common/data_lokal_vmb.csv           frozen
#         Data/Processed/common/data_lokal_vmb_current.csv   current
#         Data/Processed/common/data_lokal_kves.csv          KVES
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("05")

report_start(
  "05",
  "Habitat layer intersection",
  paste(
    "Monitoring site geometries overlaid on the national habitat mapping",
    "layer (VMB) and on the ecosystem layer (KVES 2025). One row per site x",
    "habitat segment (or ecosystem polygon) overlap, with the real area and",
    "length of each overlap."
  )
)

sites <- read_sites()

path_frozen <- processed_path("data_lokal_vmb.csv", "common")
path_current <- processed_path("data_lokal_vmb_current.csv", "common")
path_kves <- processed_path("data_lokal_kves.csv", "common")

# A file left over from an earlier run must not be mistaken for a result.
for (path in c(path_frozen, path_current, path_kves)) {
  if (file.exists(path)) file.remove(path)
}

#----------------------------------------------------------#
# The frozen intersection -----
#----------------------------------------------------------#

# Frozen copy of the intersection the original analysis used. It cannot be
# regenerated without the habitat layer of that time, so it is kept as a
# tracked input.
if (file.exists(CONFIG$paths$habitat_legacy_intersection)) {
  file.copy(CONFIG$paths$habitat_legacy_intersection, path_frozen)
}

#----------------------------------------------------------#
# The current VMB layer -----
#----------------------------------------------------------#

current_source <- if (file.exists(CONFIG$habitat$layer)) {
  CONFIG$habitat$layer
} else if (vmb_available()) {
  "the AOPK network share"
} else {
  NA_character_
}

if (!is.na(current_source)) {
  habitat_layer <- if (file.exists(CONFIG$habitat$layer)) {
    read_habitat_layer(CONFIG$habitat$layer)
  } else {
    load_vmb(vmb_x = 0)$vmb_shp_sjtsk_akt
  }

  lokal_vmb <- sf::st_drop_geometry(intersect_sites_with_habitats(sites, habitat_layer))

  validate_table(
    lokal_vmb, "site x habitat overlaps",
    columns = c(ID_LOKAL = "numeric", AREA_real = "numeric", LENGTH_real = "numeric"),
    not_na = c("ID_LOKAL", "SEGMENT_ID", "AREA_real")
  )

  # Semicolon-separated with a decimal comma, as the original intersection.
  utils::write.csv2(lokal_vmb, path_current, row.names = FALSE)
}

#----------------------------------------------------------#
# KVES 2025 -----
#----------------------------------------------------------#

if (file.exists(CONFIG$habitat$kves_layer)) {
  lokal_kves <- sf::st_drop_geometry(intersect_sites_with_habitats(sites, read_kves_layer())) %>%
    dplyr::select(ID_LOKAL, KVES_ID, KOD, KATEGORIE, AREA_real, LENGTH_real)

  validate_table(
    lokal_kves, "site x KVES overlaps",
    columns = c(ID_LOKAL = "numeric", KOD = "character", AREA_real = "numeric"),
    not_na = c("ID_LOKAL", "KVES_ID", "KOD", "AREA_real")
  )

  utils::write.csv2(lokal_kves, path_kves, row.names = FALSE)
}

#----------------------------------------------------------#
# Results -----
#----------------------------------------------------------#

retrieved_path <- file.path(dirname(CONFIG$habitat$layer), "RETRIEVED.yml")
retrieved <- if (file.exists(retrieved_path)) yaml::read_yaml(retrieved_path) else list()

#' One line of the table of habitat sources.
source_row <- function(name, used_by, path, origin) {
  tbl <- if (file.exists(path)) readr::read_csv2(path, show_col_types = FALSE, progress = FALSE) else NULL
  data.frame(
    intersection = name,
    used_by = used_by,
    source = if (is.null(tbl)) "not available" else origin,
    overlaps = if (is.null(tbl)) NA_integer_ else nrow(tbl),
    sites = if (is.null(tbl)) NA_integer_ else dplyr::n_distinct(tbl$ID_LOKAL),
    stringsAsFactors = FALSE
  )
}
#' The specifications whose habitat_source is `source`.
specs_using <- function(source) {
  specs <- Filter(function(s) identical(spec_settings(s)$habitat_source, source), pipeline_specs())
  if (length(specs)) paste(specs, collapse = ", ") else "-"
}
#' Where a layer of RETRIEVED.yml came from, and when.
retrieved_note <- function(layer, path) {
  r <- retrieved[[layer]]
  if (is.null(r)) {
    return(path)
  }
  paste0(path, " (", r$source, ", retrieved ", r$retrieved, ")")
}

report_table(
  rbind(
    source_row(
      "frozen", specs_using("frozen"), path_frozen,
      CONFIG$paths$habitat_legacy_intersection
    ),
    source_row(
      "current", specs_using("current"), path_current,
      if (identical(current_source, CONFIG$habitat$layer)) {
        retrieved_note("habitat_segments", current_source)
      } else {
        current_source
      }
    ),
    source_row("KVES", "-", path_kves, retrieved_note("kves", CONFIG$habitat$kves_layer))
  ),
  "Site x habitat intersections written by this step, and the specifications using them",
  "05_habitat_sources"
)

if (!file.exists(path_current)) {
  report_warning(
    "No current VMB layer: neither", CONFIG$habitat$layer, "nor the AOPK",
    "network share is available. Run R/tools/download_habitat_layers.R to",
    "make the local copy. Until then the specifications using the current",
    "intersection fall back to the frozen one in step 06."
  )
}
if (!file.exists(path_frozen)) {
  report_warning(
    "The frozen intersection", CONFIG$paths$habitat_legacy_intersection,
    "is missing, so the specifications using it get no habitat attributes",
    "in step 06."
  )
}

#--------------------------------------------------#
## Extent of each VMB intersection -----
#--------------------------------------------------#

#' Number of overlaps and segments per site, and the area by FSB.
intersection_summary <- function(tbl) {
  per_site <- as.numeric(table(tbl$ID_LOKAL))
  data.frame(
    measure = c(
      "site x segment overlaps",
      "distinct sites covered",
      "median segments per site",
      "largest number of segments on one site"
    ),
    value = c(
      nrow(tbl), dplyr::n_distinct(tbl$ID_LOKAL),
      stats::median(per_site), max(per_site)
    ),
    stringsAsFactors = FALSE
  )
}

for (which in c("current", "frozen")) {
  path <- if (which == "current") path_current else path_frozen
  if (!file.exists(path)) next
  tbl <- readr::read_csv2(path, show_col_types = FALSE, progress = FALSE)
  # The current layer has one row per segment x mapped habitat, so its
  # overlaps are counted once per segment. The frozen table is counted as it
  # always was, once per row.
  if (which == "current") tbl <- dplyr::distinct(tbl, ID_LOKAL, SEGMENT_ID, FSB, AREA_real)

  report_section(paste("The", which, "VMB intersection"))
  report_table(
    intersection_summary(tbl),
    paste("Extent of the", which, "habitat intersection"),
    paste0("05_habitat_intersection_summary", if (which == "current") "_current" else "")
  )
  by_fsb <- tbl %>%
    dplyr::group_by(FSB) %>%
    dplyr::summarise(
      overlaps = dplyr::n(),
      area_ha = sum(AREA_real, na.rm = TRUE) / 10000,
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(overlaps))
  report_table(
    by_fsb,
    paste("Overlaps and area by habitat quality evaluation (FSB), the", which, "intersection"),
    paste0("05_habitat_by_fsb", if (which == "current") "_current" else "")
  )
}

#--------------------------------------------------#
## The current intersection against the frozen one -----
#--------------------------------------------------#

if (file.exists(path_current) && file.exists(path_frozen)) {
  report_section("The current VMB intersection against the frozen one")

  current <- readr::read_csv2(path_current, show_col_types = FALSE, progress = FALSE) %>%
    dplyr::distinct(ID_LOKAL, SEGMENT_ID, ROK_AKT, FSB, FSB_EVAL, BIOTOP_SEZ, AREA_real)
  frozen <- readr::read_csv2(path_frozen, show_col_types = FALSE, progress = FALSE) %>%
    dplyr::distinct(ID_LOKAL, SEGMENT_ID, ROK_AKT = ROK_AKT.x, FSB, FSB_EVAL, BIOTOP_SEZ, AREA_real)
  stopifnot(
    !anyDuplicated(current[c("ID_LOKAL", "SEGMENT_ID")]),
    !anyDuplicated(frozen[c("ID_LOKAL", "SEGMENT_ID")])
  )

  pairs <- dplyr::inner_join(
    frozen, current,
    by = c("ID_LOKAL", "SEGMENT_ID"), suffix = c("_frozen", "_current")
  )
  unchanged <- pairs %>% dplyr::filter(ROK_AKT_frozen == ROK_AKT_current)
  agree <- function(x, y) sum(x == y | (is.na(x) & is.na(y)), na.rm = TRUE)
  # Within 1 %, or 1 m2 for the overlaps of points and lines (area 0).
  same_area <- sum(
    abs(unchanged$AREA_real_current - unchanged$AREA_real_frozen) <= 0.01 * pmax(unchanged$AREA_real_frozen, 1)
  )
  share <- function(n, of) sprintf("%d of %d (%.1f %%)", n, of, 100 * n / of)

  report_table(
    data.frame(
      measure = c(
        "sites in the frozen intersection that are in the current one",
        "frozen site x segment overlaps found in the current intersection",
        "of these, segments not updated since (same ROK_AKT)",
        "unchanged segments: same FSB",
        "unchanged segments: same FSB_EVAL",
        "unchanged segments: same BIOTOP_SEZ",
        "unchanged segments: overlap area within 1 %"
      ),
      value = c(
        share(sum(unique(frozen$ID_LOKAL) %in% current$ID_LOKAL), dplyr::n_distinct(frozen$ID_LOKAL)),
        share(nrow(pairs), nrow(frozen)),
        share(nrow(unchanged), nrow(pairs)),
        share(agree(unchanged$FSB_frozen, unchanged$FSB_current), nrow(unchanged)),
        share(agree(unchanged$FSB_EVAL_frozen, unchanged$FSB_EVAL_current), nrow(unchanged)),
        share(agree(unchanged$BIOTOP_SEZ_frozen, unchanged$BIOTOP_SEZ_current), nrow(unchanged)),
        share(same_area, nrow(unchanged))
      ),
      stringsAsFactors = FALSE
    ),
    paste(
      "The current intersection checked against the frozen one. Where a",
      "segment has not been updated since, the two should agree. FSB_EVAL",
      "differs by design: the frozen one kept 'moz.' for mosaics of X",
      "habitats only, and for some mosaics whose natural part has a Natura",
      "2000 record, which the network-share tables count twice; the current",
      "one is X for every mapped segment less than half natural"
    ),
    "05_current_vs_frozen"
  )
}

#--------------------------------------------------#
## KVES classes at the sites -----
#--------------------------------------------------#

if (file.exists(path_kves)) {
  report_section("KVES 2025 ecosystem classes at the sites")

  kves_sites <- summarise_kves(
    data.frame(ID_LOKAL = unique(sites$ID_LOKAL)),
    read_kves_overlaps(path_kves)
  )
  report_table(
    kves_sites %>%
      dplyr::count(KVES_KOD, KVES_KATEGORIE, name = "sites") %>%
      dplyr::arrange(dplyr::desc(sites)),
    paste(
      "Sites by their dominant KVES class (by area; a point or line site by",
      "the class it lies in, NA on a boundary between classes or outside the layer)"
    ),
    "05_kves_dominant_class"
  )
}

report_finish()

step_end("05")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

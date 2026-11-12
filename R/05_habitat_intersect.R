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
# of the overlap. It also overlays them on the ecosystem layer KVES, which
# covers the whole country, also where VMB maps no natural habitat.
#
# Each specification chooses its intersection in step 06 (config.yml,
# `habitat_source`). This step writes every one it can:
#
#   frozen   the intersection of the original analysis, copied as it is.
#            It cannot be recomputed and is used by the legacy specification.
#   current  the site areas intersected with the current VMB layer: the local
#            copy of config.yml `habitat: layer`, made by
#            R/tools/download_habitat_layers.R, or else the AOPK network
#            share //bali.nature.cz (AOPK network only).
#   KVES     the site areas intersected with the local copies of KVES
#            (`habitat: kves_layers`), made by the same tool.
#
# The site areas. A record takes its KVES information from the KVES version
# of its year (`habitat: kves_version_of_year`), so each site is taken once
# per KVES version of its records' years. A polygon site is described over
# its own area. A point or line site has none, and is described over the KVES
# polygon it lies in, from that version, cut to `habitat: margin_m` around
# the site (site_areas()). Both current intersections are therefore keyed by
# ID_LOKAL and KVES_VERSION.
#
# A missing layer is skipped cleanly, and step 06 falls back as it describes.
#
# Reads:  Data/Processed/common/sites.gpkg, records.csv   (step 01)
#         the frozen intersection, the VMB layer, the KVES layers
# Writes: Data/Processed/common/data_lokal_vmb.csv           frozen
#         Data/Processed/common/data_lokal_vmb_current.csv   current
#         Data/Processed/common/data_lokal_kves.csv          KVES
#         Data/Processed/common/site_areas.gpkg              the site areas
#         Data/Processed/common/site_area_shape.csv          their area and edge share
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("05")

report_start(
  "05",
  "Habitat layer intersection",
  paste(
    "Monitoring site geometries overlaid on the national habitat mapping",
    "layer (VMB) and on the ecosystem layer (KVES). One row per site x",
    "habitat segment (or ecosystem polygon) overlap, with the real area and",
    "length of each overlap."
  )
)

sites <- read_sites()

path_frozen <- processed_path("data_lokal_vmb.csv", "common")
path_current <- processed_path("data_lokal_vmb_current.csv", "common")
path_kves <- processed_path("data_lokal_kves.csv", "common")
path_areas <- processed_path("site_areas.gpkg", "common")
path_shape <- processed_path("site_area_shape.csv", "common")

# A file left over from an earlier run must not be mistaken for a result.
for (path in c(path_frozen, path_current, path_kves, path_areas, path_shape)) {
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
# The site areas, per KVES version -----
#----------------------------------------------------------#

# The KVES versions of each site: those of its records' years.
site_versions <- read_records() %>%
  dplyr::transmute(
    ID_LOKAL,
    KVES_VERSION = kves_version_of(as.integer(substr(DATUM_OD, 1, 4)))
  ) %>%
  dplyr::distinct()
versions <- sort(unique(site_versions$KVES_VERSION))
kves_paths <- unlist(CONFIG$habitat$kves_layers)
kves_available <- versions[versions %in% names(kves_paths) & file.exists(kves_paths[versions])]

vmb_source <- if (file.exists(CONFIG$habitat$layer)) {
  CONFIG$habitat$layer
} else if (vmb_available()) {
  "the AOPK network share"
} else {
  NA_character_
}
habitat_layer <- if (is.na(vmb_source)) {
  NULL
} else if (file.exists(CONFIG$habitat$layer)) {
  read_habitat_layer(CONFIG$habitat$layer)
} else {
  load_vmb(vmb_x = 0)$vmb_shp_sjtsk_akt
}

areas <- lokal_vmb <- lokal_kves <- list()
for (version in versions) {
  kves <- if (version %in% kves_available) read_kves_layer(kves_paths[[version]]) else NULL
  version_sites <- site_versions$ID_LOKAL[site_versions$KVES_VERSION == version]
  version_areas <- site_areas(sites[sites$ID_LOKAL %in% version_sites, ], kves)
  version_areas$KVES_VERSION <- version
  areas[[version]] <- version_areas

  # The keys first, then the attributes of the layer.
  overlaps <- function(layer) {
    sf::st_drop_geometry(intersect_sites_with_habitats(version_areas, layer)) %>%
      dplyr::relocate(ID_LOKAL, KVES_VERSION, AREA_SOURCE)
  }
  if (!is.null(kves)) {
    lokal_kves[[version]] <- overlaps(kves) %>%
      dplyr::select(ID_LOKAL, KVES_VERSION, AREA_SOURCE, KVES_ID, KOD, KATEGORIE, AREA_real, LENGTH_real)
  }
  if (!is.null(habitat_layer)) lokal_vmb[[version]] <- overlaps(habitat_layer)
}
rm(habitat_layer)
areas <- dplyr::bind_rows(areas)
areas$AREA_m2 <- units::drop_units(sf::st_area(areas))
# The share of a polygon site near its edge, a proxy for the habitat of the
# host ants (P4.6).
areas$EDGE_SHARE <- edge_share(areas)
sf::st_write(areas, path_areas, quiet = TRUE)
utils::write.csv2(
  sf::st_drop_geometry(areas)[, c("ID_LOKAL", "KVES_VERSION", "AREA_SOURCE", "AREA_m2", "EDGE_SHARE")],
  path_shape,
  row.names = FALSE
)

#----------------------------------------------------------#
# The current VMB and KVES intersections -----
#----------------------------------------------------------#

if (length(lokal_vmb)) {
  lokal_vmb <- dplyr::bind_rows(lokal_vmb)
  validate_table(
    lokal_vmb, "site x habitat overlaps",
    columns = c(ID_LOKAL = "numeric", KVES_VERSION = "character", AREA_real = "numeric", LENGTH_real = "numeric"),
    not_na = c("ID_LOKAL", "KVES_VERSION", "SEGMENT_ID", "AREA_real")
  )
  # Semicolon-separated with a decimal comma, as the original intersection.
  utils::write.csv2(lokal_vmb, path_current, row.names = FALSE)
}

if (length(lokal_kves)) {
  lokal_kves <- dplyr::bind_rows(lokal_kves)
  validate_table(
    lokal_kves, "site x KVES overlaps",
    columns = c(ID_LOKAL = "numeric", KVES_VERSION = "character", KATEGORIE = "character", AREA_real = "numeric"),
    not_na = c("ID_LOKAL", "KVES_VERSION", "KVES_ID", "KATEGORIE", "AREA_real")
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

kves_origin <- paste(
  vapply(kves_available, function(v) retrieved_note(paste0("kves_", v), kves_paths[[v]]), character(1)),
  collapse = "; "
)
report_table(
  rbind(
    source_row(
      "frozen", specs_using("frozen"), path_frozen,
      CONFIG$paths$habitat_legacy_intersection
    ),
    source_row(
      "current", specs_using("current"), path_current,
      if (identical(vmb_source, CONFIG$habitat$layer)) {
        retrieved_note("habitat_segments", vmb_source)
      } else {
        vmb_source
      }
    ),
    source_row("KVES", specs_using("current"), path_kves, kves_origin)
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
missing_versions <- setdiff(versions, kves_available)
if (length(missing_versions)) {
  report_warning(
    "No local copy of KVES", paste(missing_versions, collapse = ", "),
    "(config.yml `habitat: kves_layers`), so the point and line sites of",
    "the records of those years are intersected as they are, with no area,",
    "and get no KVES class. Run R/tools/download_habitat_layers.R."
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
## The site areas -----
#--------------------------------------------------#

report_section("The site areas")

report_table(
  sf::st_drop_geometry(areas) %>%
    dplyr::mutate(GEOMETRY = as.character(sf::st_geometry_type(sites))[match(ID_LOKAL, sites$ID_LOKAL)]) %>%
    dplyr::group_by(KVES_VERSION, GEOMETRY, AREA_SOURCE) %>%
    dplyr::summarise(
      sites = dplyr::n(),
      median_area_ha = stats::median(AREA_m2) / 10000,
      median_edge_share = stats::median(EDGE_SHARE),
      .groups = "drop"
    ),
  paste(
    "Site areas by KVES version and site geometry. A polygon site is",
    "described over its own area (site); a point or line site over the KVES",
    "polygon it lies in, cut to", CONFIG$habitat$margin_m, "m around it",
    "(KVES), or as it is when it lies outside KVES (none)"
  ),
  "05_site_areas"
)

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
  # overlaps are counted once per segment. A site is counted once, for its
  # first KVES version. The frozen table is counted as it always was, once
  # per row.
  if (which == "current") {
    tbl <- tbl %>%
      dplyr::filter(KVES_VERSION == ave(KVES_VERSION, ID_LOKAL, FUN = min)) %>%
      dplyr::distinct(ID_LOKAL, SEGMENT_ID, FSB, AREA_real)
  }

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

  # Only the polygon sites: the frozen intersection took point and line sites
  # as they are, the current one over their KVES polygon.
  current <- readr::read_csv2(path_current, show_col_types = FALSE, progress = FALSE) %>%
    dplyr::filter(AREA_SOURCE == "site") %>%
    dplyr::distinct(ID_LOKAL, SEGMENT_ID, ROK_AKT, FSB, FSB_EVAL, BIOTOP_SEZ, AREA_real)
  frozen <- readr::read_csv2(path_frozen, show_col_types = FALSE, progress = FALSE) %>%
    dplyr::filter(ID_LOKAL %in% areas$ID_LOKAL[areas$AREA_SOURCE == "site"]) %>%
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
  # Within 1 %, or 1 m2 for the smallest overlaps.
  same_area <- sum(
    abs(unchanged$AREA_real_current - unchanged$AREA_real_frozen) <= 0.01 * pmax(unchanged$AREA_real_frozen, 1)
  )
  share <- function(n, of) sprintf("%d of %d (%.1f %%)", n, of, 100 * n / of)

  report_table(
    data.frame(
      measure = c(
        "polygon sites in the frozen intersection that are in the current one",
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
      "The current intersection checked against the frozen one, on the",
      "polygon sites. Where a segment has not been updated since, the two",
      "should agree. FSB_EVAL differs by design: the frozen one kept 'moz.'",
      "for mosaics of X habitats only, and for some mosaics whose natural",
      "part has a Natura 2000 record, which the network-share tables count",
      "twice; the current one is X for every mapped segment less than half",
      "natural"
    ),
    "05_current_vs_frozen"
  )
}

#--------------------------------------------------#
## The Natura 2000 habitat types from the habitat catalogue -----
#--------------------------------------------------#

# The published habitat mapping has no Natura 2000 code, so the current
# layer takes it from the habitat code by the habitat catalogue
# (natura_habitat_of()). The frozen intersection has the codes of the
# mapping itself, which is what the catalogue is checked against.
catalogue <- read_habitat_catalogue()
if (is.null(catalogue)) {
  report_warning(
    "No habitat catalogue (Data/Input/reference/habitat_catalogue.csv): the",
    "current habitat mapping has no Natura 2000 habitat code (HABITAT). Run",
    "refresh_reference_layers(layers = \"habitat_catalogue\")",
    "(R/tools/refresh_reference_layers.R)."
  )
} else if (file.exists(path_frozen)) {
  report_section("The Natura 2000 habitat types from the habitat catalogue")
  frozen_codes <- readr::read_csv2(path_frozen, show_col_types = FALSE, progress = FALSE) %>%
    dplyr::transmute(
      BIOTOP = as.character(BIOTOP),
      HABITAT = as.character(HABITAT),
      # The priority form of 6210 is a property of the stand, not of the code.
      mapped = sub("p$", "", HABITAT),
      derived = natura_habitat_of(BIOTOP, catalogue = catalogue)
    )
  with_code <- frozen_codes[!is.na(frozen_codes$HABITAT), ]
  report_table(
    data.frame(
      overlaps = c(
        "frozen overlaps with a Natura 2000 code of the mapping",
        "the catalogue gives the same code from the habitat code",
        "the catalogue gives another code",
        "the catalogue gives none",
        "of the mapped codes, the priority form of 6210 (the catalogue gives 6210)",
        "frozen overlaps without a Natura 2000 code for which the catalogue gives one"
      ),
      n = c(
        nrow(with_code),
        sum(with_code$derived == with_code$mapped, na.rm = TRUE),
        sum(with_code$derived != with_code$mapped, na.rm = TRUE),
        sum(is.na(with_code$derived)),
        sum(grepl("p$", with_code$HABITAT)),
        sum(is.na(frozen_codes$HABITAT) & !is.na(frozen_codes$derived))
      ),
      stringsAsFactors = FALSE
    ),
    paste(
      "The Natura 2000 habitat type derived from the habitat code by the",
      "habitat catalogue, against the codes of the frozen intersection"
    ),
    "05_natura_codes_from_catalogue"
  )
}

#--------------------------------------------------#
## KVES classes at the sites -----
#--------------------------------------------------#

if (file.exists(path_kves)) {
  report_section("KVES ecosystem classes at the sites")

  kves_sites <- summarise_kves(
    sf::st_drop_geometry(areas)[c("ID_LOKAL", "KVES_VERSION")],
    read_kves_overlaps(path_kves),
    by = c("ID_LOKAL", "KVES_VERSION")
  )
  report_table(
    kves_sites %>%
      dplyr::count(KVES_KOD, KVES_KATEGORIE, name = "site_areas") %>%
      dplyr::arrange(dplyr::desc(site_areas)),
    paste(
      "Site areas (site x KVES version) by their dominant KVES class, by",
      "area (NA: outside the layer, or a point or line left as it is). The",
      "versions before 2025 have no code for some classes"
    ),
    "05_kves_dominant_class"
  )
}

report_finish()

step_end("05")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

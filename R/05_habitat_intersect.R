#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#     Step 05 - Intersect sites with the habitat mapping layer
#
#
#----------------------------------------------------------#
#
# Overlays the monitoring sites on the national habitat mapping layer (VMB), so
# that each site can be described by the habitat segments it covers: the formal
# habitat code (BIOTOP), the habitat quality evaluation (FSB), the mosaic
# composition (BIOTOP_SEZ) and the area of the overlap.
#
# NOT VALIDATED ON REAL DATA: the habitat input contract (config.yml,
# `habitat`) was tested only on a synthetic fixture. The habitat layer is
# taken from the first of these that is available:
#
#   1. the GeoPackage named in config.yml `habitat: layer`;
#   2. the AOPK network share //bali.nature.cz (AOPK network only);
#   3. the frozen intersection of the original analysis, which is used as it
#      is, without re-intersecting;
#
# and when none is, the step skips cleanly: step 06 then leaves the habitat
# variables empty and the habitat models report themselves as skipped.
#
# Reads:  Data/Processed/common/sites.gpkg   (step 01)
#         the habitat layer, or the frozen intersection
# Writes: Data/Processed/common/data_lokal_vmb.csv
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("05")

report_start(
  "05",
  "Habitat layer intersection",
  paste(
    "Monitoring site geometries overlaid on the national habitat mapping",
    "layer. One row per site x habitat segment overlap, with the real area and",
    "length of each overlap."
  )
)

path_lokal_vmb_csv <- processed_path("data_lokal_vmb.csv", "common")

# Frozen copy of the intersection the original analysis used. It cannot be
# regenerated without the habitat layer, so it is kept as a tracked input.
path_lokal_vmb_legacy <- CONFIG$paths$habitat_legacy_intersection

#----------------------------------------------------------#
# Find a habitat layer and intersect -----
#----------------------------------------------------------#

habitat_source <- if (file.exists(CONFIG$habitat$layer)) {
  "contract"
} else if (vmb_available()) {
  "network"
} else if (file.exists(path_lokal_vmb_legacy)) {
  "frozen"
} else {
  "none"
}

if (habitat_source %in% c("contract", "network")) {

  habitat_layer <- if (habitat_source == "contract") {
    read_habitat_layer(CONFIG$habitat$layer)
  } else {
    load_vmb(vmb_x = 0)$vmb_shp_sjtsk_akt
  }

  lokal_vmb <- intersect_sites_with_habitats(read_sites(), habitat_layer)

  validate_table(
    sf::st_drop_geometry(lokal_vmb), "site x habitat overlaps",
    columns = c(ID_LOKAL = "numeric", AREA_real = "numeric", LENGTH_real = "numeric"),
    not_na = c("ID_LOKAL", "SEGMENT_ID", "AREA_real")
  )

  # Semicolon-separated with a decimal comma, as the original intersection.
  utils::write.csv2(
    lokal_vmb %>%
      sf::st_drop_geometry(),
    path_lokal_vmb_csv,
    row.names = FALSE
  )

  report_warning(
    "The habitat attributes come from",
    if (habitat_source == "contract") CONFIG$habitat$layer else "the AOPK network share",
    "and were intersected with the current site geometries. This path has",
    "not been validated on real data."
  )

} else if (habitat_source == "frozen") {

  file.copy(path_lokal_vmb_legacy, path_lokal_vmb_csv, overwrite = TRUE)

  report_warning(
    "No habitat layer was available (neither", CONFIG$habitat$layer,
    "nor the AOPK network share), so the intersection was not recomputed.",
    "The frozen intersection of the original analysis,",
    path_lokal_vmb_legacy, ", is used instead, so the habitat variables in",
    "step 06 may be out of date relative to the current site geometries."
  )
  message("  habitat layer unavailable - using the frozen intersection")

} else {

  # A file left over from an earlier run must not be mistaken for a result.
  if (file.exists(path_lokal_vmb_csv)) file.remove(path_lokal_vmb_csv)

  report_warning(
    "No habitat layer and no frozen intersection are available, so step 06",
    "will leave the habitat variables empty and the habitat models will be",
    "skipped."
  )
  message("  no habitat data - skipping")

}

habitat_intersection_available <- file.exists(path_lokal_vmb_csv)


#----------------------------------------------------------#
# Results -----
#----------------------------------------------------------#

if (habitat_intersection_available) {

  lokal_vmb_tbl <- readr::read_csv2(
    path_lokal_vmb_csv,
    show_col_types = FALSE
  )

  habitat_intersection_summary <- data.frame(
    measure = c(
      "site x segment overlaps",
      "distinct sites covered",
      "median segments per site",
      "largest number of segments on one site"
    ),
    value = c(
      nrow(lokal_vmb_tbl),
      dplyr::n_distinct(lokal_vmb_tbl$ID_LOKAL),
      stats::median(as.numeric(table(lokal_vmb_tbl$ID_LOKAL))),
      max(as.numeric(table(lokal_vmb_tbl$ID_LOKAL)))
    ),
    stringsAsFactors = FALSE
  )

  report_table(
    habitat_intersection_summary,
    "Extent of the habitat intersection",
    "05_habitat_intersection_summary"
  )

  habitat_by_fsb <-
    lokal_vmb_tbl %>%
    dplyr::group_by(FSB) %>%
    dplyr::summarise(
      overlaps  = dplyr::n(),
      area_ha   = sum(AREA_real, na.rm = TRUE) / 10000,
      .groups   = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(overlaps))

  report_table(
    habitat_by_fsb,
    "Overlaps and area by habitat quality evaluation (FSB)",
    "05_habitat_by_fsb"
  )

}

report_finish()

step_end("05")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

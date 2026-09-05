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
# The habitat layer lives on the AOPK network share //bali.nature.cz. When that
# share is unreachable the step is skipped, the previously written intersection
# is reused if present, and the reason is recorded in the report.
#
# Reads:  Data/Processed/lokal_new.gpkg   (step 01)
#         the VMB layer over the network
# Writes: Data/Processed/lokal_vmb.gpkg
#         Data/Processed/data_lokal_vmb.csv
#
#----------------------------------------------------------#

message("Step 05: intersecting sites with the habitat layer")

report_start(
  "05",
  "Habitat layer intersection",
  paste(
    "Monitoring site geometries overlaid on the national habitat mapping",
    "layer. One row per site x habitat segment overlap, with the real area and",
    "length of each overlap."
  )
)

path_lokal_vmb_gpkg <- file.path(PATHS$processed, "lokal_vmb.gpkg")
path_lokal_vmb_csv  <- file.path(PATHS$processed, "data_lokal_vmb.csv")

#----------------------------------------------------------#
# Run the intersection, if the habitat layer is reachable -----
#----------------------------------------------------------#

if (vmb_available()) {

  lokal_new <- sf::st_read(file.path(PATHS$processed, "lokal_new.gpkg"))

  habitat_layer       <- load_vmb(vmb_x = 0)
  habitat_layer_shape <- habitat_layer$vmb_shp_sjtsk_akt

  # Align the habitat layer with the site geometries before intersecting.
  habitat_layer_transformed <- sf::st_transform(
    habitat_layer_shape,
    crs = sf::st_crs(lokal_new)
  )

  lokal_vmb <-
    sf::st_intersection(
      lokal_new,
      habitat_layer_transformed
    ) %>%
    dplyr::mutate(
      AREA_real   = units::drop_units(sf::st_area(geom)),
      LENGTH_real = units::drop_units(sf::st_length(geom))
    )

  sf::st_write(
    lokal_vmb,
    path_lokal_vmb_gpkg,
    delete_dsn = TRUE
  )

  utils::write.csv2(
    lokal_vmb %>%
      sf::st_drop_geometry(),
    path_lokal_vmb_csv,
    row.names = FALSE
  )

  habitat_intersection_available <- TRUE

} else {

  habitat_intersection_available <- file.exists(path_lokal_vmb_csv)

  report_warning(
    "The habitat mapping share //bali.nature.cz was not reachable, so the",
    "intersection was not recomputed.",
    if (habitat_intersection_available) {
      "The previously written Data/Processed/data_lokal_vmb.csv is reused, so
       the habitat variables in step 06 may be out of date relative to the
       current site geometries."
    } else {
      "No previous intersection exists either, so step 06 will leave the
       habitat variables empty and the habitat models will be skipped."
    }
  )

  message("  habitat layer unavailable - skipping intersection")

}

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

message("Step 05 done")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

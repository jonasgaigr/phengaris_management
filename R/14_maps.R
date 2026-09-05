#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#                  Step 14 - Maps
#
#
#----------------------------------------------------------#
#
# Two maps for the manuscript:
#
#   * the monitored sites on a shaded relief background, coloured by whether
#     the species was found;
#   * the mapping grid, showing which fields were surveyed and which hold each
#     species.
#
# Needs the spatial objects that step 01 puts into the session
# (phengaris_lokal_new, czechia_border, sitmap), so it has to run in the same
# session as step 01.
#
# Reads:  Data/Processed/data_analysis.csv   (step 06) and the step 01 objects
# Writes: Outputs/Figures/14_*.png
#         Outputs/Reports/14_maps.md
#
#----------------------------------------------------------#

message("Step 14: drawing maps")

report_start(
  "14",
  "Maps",
  "Distribution of the monitored sites and of the two species across the national mapping grid."
)

spatial_ready <- all(vapply(
  c("phengaris_lokal_new", "czechia_border", "sitmap"),
  exists,
  logical(1)
))

if (!spatial_ready) {

  report_warning(
    "The spatial objects from step 01 are not in the session, so no maps were",
    "drawn. Run the cascade from step 01, or source R/01_load_source_data.R",
    "before this step."
  )
  message("  spatial objects missing - skipping maps")

} else {

  data <- read_data_analysis()

  #--------------------------------------------------#
  ## Monitored sites on shaded relief -----
  #--------------------------------------------------#

  hypso_read <- RCzechia::vyskopis(format = "rayshaded", cropped = FALSE)

  # The relief arrives in WGS84 while every other layer is in S-JTSK.
  # geom_raster() draws raw x and y rather than reprojecting, so without this
  # the raster and the vector layers occupy different coordinate ranges and the
  # country collapses into a corner of an otherwise empty plot.
  hypso_proj <- terra::project(hypso_read, paste0("EPSG:", CRS_SJTSK))

  # At full resolution this is about 21 million cells, far more than a figure
  # of a few hundred dpi can show.
  hypso_proj <- terra::aggregate(hypso_proj, fact = 3, fun = "mean", na.rm = TRUE)

  # Clip to the border. The source is a rectangle in WGS84, so reprojecting it
  # into the Krovak grid leaves a rotated rectangle of relief spilling well
  # past Czechia. Masking drops everything outside the country; cells outside
  # become NA and are removed with the data frame conversion below.
  czechia_vect <- terra::vect(czechia_border)
  hypso_proj <- terra::mask(
    terra::crop(hypso_proj, czechia_vect),
    czechia_vect
  )

  hypso_df <- as.data.frame(hypso_proj, xy = TRUE, na.rm = TRUE)
  # The band holds shading intensity, not metres above sea level.
  colnames(hypso_df) <- c("x", "y", "shade")

  data_map <- ggplot2::ggplot(
    data = phengaris_lokal_new %>%
      dplyr::filter(ZDROJ %in% target_mon_zdroj) %>%
      dplyr::mutate(geometry = sf::st_centroid(geometry))
  ) +
    ggplot2::geom_raster(
      data = hypso_df,
      ggplot2::aes(x = x, y = y, fill = shade)
    ) +
    # A hillshade is a backdrop; its values carry no units worth a legend.
    ggplot2::scale_fill_gradient(
      low = "grey35", high = "white", guide = "none"
    ) +
    ggplot2::geom_sf(
      ggplot2::aes(color = as.factor(NEGATIV)),
      size = 0.5,
      show.legend = FALSE
    ) +
    ggplot2::geom_sf(
      data = czechia_border,
      fill = NA
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.05))) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.05))) +
    ggplot2::theme_void()

  report_figure(
    data_map,
    "Monitored sites on shaded relief",
    "14_monitored_sites",
    width = 9, height = 6
  )

  #--------------------------------------------------#
  ## Species distribution across the mapping grid -----
  #--------------------------------------------------#

  # SITMAP in the occurrence data is the four-digit basic mapping field, while
  # POLE in the grid layer identifies a quadrant of it ("5845d"). Matching the
  # two directly returns nothing, so the basic field is taken from the first
  # four characters of POLE and each surveyed field is drawn as its four
  # quadrants.
  sitmap_fields <- sitmap %>%
    dplyr::mutate(
      POLE_BASE = substr(as.character(POLE), 1, 4)
    )

  field_codes <- function(x) unique(as.character(x))

  phenau_dist <- data %>%
    dplyr::filter(DRUH == SPECIES_NAU & POSITIVE == 1) %>%
    dplyr::pull(SITMAP) %>%
    field_codes()

  phetel_dist <- data %>%
    dplyr::filter(DRUH == SPECIES_TEL & POSITIVE == 1) %>%
    dplyr::pull(SITMAP) %>%
    field_codes()

  sample_dist <- data %>%
    dplyr::pull(SITMAP) %>%
    field_codes()

  # An unmatched join would produce a blank map rather than an error, so it is
  # checked explicitly.
  n_matched <- sum(sitmap_fields$POLE_BASE %in% sample_dist)
  if (n_matched == 0) {
    warning(
      "No mapping-grid quadrant matched a surveyed SITMAP code. ",
      "The grid map would be empty; check the POLE and SITMAP formats.",
      call. = FALSE
    )
  }

  data_dist_map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = czechia_border, fill = NA, linewidth = 1.5) +
    ggplot2::geom_sf(
      data = sitmap_fields %>%
        dplyr::filter(POLE_BASE %in% sample_dist),
      fill = "light grey"
    ) +
    ggplot2::geom_sf(
      data = sitmap_fields %>%
        dplyr::filter(POLE_BASE %in% phenau_dist),
      fill = "blue",
      alpha = .5
    ) +
    ggplot2::geom_sf(
      data = sitmap_fields %>%
        dplyr::filter(POLE_BASE %in% phetel_dist),
      fill = "red",
      alpha = .5
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.05))) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.05))) +
    ggplot2::theme_void()

  report_figure(
    data_dist_map,
    "Mapping fields surveyed (grey), with P. nausithous (blue) and P. teleius (red)",
    "14_species_distribution",
    width = 9, height = 6
  )

  report_table(
    data.frame(
      subset = c(
        "mapping fields surveyed",
        "mapping fields with P. nausithous",
        "mapping fields with P. teleius"
      ),
      fields = c(
        length(sample_dist),
        length(phenau_dist),
        length(phetel_dist)
      ),
      stringsAsFactors = FALSE
    ),
    "Mapping grid coverage",
    "14_grid_coverage"
  )

  report_note(paste(
    "The grid map previously referenced an object called `czechia`, which was",
    "never created, and printed an undefined `both_dist`. It now uses",
    "`czechia_border` from step 01, and the stray reference is gone."
  ))

  report_warning(
    "The relief map drew the shaded-relief raster without reprojecting it.",
    "The relief is delivered in WGS84 degrees while every other layer is in",
    "S-JTSK metres, and geom_raster() plots raw coordinates, so the two ended",
    "up in unrelated coordinate ranges: the raster never appeared and the",
    "country was squeezed into a corner. The raster is now projected to",
    "S-JTSK first. Its legend, which was labelled elevation but held shading",
    "intensity between 0 and 1, has been dropped. The relief is also masked to",
    "the border, because the source is a rectangle in WGS84 and reprojecting it",
    "left a rotated block of relief extending well outside Czechia."
  )

  report_warning(
    "The grid map also matched the four-digit SITMAP code in the occurrence",
    "data against POLE in the grid layer, which identifies a quadrant of a",
    "basic field and always carries a letter suffix. Nothing ever matched, so",
    "the map came out as an empty outline. It now matches on the first four",
    "characters of POLE, which resolves all", length(sample_dist),
    "surveyed fields to", n_matched, "quadrants."
  )

}

report_finish()

message("Step 14 done")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

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
  hypso_df <- as.data.frame(hypso_read, xy = TRUE)
  colnames(hypso_df) <- c("x", "y", "elevation")

  data_map <- ggplot2::ggplot(
    data = phengaris_lokal_new %>%
      dplyr::filter(ZDROJ %in% target_mon_zdroj) %>%
      dplyr::mutate(geometry = sf::st_centroid(geometry))
  ) +
    ggplot2::geom_raster(
      data = hypso_df,
      ggplot2::aes(x = x, y = y, fill = elevation)
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

  phenau_dist <- data %>%
    dplyr::filter(DRUH == SPECIES_NAU & POSITIVE == 1) %>%
    dplyr::pull(SITMAP) %>%
    unique()

  phetel_dist <- data %>%
    dplyr::filter(DRUH == SPECIES_TEL & POSITIVE == 1) %>%
    dplyr::pull(SITMAP) %>%
    unique()

  sample_dist <- data %>%
    dplyr::pull(SITMAP) %>%
    unique()

  data_dist_map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = czechia_border, fill = NA, linewidth = 1.5) +
    ggplot2::geom_sf(
      data = sitmap %>%
        dplyr::filter(POLE %in% sample_dist),
      fill = "light grey"
    ) +
    ggplot2::geom_sf(
      data = sitmap %>%
        dplyr::filter(POLE %in% phenau_dist),
      fill = "blue",
      alpha = .5
    ) +
    ggplot2::geom_sf(
      data = sitmap %>%
        dplyr::filter(POLE %in% phetel_dist),
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

}

report_finish()

message("Step 14 done")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

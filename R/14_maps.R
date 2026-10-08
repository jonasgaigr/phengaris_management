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
# Reads:  Data/Processed/<spec>/data_analysis.csv          (step 06)
#         Data/Processed/common/records.csv, sites.gpkg    (step 01)
#         Data/Input/reference/czechia_border, sitmap, relief_shaded
# Writes: Outputs/<spec>/Figures/14_*.png
#         Outputs/<spec>/Reports/14_maps.md
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("14")

report_start(
  "14",
  "Maps",
  "Distribution of the monitored sites and of the two species across the national mapping grid."
)

data <- read_data_analysis()
phengaris_lokal_new <- read_records_with_sites()

czechia_border <-
  read_reference_layer("czechia_border") %>%
  sf::st_transform(CRS_SJTSK)

sitmap <-
  read_reference_layer("sitmap") %>%
  sf::st_transform(CRS_SJTSK) %>%
  sf::st_crop(czechia_border)

#--------------------------------------------------#
## Monitored sites on shaded relief -----
#--------------------------------------------------#

# The shaded relief, already projected to S-JTSK, aggregated and clipped to
# the border by R/tools/refresh_reference_layers.R. geom_raster() draws raw
# x and y, so the relief has to be in the same CRS as the vector layers;
# cells outside the border are NA and dropped by the conversion below.
hypso_proj <- terra::rast(reference_path("relief_shaded.tif"))

hypso_df <- as.data.frame(hypso_proj, xy = TRUE, na.rm = TRUE)
# The band holds shading intensity, not metres above sea level.
colnames(hypso_df) <- c("x", "y", "shade")

# One point per targeted record, at the centroid of its site.
monitored_sites <- phengaris_lokal_new %>%
  dplyr::filter(ZDROJ %in% target_mon_zdroj)
sf::st_geometry(monitored_sites) <- sf::st_centroid(sf::st_geometry(monitored_sites))

data_map <- ggplot2::ggplot(data = monitored_sites) +
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

report_warning(
  "The grid map matches the four-digit SITMAP code in the occurrence data",
  "against the first four characters of POLE in the grid layer, which",
  "identifies a quadrant of a basic field. This resolves all",
  length(sample_dist), "surveyed fields to", n_matched, "quadrants."
)

report_finish()

step_end("14")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

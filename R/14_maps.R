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
#   * the quadrants of the mapping grid, showing which were surveyed and
#     which hold each species.
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
## The frame of both maps -----
#--------------------------------------------------#

# Both maps share one frame, one relief and the same scale bar and north
# arrow, so that they can stand side by side.
border_box <- sf::st_bbox(czechia_border)
frame_x <- border_box[c("xmin", "xmax")] + c(-1, 1) * 0.015 * diff(border_box[c("xmin", "xmax")])
frame_y <- border_box[c("ymin", "ymax")] + c(-1, 1) * 0.025 * diff(border_box[c("ymin", "ymax")])
map_width <- diff(frame_x)
map_height <- diff(frame_y)

# The scale bar and the north arrow sit in the empty corner south-west of
# Bohemia; the legends in the one north of Moravia.
map_frame <- function() {
  list(
    map_north_arrow(
      frame_x[[1]] + 0.04 * map_width, frame_y[[1]] + 0.17 * map_height,
      length = 0.075 * map_height, crs = sf::st_crs(CRS_SJTSK)
    ),
    map_scale_bar(frame_x[[1]] + 0.02 * map_width, frame_y[[1]] + 0.04 * map_height, length_km = 100),
    ggplot2::coord_sf(
      crs = sf::st_crs(CRS_SJTSK), xlim = frame_x, ylim = frame_y, expand = FALSE, datum = NA
    )
  )
}

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

# The elevation tints the relief. It is aggregated to the 300 m of the
# hillshade, which is all a page-wide map can show, and clipped to the
# border.
elevation_df <- terra::rast(reference_path("elevation.tif")) %>%
  terra::aggregate(fact = 3) %>%
  terra::mask(terra::vect(czechia_border)) %>%
  as.data.frame(xy = TRUE, na.rm = TRUE)
colnames(elevation_df) <- c("x", "y", "elevation")

# One point per targeted record, at the centroid of its site.
monitored_sites <- phengaris_lokal_new %>%
  dplyr::filter(ZDROJ %in% target_mon_zdroj)
sf::st_geometry(monitored_sites) <- sf::st_centroid(sf::st_geometry(monitored_sites))
# NEGATIV is 1 for a record without the species: the factor has the levels
# FALSE (negative) and TRUE (positive), in the order of the occupancy scale.
monitored_sites$positive <- factor(monitored_sites$NEGATIV == 0, levels = c(FALSE, TRUE))
records_per_class <- table(monitored_sites$positive)

#' The records of one class with a white halo, so that they stand apart
#' from the class drawn under them. The positive records are drawn last.
record_points <- function(class) {
  points <- monitored_sites[monitored_sites$positive == class, ]
  list(
    ggplot2::geom_sf(data = points, colour = "white", size = 1.05, show.legend = FALSE),
    ggplot2::geom_sf(data = points, ggplot2::aes(colour = positive), size = 0.6)
  )
}

data_map <- ggplot2::ggplot() +
  ggplot2::geom_raster(
    data = elevation_df,
    ggplot2::aes(x = x, y = y, fill = elevation)
  ) +
  map_relief(hypso_df) +
  ggplot2::geom_sf(
    data = czechia_border,
    fill = NA,
    colour = phengaris_col("spot_black"),
    linewidth = 0.35
  ) +
  record_points("FALSE") +
  record_points("TRUE") +
  ggplot2::scale_fill_gradientn(
    colours = ELEVATION_TINTS,
    values = scales::rescale(c(150, 400, 700, 1100)),
    limits = c(150, 1100), oob = scales::squish,
    breaks = c(200, 500, 800, 1100), labels = c("200", "500", "800", "1100+"),
    name = "Elevation (m)",
    guide = ggplot2::guide_colourbar(
      direction = "horizontal", title.position = "top", order = 2,
      barwidth = ggplot2::unit(3, "cm"), barheight = ggplot2::unit(0.22, "cm"),
      frame.colour = phengaris_col("spot_black"), frame.linewidth = 0.2,
      theme = ggplot2::theme(legend.ticks = ggplot2::element_blank())
    )
  ) +
  scale_colour_occupancy(
    name = "Records",
    labels = sprintf("%s (%s)", OCCUPANCY_LABELS, format(as.integer(records_per_class), big.mark = ","))
  ) +
  ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 1.8), order = 1)) +
  map_frame() +
  theme_phengaris_map(legend = c(0.975, 0.985))

report_figure(
  data_map,
  "Monitored sites on shaded relief, tinted by elevation",
  "14_monitored_sites",
  height = 11.2
)

#--------------------------------------------------#
## Species distribution across the mapping grid -----
#--------------------------------------------------#

# The grid layer holds the quadrants of the mapping grid (POLE, "5845d"), a
# quarter of a basic field each, while SITMAP in the occurrence data is the
# four-digit basic field, the grid of the models. The exports carry no
# quadrant code, so each record is placed in the quadrant that holds its
# coordinates. Matching POLE on its first four characters, as the map used
# to, gave all four quadrants of a field the class of the whole field: a
# quadrant without a record was drawn as surveyed, and a species found in
# one quadrant was drawn in all four.
record_xy <- sf::st_as_sf(
  data.frame(X = data$X, Y = data$Y),
  coords = c("X", "Y"), crs = CRS_SJTSK
)
# A point on the line between two quadrants takes the first of them.
data$POLE <- vapply(
  sf::st_intersects(record_xy, sitmap),
  function(i) if (length(i)) as.character(sitmap$POLE[[i[[1]]]]) else NA_character_,
  character(1)
)

# A record outside every quadrant would drop out of the map unnoticed, so it
# is checked explicitly.
n_unplaced <- sum(is.na(data$POLE))
if (n_unplaced > 0) {
  warning(
    n_unplaced, " records lie in no quadrant of the mapping grid and are ",
    "left out of the grid map; check their coordinates.",
    call. = FALSE
  )
}
# The few records whose coordinates lie in a basic field other than their
# SITMAP, for the report.
n_other_field <- sum(substr(data$POLE, 1, 4) != as.character(data$SITMAP), na.rm = TRUE)

grid_codes <- function(x) unique(as.character(stats::na.omit(x)))

#' The cells of one level of the grid ("SITMAP" or "POLE") that were
#' surveyed and that hold each species.
grid_cells <- function(level) {
  found <- function(species) data[[level]][data$DRUH == species & data$POSITIVE == 1]
  list(
    surveyed = grid_codes(data[[level]]),
    nausithous = grid_codes(found(SPECIES_NAU)),
    teleius = grid_codes(found(SPECIES_TEL))
  )
}
fields <- grid_cells("SITMAP")
quadrants <- grid_cells("POLE")
n_quadrants_other_field <- sum(!substr(quadrants$surveyed, 1, 4) %in% fields$surveyed)

# Each surveyed quadrant is filled by what was found in it: the species
# colour where one species was, the spot black where both were. The
# map used to overlay two half-transparent layers, which left the fields with
# both species in a mixed colour the caption did not name.
distribution_classes <- c(
  "surveyed only", "P. nausithous", "P. teleius", "both species"
)
distribution_fills <- stats::setNames(
  c("grey85", phengaris_col(c("nausithous_brown", "teleius_blue", "spot_black"))),
  distribution_classes
)
surveyed_quadrants <- sitmap %>%
  dplyr::filter(POLE %in% quadrants$surveyed) %>%
  dplyr::mutate(
    found = factor(
      dplyr::case_when(
        POLE %in% quadrants$nausithous & POLE %in% quadrants$teleius ~ "both species",
        POLE %in% quadrants$nausithous ~ "P. nausithous",
        POLE %in% quadrants$teleius ~ "P. teleius",
        TRUE ~ "surveyed only"
      ),
      levels = distribution_classes
    )
  )

# A quadrant on the border lies partly abroad. Each quadrant is clipped to
# the border, so the map shows no grid outside Czechia.
sf::st_agr(surveyed_quadrants) <- "constant"
surveyed_quadrants <- sf::st_intersection(surveyed_quadrants, sf::st_union(czechia_border))

# The number of quadrants in each class, for the legend.
quadrants_per_class <- as.integer(table(surveyed_quadrants$found))

data_dist_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = czechia_border,
    fill = ELEVATION_TINTS[1],
    colour = NA
  ) +
  map_relief(hypso_df) +
  ggplot2::geom_sf(
    data = surveyed_quadrants,
    ggplot2::aes(fill = found),
    colour = "white",
    linewidth = 0.12
  ) +
  ggplot2::geom_sf(
    data = czechia_border,
    fill = NA,
    colour = phengaris_col("spot_black"),
    linewidth = 0.35
  ) +
  ggplot2::scale_fill_manual(
    values = distribution_fills,
    labels = parse(text = sprintf(
      c(
        "'surveyed only (%s)'", "italic('P. nausithous')~'only (%s)'", "italic('P. teleius')~'only (%s)'",
        "'both species (%s)'"
      ),
      quadrants_per_class
    )),
    name = "Mapping-grid quadrants",
    drop = FALSE
  ) +
  map_frame() +
  theme_phengaris_map(legend = c(0.975, 0.985))

report_figure(
  data_dist_map,
  paste(
    "Quadrants of the mapping grid surveyed (grey), with P. nausithous only (brown),",
    "P. teleius only (blue) and both species (black)"
  ),
  "14_species_distribution",
  height = 11.2
)

report_table(
  data.frame(
    subset = c("surveyed", "with P. nausithous", "with P. teleius"),
    basic_fields = unname(lengths(fields)),
    quadrants = unname(lengths(quadrants)),
    stringsAsFactors = FALSE
  ),
  "Mapping grid coverage: basic fields (SITMAP, the grid of the models) and their quadrants (the map)",
  "14_grid_coverage"
)

report_warning(
  "The grid map is drawn at the quadrants of the mapping grid (POLE in the",
  "grid layer). The occurrence exports carry only the four-digit basic field",
  "(SITMAP), the grid of the models, so each record is placed in the quadrant",
  "that holds its coordinates and each quadrant is coloured by its own",
  "records. Records in no quadrant:", paste0(n_unplaced, "."),
  "Records whose coordinates lie in a basic field other than their SITMAP:",
  paste0(n_other_field, ","), "which leaves", n_quadrants_other_field,
  "surveyed quadrants in a field that no SITMAP code names."
)

report_finish()

step_end("14")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

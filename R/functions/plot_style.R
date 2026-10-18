#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: palette, themes and figure sizes
#
#----------------------------------------------------------#
#
# One palette, one theme and one set of print sizes for every figure of the
# cascade, so that the figures of either specification read as one set.
#
# The palette is taken from Phengaris teleius itself, from the photographs
# of the species on lepidoptera.cz
# (https://www.lepidoptera.cz/motyli/modrasek-ockovany-maculinea-telejus-bergstraesser-1779):
#
#   - the two blues: hue and lightness sampled from the male upperside and
#     the wing base of the specimen plate /images/820.jpg (Josef Dvořák) and
#     the live male /images/76.jpg. The wing photographs as a nearly grey
#     silver (CIELAB chroma about 4), too grey to tell series apart, so the
#     chroma is that of the provisional blues of the brief;
#   - the border brown, the underside buff, the spot black and the fringe
#     white: sampled from the forewing margin of 820.jpg, 76.jpg and
#     1132.jpg, the hindwing underside of 1616.jpg, 1133.jpg and 77.jpg, its
#     post-discal spots and its fringe (the fringe raised to L* 95, hue kept);
#   - the nausithous brown is not in these photographs: it is the provisional
#     cinnamon of the brief, close to the dark underside of P. nausithous.
#
# changes/figures_style.md lists every sample, every adjustment and the
# check of each colour pair under simulated colour-vision deficiency.
#
#----------------------------------------------------------#

#--------------------------------------------------#
## Palette -----
#--------------------------------------------------#

PHENGARIS_COLS <- c(
  # Male upperside: the series colour of P. teleius (3.4:1 on white).
  teleius_blue = "#8387B5",
  # Wing base: positive records, single-series bars, the end of the ramps.
  deep_blue = "#4F5789",
  # Dark marginal band: axis lines and ticks.
  border_brown = "#644938",
  # Underside: negative records (a fill; 2.6:1 on white, too light for lines).
  underside_buff = "#B89B7F",
  # Post-discal spots: text, the primary series, arrows.
  spot_black = "#0A0301",
  # Fringe and spot rings: the light end of the sequential ramp.
  fringe_white = "#F3F1EA",
  # P. nausithous, against the teleius blue.
  nausithous_brown = "#8A5A3B"
)

# Tints of the palette: the same CIELAB hue and chroma at another lightness.
PHENGARIS_TINTS <- c(
  # L* 78: hull and box fills of P. teleius.
  teleius_pale = "#BDBEE0",
  # L* 60: P. nausithous where it is the lighter of two (3.2:1 on white).
  nausithous_light = "#B5876B",
  # L* 58: the buff for points and lines (3.4:1 on white).
  buff_dark = "#A2876B",
  # L* 82: imputed absences, lighter than the observed ones.
  buff_light = "#DCC9B7"
)

#' A palette colour by name, unnamed.
phengaris_col <- function(name) unname(c(PHENGARIS_COLS, PHENGARIS_TINTS)[name])

# Neutral greys for what carries no data: reference lines, gridlines.
REFERENCE_GREY <- "grey50"
GRID_GREY <- "grey91"

#--------------------------------------------------#
## Species -----
#--------------------------------------------------#

#' Colour of each species, named by the full and the short name, so that a
#' manual scale matches either.
species_colours <- function() {
  cols <- c(phengaris_col("nausithous_brown"), phengaris_col("teleius_blue"))
  stats::setNames(
    c(cols, cols),
    c(SPECIES_NAU, SPECIES_TEL, sub("^Phengaris ", "P. ", c(SPECIES_NAU, SPECIES_TEL)))
  )
}

#' Labels in italics, for species names on axes, in legends and strips.
italic_labels <- function(x) {
  parse(text = sprintf("italic(\"%s\")", gsub("\"", "\\\\\"", as.character(x))))
}

#' The colour and fill scales of the two species, labels in italics.
scale_colour_species <- function(name = NULL, ...) {
  ggplot2::scale_colour_manual(values = species_colours(), labels = italic_labels, name = name, ...)
}

scale_fill_species <- function(name = NULL, ...) {
  ggplot2::scale_fill_manual(values = species_colours(), labels = italic_labels, name = name, ...)
}

#--------------------------------------------------#
## Site occupancy -----
#--------------------------------------------------#

# Site occupancy is shown with the same two colours throughout: the buff of
# the underside for records without the species, the deep blue for records
# with it. Both scales take the factor of POSITIVE (levels 0, 1).
OCCUPANCY_VALUES <- c(phengaris_col("underside_buff"), phengaris_col("deep_blue"))
OCCUPANCY_LINE_VALUES <- c(phengaris_col("buff_dark"), phengaris_col("deep_blue"))
OCCUPANCY_LABELS <- c("negative", "positive")

scale_fill_occupancy <- function(name = "site occupancy", labels = OCCUPANCY_LABELS) {
  ggplot2::scale_fill_manual(
    labels = labels,
    name   = name,
    values = OCCUPANCY_VALUES
  )
}

# Points and lines take the darker buff, which stands out from a white
# background.
scale_colour_occupancy <- function(name = "site occupancy", labels = OCCUPANCY_LABELS) {
  ggplot2::scale_color_manual(
    labels = labels,
    name   = name,
    values = OCCUPANCY_LINE_VALUES
  )
}

#--------------------------------------------------#
## Ramps -----
#--------------------------------------------------#

#' Sequential ramp for fills, from the fringe white to the deep blue,
#' interpolated in CIELAB.
phengaris_ramp <- grDevices::colorRampPalette(
  c(phengaris_col("fringe_white"), phengaris_col("teleius_blue"), phengaris_col("deep_blue")),
  space = "Lab"
)

#' n ordered fills from the ramp, the lightest ones left out so that every
#' fill shows on white.
phengaris_fills <- function(n, begin = 0.3) {
  phengaris_ramp(101)[round(100 * seq(begin, 1, length.out = n)) + 1]
}

#' n ordered line colours, dark to light: spot black, deep blue, teleius
#' blue and the steps between them. Every one reads on white.
phengaris_lines <- function(n) {
  ramp <- grDevices::colorRampPalette(
    c(phengaris_col("spot_black"), phengaris_col("deep_blue"), phengaris_col("teleius_blue")),
    space = "Lab"
  )
  ramp(n)
}

#' Continuous fill scale on the ramp.
scale_fill_phengaris_c <- function(...) {
  ggplot2::scale_fill_gradientn(colours = phengaris_ramp(256), ...)
}

#' Bar charts start at zero and get a little headroom above the tallest bar.
scale_y_count <- function() {
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))
}

#' The same for horizontal bars.
scale_x_count <- function() {
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))
}

#--------------------------------------------------#
## Sizes -----
#--------------------------------------------------#

# The figures are written at their print size, so that the type is 8-9 pt in
# print: 9 pt titles, 8 pt tick labels, legends and annotations.
FIGURE_BASE_SIZE <- 9

# Journal column widths (cm) and the default heights.
FIGURE_WIDTHS <- c(single = 8.5, double = 17.5)
FIGURE_HEIGHTS <- c(single = 7, double = 11)
FIGURE_DPI <- 300

# The size of annotation text (geom_text, annotate) in mm, matching the
# 8 pt tick labels.
FIGURE_TEXT_MM <- 0.9 * FIGURE_BASE_SIZE / ggplot2::.pt

#' Width and height of a figure in cm, from a preset width.
#'
#' @param size    "single" or "double" column
#' @param height  height in cm; the preset height when NULL
figure_size <- function(size = c("double", "single"), height = NULL) {
  size <- match.arg(size)
  list(width = FIGURE_WIDTHS[[size]], height = if (is.null(height)) FIGURE_HEIGHTS[[size]] else height)
}

#--------------------------------------------------#
## Themes -----
#--------------------------------------------------#

#' The theme of every figure: classic axes in the border brown, no top or
#' right axis line, untitled strips with left-aligned bold labels, the
#' legend below.
#'
#' @param grid    gridlines along the axis the values are read off: "none",
#'                "y" (horizontal lines, for bars and point estimates),
#'                "x" (vertical lines, for forest plots) or "xy"
#' @param legend  "bottom", "right", "none", or the c(x, y) position of the
#'                legend inside the panel, in npc
theme_phengaris <- function(base_size = FIGURE_BASE_SIZE, grid = c("none", "y", "x", "xy"),
                            legend = "bottom") {
  grid <- match.arg(grid)
  gridline <- ggplot2::element_line(colour = GRID_GREY, linewidth = 0.25)
  ink <- phengaris_col("spot_black")
  inside <- is.numeric(legend)
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = ink),
      axis.line = ggplot2::element_line(colour = phengaris_col("border_brown"), linewidth = 0.3),
      axis.ticks = ggplot2::element_line(colour = phengaris_col("border_brown"), linewidth = 0.3),
      axis.ticks.length = ggplot2::unit(0.15, "lines"),
      axis.text = ggplot2::element_text(colour = ink, size = ggplot2::rel(0.9)),
      panel.grid.major.y = if (grid %in% c("y", "xy")) gridline else ggplot2::element_blank(),
      panel.grid.major.x = if (grid %in% c("x", "xy")) gridline else ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = ggplot2::rel(1), colour = ink),
      # A left-aligned strip label may run past a narrow panel.
      strip.clip = "off",
      panel.spacing = ggplot2::unit(0.8, "lines"),
      legend.position = if (inside) "inside" else legend,
      legend.position.inside = if (inside) legend,
      legend.justification.inside = if (inside) legend,
      legend.title = ggplot2::element_text(size = ggplot2::rel(0.9)),
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.9)),
      legend.key.size = ggplot2::unit(0.9, "lines"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = ggplot2::unit(0.4, "lines"),
      legend.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = ggplot2::rel(1), face = "bold"),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.9)),
      plot.title.position = "plot",
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(4, 6, 4, 4)
    )
}

#' Species strips in italics.
theme_species_strips <- function() {
  ggplot2::theme(strip.text = ggplot2::element_text(face = "bold.italic", hjust = 0))
}

#' The theme of the maps: no axes, a white background.
#'
#' @param legend  "bottom", "none", or the c(x, y) corner of the legend
#'                inside the frame, in npc (the legend is justified to it)
theme_phengaris_map <- function(base_size = FIGURE_BASE_SIZE, legend = "bottom") {
  inside <- is.numeric(legend)
  ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = phengaris_col("spot_black")),
      legend.position = if (inside) "inside" else legend,
      legend.position.inside = if (inside) legend,
      legend.justification.inside = if (inside) legend,
      legend.box.just = "left",
      legend.spacing.y = ggplot2::unit(0.5, "lines"),
      legend.title = ggplot2::element_text(size = ggplot2::rel(0.9), face = "bold"),
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.9)),
      legend.key.size = ggplot2::unit(0.9, "lines"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(2, 2, 2, 2)
    )
}

#--------------------------------------------------#
## Map furniture -----
#--------------------------------------------------#

# Elevation tints of the maps: from the fringe white of the lowlands to a
# pale silver of the teleius hue in the mountains (L* 96 to 76), light
# enough for the records to read on top.
ELEVATION_TINTS <- c("#F5F3EE", "#E4E4EE", "#D0D0E0", "#B9BACE")

#' The relief of the maps as two layers over a light base: shadows in the
#' spot black where the hillshade is darker than flat ground, highlights in
#' white where it is lighter.
#'
#' @param shade      data frame of x, y and shade (the hillshade)
#' @param intensity  the opacity of the darkest shadow and the brightest
#'                   highlight
map_relief <- function(shade, intensity = c(shadow = 0.45, light = 0.55)) {
  flat <- stats::median(shade$shade)
  q <- stats::quantile(shade$shade, c(0.01, 0.99))
  shade$shadow <- intensity[["shadow"]] * pmin(1, pmax(0, (flat - shade$shade) / (flat - q[[1]])))
  shade$light <- intensity[["light"]] * pmin(1, pmax(0, (shade$shade - flat) / (q[[2]] - flat)))
  list(
    ggplot2::geom_raster(
      data = shade, ggplot2::aes(x = x, y = y, alpha = shadow),
      fill = phengaris_col("spot_black"), inherit.aes = FALSE
    ),
    ggplot2::geom_raster(
      data = shade, ggplot2::aes(x = x, y = y, alpha = light),
      fill = "white", inherit.aes = FALSE
    ),
    ggplot2::scale_alpha_identity()
  )
}

#' A scale bar of alternating black and white blocks, for a map in a
#' projected CRS in metres.
#'
#' @param x,y        the lower-left corner, in map units
#' @param length_km  the length of the bar
#' @param blocks     the number of blocks
#' @param height     the height of the bar, in map units
map_scale_bar <- function(x, y, length_km = 100, blocks = 4, height = 3500) {
  step <- 1000 * length_km / blocks
  bar <- data.frame(
    xmin = x + step * (seq_len(blocks) - 1), xmax = x + step * seq_len(blocks),
    fill = rep(c(phengaris_col("spot_black"), "white"), length.out = blocks)
  )
  list(
    ggplot2::annotate(
      "rect",
      xmin = bar$xmin, xmax = bar$xmax, ymin = y, ymax = y + height,
      fill = bar$fill, colour = phengaris_col("spot_black"), linewidth = 0.2
    ),
    ggplot2::annotate(
      "text",
      x = c(x, x + 500 * length_km, x + 1000 * length_km), y = y + 2.4 * height,
      label = c("0", format_tick(length_km / 2), paste(length_km, "km")),
      size = 0.85 * FIGURE_TEXT_MM, colour = phengaris_col("spot_black"), vjust = 0
    )
  )
}

#' The angle (degrees, clockwise from the map's up) of true north at a
#' point, for a map in a CRS whose grid north is not true north (in S-JTSK
#' about 7 degrees in the middle of Czechia).
true_north_angle <- function(x, y, crs) {
  here <- sf::st_sfc(sf::st_point(c(x, y)), crs = crs)
  lonlat <- sf::st_coordinates(sf::st_transform(here, 4326))
  north <- sf::st_sfc(sf::st_point(c(lonlat[1], lonlat[2] + 0.1)), crs = 4326)
  ahead <- sf::st_coordinates(sf::st_transform(north, crs))
  atan2(ahead[1] - x, ahead[2] - y) * 180 / pi
}

#' A north arrow pointing to true north, with an "N" at its tip.
#'
#' @param x,y     the foot of the arrow, in map units
#' @param length  the length of the arrow, in map units
#' @param crs     the CRS of the map
map_north_arrow <- function(x, y, length = 22000, crs) {
  angle <- true_north_angle(x, y, crs) * pi / 180
  tip <- c(x + length * sin(angle), y + length * cos(angle))
  list(
    ggplot2::annotate(
      "segment",
      x = x, y = y, xend = tip[1], yend = tip[2],
      arrow = ggplot2::arrow(length = ggplot2::unit(0.16, "cm"), type = "closed"),
      colour = phengaris_col("spot_black"), linewidth = 0.4, linejoin = "mitre"
    ),
    ggplot2::annotate(
      "text",
      x = tip[1] + 0.25 * length * sin(angle), y = tip[2] + 0.25 * length * cos(angle),
      label = "N", fontface = "bold", size = FIGURE_TEXT_MM, colour = phengaris_col("spot_black")
    )
  )
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

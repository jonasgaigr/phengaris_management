#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#     Helper functions: the figures of the revised analysis
#
#
#----------------------------------------------------------#
#
# One style for the figures of the revised specification and of the common
# steps, so that they read as one set:
#
#   - the three samples of the revised models always in the same order and
#     with the same marks, from dark to light as fewer imputed absences are
#     kept (with all of them, with the plausible ones, observed records only),
#     each with its own shape so that they stay apart in greyscale print;
#   - species names in italics;
#   - readable labels for the model terms, the threat classes and the land
#     covers instead of the column names;
#   - odds ratios and distances on log scales with plain tick labels.
#
# Each plot function takes the table its step also writes as CSV, so every
# figure can be checked against a table and redrawn without refitting.
#
# The palette, theme_phengaris() and the print sizes, shared with the legacy
# figures, are in R/functions/plot_style.R.
#
#----------------------------------------------------------#

#--------------------------------------------------#
## Style -----
#--------------------------------------------------#

#' "Phengaris nausithous" -> "P. nausithous".
species_short <- function(species) sub("^Phengaris ", "P. ", species)

#' An axis title ending in an italic species name.
species_title <- function(text, species_label) {
  bquote(.(text) ~ italic(.(species_label)))
}

#' Tick labels without trailing zeros (0.3, 1, 10 rather than 0.30, 1.00,
#' 10.00).
format_tick <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) NA_character_ else format(signif(v, 3), trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
  }, character(1))
}

#' Breaks of a log axis: 1-2-5 per decade over a short range, 1-3 over a
#' medium one, powers of ten over a long one.
#'
#' @param max_n  at most this many breaks, for narrow panels: every other
#'               break is dropped, keeping the one nearest 1, until they fit
log_breaks <- function(limits, max_n = Inf) {
  limits <- range(limits[is.finite(limits) & limits > 0])
  span <- diff(log10(limits))
  base <- if (span > 2.5) 1 else if (span > 1.2) c(1, 3) else c(1, 2, 5)
  breaks <- sort(as.vector(outer(base, 10^(-4:4))))
  breaks <- breaks[breaks >= limits[1] * 0.999 & breaks <= limits[2] * 1.001]
  while (length(breaks) > max_n) {
    anchor <- which.min(abs(log10(breaks)))
    breaks <- breaks[(seq_along(breaks) - anchor) %% 2 == 0]
  }
  breaks
}

#' A log x axis for odds ratios.
#'
#' @param max_breaks  passed to log_breaks() as max_n
scale_x_ratio <- function(name = "Odds ratio (95% interval, log scale)", max_breaks = Inf, ...) {
  ggplot2::scale_x_log10(
    name = name, breaks = function(limits) log_breaks(limits, max_n = max_breaks), labels = format_tick, ...
  )
}

#' The reference line at an odds ratio of 1.
ratio_reference <- function() {
  ggplot2::geom_vline(xintercept = 1, colour = REFERENCE_GREY, linewidth = 0.35)
}

#--------------------------------------------------#
## The three samples -----
#--------------------------------------------------#

#' The samples in their fixed order, as a factor.
sample_factor <- function(sample) factor(sample, levels = unname(REVISED_SAMPLES))

#' Colour of each sample, darker for more imputed absences: the spot black
#' for the primary sample, then the deep and the teleius blue.
sample_colours <- function() stats::setNames(phengaris_lines(3), unname(REVISED_SAMPLES))

#' Shape of each sample: circle, triangle, square (all fillable).
sample_shapes <- function() stats::setNames(c(21, 24, 22), unname(REVISED_SAMPLES))

#' Line type of each sample, for curves.
sample_linetypes <- function() stats::setNames(c("solid", "longdash", "dotted"), unname(REVISED_SAMPLES))

#' The colour, fill and shape scales of the samples; one legend. The values
#' are matched by name, so a sample keeps its marks when another is absent.
scale_sample <- function(name = NULL, fill = TRUE) {
  list(
    ggplot2::scale_colour_manual(values = sample_colours(), name = name),
    if (fill) ggplot2::scale_fill_manual(values = sample_colours(), name = name),
    ggplot2::scale_shape_manual(values = sample_shapes(), name = name)
  )
}

#' Point ranges of an estimate per sample, dodged vertically (for a
#' horizontal forest plot, y a factor).
sample_pointrange <- function(width = 0.65, size = 0.3, ...) {
  ggplot2::geom_pointrange(
    ggplot2::aes(xmin = low, xmax = high, colour = sample, fill = sample, shape = sample),
    position = ggplot2::position_dodge(width = width),
    size = size, linewidth = 0.45, stroke = 0.6, ...
  )
}

#--------------------------------------------------#
## Labels -----
#--------------------------------------------------#

#' Readable labels of the threat classes (config.yml `strukt_pozn:
#' threat_items`), with their Article 17 code.
THREAT_LABELS <- c(
  LandUseChange = "change of agricultural land use",
  Abandonment = "abandonment of management",
  HarmfulMow = "harmful mowing",
  HarmfulGrazing = "intensive grazing",
  GrazingByeffects = "effects of livestock farming",
  FertilizerUse = "fertiliser use",
  Afforestation = "afforestation",
  Invasives = "invasive alien species",
  NativeDominants = "expanding native species",
  AbioticNaturalProcesses = "abiotic natural processes",
  Encroachment = "succession and encroachment",
  BiomassAccumulation = "accumulation of biomass",
  Eutrophization = "eutrophication"
)

#' "FertilizerUse" -> "fertiliser use (A19, A20)".
threat_label <- function(class) {
  label <- ifelse(class %in% names(THREAT_LABELS), THREAT_LABELS[class], class)
  code <- article17_code(class)
  unname(ifelse(is.na(code) | code == "", label, paste0(label, " (", code, ")")))
}

#' Readable labels of the landscape variables (config.yml `landscape:
#' groups`, and the VMB shares of R/functions/landscape.R).
LANDSCAPE_LABELS <- c(
  GRASSLAND = "grassland",
  ARABLE = "arable land",
  FOREST = "forest",
  WATER = "water and wetlands",
  URBAN = "settlements",
  NATURAL = "natural habitats",
  SANGUISORBA = "Sanguisorba meadows"
)

#' The landscape variables as a factor of readable labels, in a fixed order.
landscape_factor <- function(variable) {
  known <- intersect(names(LANDSCAPE_LABELS), unique(variable))
  levels <- c(known, setdiff(unique(variable), known))
  labels <- ifelse(levels %in% names(LANDSCAPE_LABELS), LANDSCAPE_LABELS[levels], levels)
  factor(variable, levels = levels, labels = labels)
}

#' The key terms (SYNTHESIS_TERMS) as a factor of readable labels.
term_factor <- function(term) {
  factor(SYNTHESIS_TERMS[term], levels = unname(SYNTHESIS_TERMS))
}

#' The term names of the occupancy posterior (step 23), which replace ":"
#' and other symbols with "_", back as the model terms ("TIMING_METHOD" ->
#' "TIMING:METHOD"), as collect_robustness() reads them.
model_term <- function(term) {
  clean <- stats::setNames(names(SYNTHESIS_TERMS), gsub("[^A-Za-z0-9_]", "_", names(SYNTHESIS_TERMS)))
  unname(ifelse(term %in% names(clean), clean[term], term))
}

#--------------------------------------------------#
## Step 08: the records of the revised models -----
#--------------------------------------------------#

#' Fill of each record outcome (RECORD_OUTCOMES): the deep blue for the
#' species found, the buff for the observed negatives, lighter buff and the
#' fringe white for the imputed ones.
outcome_fills <- function() {
  stats::setNames(
    phengaris_col(c("deep_blue", "underside_buff", "buff_light", "fringe_white")),
    RECORD_OUTCOMES
  )
}

#' In-season records per year by outcome, stacked.
#'
#' @param tab  summarise_records_by_outcome()
plot_records_by_outcome <- function(tab) {
  tab <- dplyr::mutate(
    tab,
    species = species_short(DRUH),
    outcome = factor(outcome, levels = rev(RECORD_OUTCOMES))
  )
  # The buff outline keeps the near-white segments of the imputed absences
  # visible against the background.
  ggplot2::ggplot(tab, ggplot2::aes(x = factor(YEAR), y = records, fill = outcome)) +
    ggplot2::geom_col(colour = phengaris_col("underside_buff"), linewidth = 0.2, width = 0.75) +
    ggplot2::facet_wrap(~species) +
    ggplot2::scale_fill_manual(values = outcome_fills(), breaks = RECORD_OUTCOMES, name = NULL) +
    scale_y_count() +
    ggplot2::labs(x = NULL, y = "Records in the flight season") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2)) +
    theme_phengaris(grid = "y") +
    theme_species_strips()
}

#' Observed visits by their position in the season, stacked by outcome.
#'
#' @param tab    summarise_visit_timing()
#' @param width  the width of the day classes
#' @param xlim   the days shown (those of the flight curve of step 16)
plot_visit_timing <- function(tab, width = 5, xlim = c(-45, 45)) {
  tab <- tab %>%
    dplyr::group_by(DRUH, day_class, outcome) %>%
    dplyr::summarise(records = sum(records), .groups = "drop") %>%
    dplyr::mutate(
      species = species_short(DRUH),
      outcome = factor(outcome, levels = c("negative", "positive"))
    )
  ggplot2::ggplot(tab, ggplot2::aes(x = day_class + width / 2, y = records, fill = outcome)) +
    ggplot2::geom_col(width = width, colour = "white", linewidth = 0.25) +
    ggplot2::geom_vline(xintercept = 0, colour = REFERENCE_GREY, linewidth = 0.35) +
    ggplot2::facet_wrap(~species, ncol = 1, scales = "free_y") +
    ggplot2::scale_fill_manual(
      values = stats::setNames(OCCUPANCY_VALUES, c("negative", "positive")), breaks = c("positive", "negative"),
      name = "Visit"
    ) +
    ggplot2::coord_cartesian(xlim = xlim) +
    scale_y_count() +
    ggplot2::labs(x = "Days from the year's median positive record", y = "Observed visits") +
    theme_phengaris(grid = "y") +
    theme_species_strips()
}

#' Share of the in-season records with the species at each value of the
#' management model's variables, per species and sample.
#'
#' @param tab  summarise_share_positive()
plot_share_positive <- function(tab) {
  values <- unname(unlist(MANAGEMENT_VARIABLES))
  tab <- dplyr::mutate(
    tab,
    species = species_short(DRUH),
    variable = factor(MANAGEMENT_VARIABLE_LABELS[variable], levels = unname(MANAGEMENT_VARIABLE_LABELS)),
    value = factor(value, levels = unique(values)),
    sample = sample_factor(sample)
  )
  ggplot2::ggplot(tab, ggplot2::aes(x = value, y = share_positive)) +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = low, ymax = high, colour = sample),
      position = ggplot2::position_dodge(width = 0.6), linewidth = 0.45
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = sample, fill = sample, shape = sample),
      position = ggplot2::position_dodge(width = 0.6), size = 1.9, stroke = 0.6
    ) +
    ggplot2::facet_grid(
      species ~ variable,
      scales = "free", space = "free_x",
      labeller = ggplot2::labeller(variable = ggplot2::label_wrap_gen(18))
    ) +
    scale_sample() +
    ggplot2::scale_x_discrete(labels = scales::label_wrap(11)) +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = NULL, y = "Share of the records with the species (95% interval)") +
    theme_phengaris(grid = "y") +
    ggplot2::theme(strip.text.y = ggplot2::element_text(face = "bold.italic", angle = 0, hjust = 0))
}

#' Records per observer: how many observers recorded how many records.
#'
#' @param observers  summarise_observers()
plot_observers <- function(observers) {
  ggplot2::ggplot(observers, ggplot2::aes(x = obs_num)) +
    ggplot2::geom_histogram(bins = 20, fill = phengaris_col("deep_blue"), colour = "white", linewidth = 0.3) +
    ggplot2::geom_vline(
      xintercept = stats::median(observers$obs_num), colour = phengaris_col("spot_black"),
      linewidth = 0.4, linetype = "dashed"
    ) +
    ggplot2::annotate(
      "text",
      x = stats::median(observers$obs_num), y = Inf,
      label = sprintf("median %g", stats::median(observers$obs_num)),
      hjust = -0.1, vjust = 1.5, size = FIGURE_TEXT_MM, colour = phengaris_col("spot_black")
    ) +
    ggplot2::scale_x_log10(labels = format_tick) +
    scale_y_count() +
    ggplot2::labs(x = "Records per observer (log scale)", y = "Observers") +
    theme_phengaris(grid = "y")
}

#--------------------------------------------------#
## Step 16: the flight curve -----
#--------------------------------------------------#

#' Flight curves of the two species: the relative activity of adults across
#' the season, solid over the days the visits cover and dotted where the
#' curve is extrapolated, with the cut-off of the flight season.
#'
#' @param curve   flight_curve.csv, the elevations to show only
#' @param cutoff  the relative activity the flight season ends at
plot_flight_curve <- function(curve, cutoff) {
  elevations <- sort(unique(curve$ELEVATION[!is.na(curve$ELEVATION)]))
  elevation_levels <- if (length(elevations)) paste(elevations, "m") else character()
  levels <- c(elevation_levels, "no elevation term")
  # Dark to light up the elevations; the curve without an elevation term in
  # the spot black.
  cols <- c(if (length(elevations)) phengaris_lines(length(elevations)), phengaris_col("spot_black"))
  curve <- curve %>%
    dplyr::mutate(
      species = species_short(DRUH),
      elevation = factor(ifelse(is.na(ELEVATION), "no elevation term", paste(ELEVATION, "m")), levels = levels)
    ) %>%
    dplyr::group_by(species, elevation) %>%
    dplyr::arrange(DOY_DEV, .by_group = TRUE) %>%
    dplyr::mutate(segment = cumsum(IN_DATA_RANGE != dplyr::lag(IN_DATA_RANGE, default = IN_DATA_RANGE[1]))) %>%
    dplyr::ungroup()

  ggplot2::ggplot(
    curve,
    ggplot2::aes(x = DOY_DEV, y = RELATIVE_ACTIVITY, colour = elevation, group = interaction(elevation, segment))
  ) +
    ggplot2::geom_hline(yintercept = cutoff, colour = REFERENCE_GREY, linewidth = 0.35) +
    ggplot2::annotate(
      "text",
      x = 0, y = cutoff, label = sprintf("end of the flight season: %g %% of the peak", 100 * cutoff),
      vjust = -0.6, size = FIGURE_TEXT_MM, colour = REFERENCE_GREY
    ) +
    ggplot2::geom_line(data = dplyr::filter(curve, !IN_DATA_RANGE), linetype = "dotted", linewidth = 0.5) +
    ggplot2::geom_line(data = dplyr::filter(curve, IN_DATA_RANGE), linewidth = 0.8) +
    ggplot2::facet_wrap(~species) +
    ggplot2::scale_colour_manual(
      values = stats::setNames(cols, levels), breaks = elevation_levels,
      name = "Elevation of the site"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1.02), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(x = "Days from the year's median positive record", y = "Relative activity of adults") +
    theme_phengaris() +
    theme_species_strips()
}

#--------------------------------------------------#
## Step 17: occupancy models -----
#--------------------------------------------------#

#' Marginal predicted probability of recording the species by mowing
#' timing and method, host-plant abundance and the EVL designated for the
#' species, in the three samples.
#'
#' @param pred_mgmt   17_<species>_predictions_management
#' @param pred_other  17_<species>_predictions_plant_protection
#' @param leave_out   models not drawn (flagged by the diagnostics)
plot_predictions <- function(pred_mgmt, pred_other, species_label, leave_out = character()) {
  pred_mgmt <- pred_mgmt[!pred_mgmt$model %in% leave_out, , drop = FALSE]
  pred_other <- pred_other[!pred_other$model %in% leave_out, , drop = FALSE]
  mowing_levels <- c(
    "harmful\nwhole area", "harmful\npatches uncut", "appropriate\nwhole area", "appropriate\npatches uncut"
  )
  plant_levels <- c("single plants", "abundant", "dominant")
  evl_levels <- c("outside", "inside")
  panels <- c(
    "a  mowing: timing and method", "b  host plant", "c  EVL designated for the species"
  )
  rows <- dplyr::bind_rows(
    pred_mgmt %>%
      dplyr::mutate(
        panel = panels[1],
        level = mowing_levels[1 + 2 * as.integer(TIMING) + as.integer(METHOD)]
      ),
    pred_other %>%
      dplyr::filter(!is.na(PLANT)) %>%
      dplyr::mutate(
        panel = panels[2],
        level = plant_levels[match(PLANT, c("single", "abundant", "dominant"))]
      ),
    pred_other %>%
      dplyr::filter(!is.na(EVL_species)) %>%
      dplyr::mutate(panel = panels[3], level = evl_levels[1 + as.integer(EVL_species)])
  ) %>%
    dplyr::mutate(
      panel = factor(panel, levels = panels),
      level = factor(level, levels = c(mowing_levels, plant_levels, evl_levels)),
      sample = sample_factor(sample)
    )

  ggplot2::ggplot(rows, ggplot2::aes(x = level, y = estimate)) +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = conf_low, ymax = conf_high, colour = sample),
      position = ggplot2::position_dodge(width = 0.6), linewidth = 0.45
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = sample, fill = sample, shape = sample),
      position = ggplot2::position_dodge(width = 0.6), size = 2, stroke = 0.6
    ) +
    ggplot2::facet_grid(
      ~panel,
      scales = "free_x", space = "free_x", labeller = ggplot2::labeller(panel = ggplot2::label_wrap_gen(22))
    ) +
    scale_sample() +
    ggplot2::scale_x_discrete(labels = function(x) gsub(" ", "\n", x)) +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.04))) +
    ggplot2::labs(x = NULL, y = species_title("Predicted probability of recording", species_label)) +
    theme_phengaris(grid = "y")
}

#' Predicted probability of recording the species across the flight
#' season (baseline model), in the three samples.
#'
#' @param curve  season_curve() of the three baseline models
plot_season <- function(curve, species_label) {
  curve <- dplyr::mutate(curve, sample = sample_factor(sample))
  ggplot2::ggplot(curve, ggplot2::aes(x = DOY_DEV, y = estimate)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = conf_low, ymax = conf_high, fill = sample),
      alpha = 0.12, colour = NA
    ) +
    ggplot2::geom_line(ggplot2::aes(colour = sample, linetype = sample), linewidth = 0.7) +
    ggplot2::scale_colour_manual(values = sample_colours(), name = NULL) +
    ggplot2::scale_fill_manual(values = sample_colours(), name = NULL) +
    ggplot2::scale_linetype_manual(values = sample_linetypes(), name = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(
      x = "Days from the year's median positive record",
      y = bquote(atop("Predicted probability", "of recording" ~ italic(.(species_label))))
    ) +
    theme_phengaris(grid = "y") +
    # A single-column figure: the samples one below the other.
    ggplot2::theme(legend.key.width = ggplot2::unit(2.2, "lines"), legend.direction = "vertical")
}

#' Profile likelihood of the connectivity: delta AIC against the mean
#' dispersal distance 1 / alpha, one line per area exponent b, the best pair
#' of each sample ringed.
#'
#' @param profile  17_<species>_connectivity_profile
plot_connectivity_profile <- function(profile, species_label) {
  exponents <- sort(unique(profile$area_exponent))
  cols <- stats::setNames(phengaris_lines(length(exponents)), exponents)
  shapes <- stats::setNames(c(16, 17, 15, 18, 4, 3)[seq_along(exponents)], exponents)
  profile <- profile %>%
    dplyr::mutate(
      sample = sample_factor(sample),
      distance_km = 1 / alpha_per_km,
      b = factor(area_exponent, levels = exponents)
    )
  best <- profile %>%
    dplyr::group_by(sample) %>%
    dplyr::slice_min(delta_AIC, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  ggplot2::ggplot(profile, ggplot2::aes(x = distance_km, y = delta_AIC, colour = b, shape = b)) +
    ggplot2::geom_hline(yintercept = 2, colour = REFERENCE_GREY, linewidth = 0.35, linetype = "dashed") +
    ggplot2::geom_line(linewidth = 0.45) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::geom_point(data = best, shape = 1, size = 3.8, colour = phengaris_col("spot_black"), stroke = 0.6) +
    ggplot2::facet_wrap(~sample, scales = "free_y") +
    ggplot2::scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1, 2, 4), labels = format_tick) +
    ggplot2::scale_colour_manual(values = cols, name = "Area exponent b") +
    ggplot2::scale_shape_manual(values = shapes, name = "Area exponent b") +
    ggplot2::labs(
      x = "Mean dispersal distance 1 / alpha (km, log scale)",
      y = species_title("Delta AIC from the best pair,", species_label)
    ) +
    theme_phengaris()
}

#--------------------------------------------------#
## Step 19: threats -----
#--------------------------------------------------#

#' Odds ratios of the probability of recording the species where each
#' threat class is noted; filled marks pass the multiplicity adjustment.
#'
#' @param estimates  species, sample, term (the class), ratio, ratio_low,
#'                   ratio_high, p_adjusted
plot_threat_estimates <- function(estimates, alpha = 0.05) {
  classes <- names(CONFIG$strukt_pozn$threat_article17)
  classes <- c(intersect(classes, estimates$term), setdiff(unique(estimates$term), classes))
  rows <- estimates %>%
    dplyr::transmute(
      species = species_short(species), sample = sample_factor(sample),
      threat = factor(threat_label(term), levels = rev(threat_label(classes))),
      odds_ratio = ratio, or_low = ratio_low, or_high = ratio_high,
      low = ratio_low, high = ratio_high,
      passes = !is.na(p_adjusted) & p_adjusted < alpha
    )
  rows <- rows[is_estimable(rows), , drop = FALSE]

  ggplot2::ggplot(rows, ggplot2::aes(x = odds_ratio, y = threat, group = sample)) +
    ratio_reference() +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = low, xmax = high, colour = sample),
      position = ggplot2::position_dodge(width = 0.7), linewidth = 0.45
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = sample, shape = sample, fill = ifelse(passes, as.character(sample), "none")),
      position = ggplot2::position_dodge(width = 0.7), size = 1.9, stroke = 0.6
    ) +
    ggplot2::facet_wrap(~species, scales = "free_x") +
    ggplot2::scale_colour_manual(values = sample_colours(), name = NULL) +
    ggplot2::scale_shape_manual(values = sample_shapes(), name = NULL) +
    ggplot2::scale_fill_manual(values = c(sample_colours(), none = "white"), guide = "none") +
    scale_x_ratio() +
    ggplot2::labs(y = NULL) +
    theme_phengaris(grid = "x") +
    theme_species_strips() +
    ggplot2::guides(
      colour = ggplot2::guide_legend(override.aes = list(fill = unname(sample_colours())))
    )
}

#--------------------------------------------------#
## Step 21 and 22: the landscape -----
#--------------------------------------------------#

#' The radius axis of the landscape figures, in km on a log scale.
scale_x_radius <- function(radii_m) {
  ggplot2::scale_x_log10(
    name = "Radius (km, log scale)", breaks = radii_m / 1000, labels = format_tick
  )
}

#' Shares of each land cover in the circles around the surveyed sites, by
#' radius.
#'
#' @param long  ID_LOKAL, variable, radius_m, share
plot_landscape_shares <- function(long) {
  long <- dplyr::mutate(
    long,
    variable = landscape_factor(variable),
    radius = factor(format_tick(radius_m / 1000), levels = format_tick(sort(unique(radius_m)) / 1000))
  )
  ggplot2::ggplot(long, ggplot2::aes(x = radius, y = share)) +
    ggplot2::geom_boxplot(
      outlier.size = 0.25, outlier.colour = phengaris_col("teleius_blue"), colour = phengaris_col("deep_blue"),
      fill = phengaris_col("teleius_pale"), linewidth = 0.35, width = 0.65
    ) +
    ggplot2::facet_wrap(~variable, scales = "free_y", ncol = 4) +
    # The short radii are close together: labels that would overlap are left out.
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
    ggplot2::labs(x = "Radius (km)", y = "Share of the circle") +
    theme_phengaris(grid = "y")
}

#' Scale of effect of each landscape variable: delta AIC of the management
#' model with the variable at each radius against the model without it; the
#' best radius marked, with its odds ratio per 10 percentage points.
#'
#' @param scales    22_<species>_landscape_scales
#' @param min_gain  the AIC gain a variable needs to enter the combined model
plot_landscape_scales <- function(scales, min_gain, species_label) {
  scales <- dplyr::mutate(scales, variable = landscape_factor(variable), radius_km = radius_m / 1000)
  best <- scales %>%
    dplyr::group_by(variable) %>%
    dplyr::slice_min(delta_AIC_vs_management, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      enters = delta_AIC_vs_management < -min_gain,
      label = sprintf("%.2f", odds_ratio)
    )

  ggplot2::ggplot(scales, ggplot2::aes(x = radius_km, y = delta_AIC_vs_management)) +
    ggplot2::geom_hline(yintercept = 0, colour = REFERENCE_GREY, linewidth = 0.35) +
    ggplot2::geom_hline(yintercept = -min_gain, colour = REFERENCE_GREY, linewidth = 0.35, linetype = "dashed") +
    ggplot2::geom_line(colour = phengaris_col("deep_blue"), linewidth = 0.45) +
    ggplot2::geom_point(colour = phengaris_col("deep_blue"), size = 1.1) +
    ggplot2::geom_point(
      data = best, ggplot2::aes(fill = enters), shape = 21, size = 2.6,
      colour = phengaris_col("spot_black"), stroke = 0.6
    ) +
    ggplot2::geom_text(
      data = best, ggplot2::aes(label = label), vjust = 1.9, hjust = 0.7, size = FIGURE_TEXT_MM,
      colour = phengaris_col("spot_black")
    ) +
    ggplot2::facet_wrap(~variable, ncol = 4) +
    scale_x_radius(sort(unique(scales$radius_m))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.15, 0.05))) +
    ggplot2::scale_fill_manual(
      values = c(`TRUE` = phengaris_col("deep_blue"), `FALSE` = "white"),
      breaks = c(TRUE, FALSE),
      labels = c(`TRUE` = "enters the combined model", `FALSE` = "stays out of it"),
      name = "Best radius"
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(y = species_title("Delta AIC against the management model,", species_label)) +
    theme_phengaris()
}

#--------------------------------------------------#
## Step 23: detection -----
#--------------------------------------------------#

#' The key odds ratios of the management model as detection is allowed
#' for: the GLMM of step 17, GLMs with the detection probability fixed at
#' p, and the occupancy part of the mixed-design model.
#'
#' @param rows  fit, sample, term, odds_ratio, or_low, or_high
#' @param fits  the fits in their order, top to bottom
plot_detection_sensitivity <- function(rows, fits, species_label) {
  rows <- rows %>%
    dplyr::filter(term %in% names(SYNTHESIS_TERMS)) %>%
    dplyr::mutate(
      term = term_factor(term), sample = sample_factor(sample),
      fit = factor(fit, levels = rev(fits)), low = or_low, high = or_high
    )
  rows <- rows[is_estimable(rows), , drop = FALSE]
  ggplot2::ggplot(rows, ggplot2::aes(x = odds_ratio, y = fit, group = sample)) +
    ratio_reference() +
    sample_pointrange() +
    ggplot2::facet_wrap(~term, scales = "free_x", ncol = 3, labeller = ggplot2::label_wrap_gen(24)) +
    scale_sample() +
    scale_x_ratio() +
    ggplot2::labs(y = NULL, title = species_title("", species_label)) +
    theme_phengaris(grid = "x")
}

#' Detection probability at the year's median day, per field-form era and
#' model (posterior mean and 95% credible interval), against the prior mean.
#'
#' @param detection  23_<species>_detection_probability
plot_detection_probability <- function(detection, prior_p, species_label) {
  detection <- detection %>%
    dplyr::mutate(
      sample = sample_factor(sub(",.*$", "", model)),
      prior = sub("^[^,]*, ", "", model),
      form = sub("^p at the median day, ", "", quantity),
      form = factor(form, levels = c(sort(unique(form), decreasing = TRUE))),
      model = factor(model, levels = rev(unique(model))),
      low = q2.5, high = q97.5
    )
  ggplot2::ggplot(detection, ggplot2::aes(x = mean, y = model, group = sample)) +
    ggplot2::geom_vline(xintercept = prior_p, colour = REFERENCE_GREY, linewidth = 0.35, linetype = "dashed") +
    sample_pointrange() +
    ggplot2::facet_wrap(~form) +
    scale_sample() +
    ggplot2::scale_x_continuous(limits = c(0, 1), name = "Detection probability at the median day") +
    ggplot2::labs(y = NULL, title = species_title("", species_label)) +
    theme_phengaris(grid = "x")
}

#--------------------------------------------------#
## Step 24: Natura 2000 -----
#--------------------------------------------------#

#' Odds ratio of recording the species inside an EVL designated for it,
#' unmatched, covariate-adjusted and after matching.
#'
#' @param designation  24_<species>_designation
plot_designation <- function(designation, species_label) {
  models <- unique(designation$outcome_model)
  rows <- designation %>%
    dplyr::mutate(
      sample = sample_factor(sample), outcome_model = factor(outcome_model, levels = rev(models)),
      low = or_low, high = or_high
    )
  ggplot2::ggplot(rows, ggplot2::aes(x = odds_ratio, y = outcome_model, group = sample)) +
    ratio_reference() +
    sample_pointrange() +
    scale_sample() +
    scale_x_ratio() +
    ggplot2::labs(y = NULL, title = species_title("", species_label)) +
    theme_phengaris(grid = "x")
}

#' Share of the visits that found the species, at sites with and without a
#' population known in 2012-2018, inside and outside the EVLs designated for
#' the species.
#'
#' @param counts  24_<species>_persistence_counts
plot_persistence <- function(counts, species_label) {
  rows <- counts %>%
    dplyr::mutate(
      sample = sample_factor(sample),
      known = factor(
        known_2012_2018,
        levels = c(FALSE, TRUE), labels = c("no record 2012-2018", "recorded 2012-2018")
      ),
      evl = factor(inside_evl, levels = c(FALSE, TRUE), labels = c("outside the EVL", "inside the EVL"))
    ) %>%
    dplyr::bind_cols(jeffreys_interval(counts$positive, counts$visits))
  ggplot2::ggplot(rows, ggplot2::aes(x = share_positive, y = evl, group = sample)) +
    sample_pointrange() +
    ggplot2::facet_wrap(~known, ncol = 1) +
    scale_sample() +
    ggplot2::scale_x_continuous(
      limits = c(0, 1), name = "Share of visits that found the species\n(95% interval)"
    ) +
    ggplot2::labs(y = NULL, title = species_title("", species_label)) +
    theme_phengaris(grid = "x") +
    # A single-column figure: the samples one below the other.
    ggplot2::theme(legend.direction = "vertical")
}

#--------------------------------------------------#
## Step 25: robustness -----
#--------------------------------------------------#

#' What each analysis of the synthesis changes against the management
#' model, for the rows of the robustness figure.
ROBUSTNESS_GROUPS <- c(
  "management model (step 17)" = "reference",
  "+ connectivity" = "covariates",
  "2021-2024 polygons, + grazing and area" = "records",
  "window of 28 days" = "records",
  "window of 45 days" = "records",
  "+ habitat and landscape" = "covariates",
  "SPDE spatial field (step 18)" = "space",
  "spatial+ (step 18)" = "space",
  "known detection p = 0.7 (step 23)" = "detection",
  "occupancy model, mixed design (step 23)" = "detection",
  "EVL: covariate-adjusted (step 24)" = "matching",
  "EVL: matched, cluster-robust GLM (step 24)" = "matching"
)

#' The odds ratios of the key terms across the analyses of steps 17-24,
#' grouped by what each analysis changes; one column per term.
#'
#' @param robustness  collect_robustness()
plot_robustness <- function(robustness) {
  # The step is shown by the group, so the labels drop it.
  short <- function(analysis) sub(" \\(step [0-9]+\\)$", "", analysis)
  groups <- unique(c(unname(ROBUSTNESS_GROUPS), "other"))
  rows <- robustness[is_estimable(robustness), , drop = FALSE] %>%
    dplyr::mutate(
      group = factor(
        ifelse(analysis %in% names(ROBUSTNESS_GROUPS), ROBUSTNESS_GROUPS[analysis], "other"),
        levels = groups
      ),
      analysis = factor(short(analysis), levels = rev(unique(short(robustness$analysis)))),
      term = term_factor(term),
      sample = sample_factor(sample),
      low = or_low, high = or_high
    )
  ggplot2::ggplot(rows, ggplot2::aes(x = odds_ratio, y = analysis, group = sample)) +
    ratio_reference() +
    sample_pointrange(width = 0.7, size = 0.22) +
    ggplot2::facet_grid(
      group ~ term,
      scales = "free", space = "free_y", switch = "y",
      labeller = ggplot2::labeller(term = ggplot2::label_wrap_gen(14))
    ) +
    scale_sample() +
    scale_x_ratio(max_breaks = 3) +
    ggplot2::scale_y_discrete(labels = scales::label_wrap(22)) +
    ggplot2::labs(y = NULL) +
    theme_phengaris(grid = "x") +
    ggplot2::theme(
      strip.placement = "outside",
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, face = "italic", colour = REFERENCE_GREY),
      strip.text.x = ggplot2::element_text(face = "bold", hjust = 0, size = ggplot2::rel(0.9)),
      panel.spacing.x = ggplot2::unit(0.6, "lines"),
      panel.spacing.y = ggplot2::unit(0.4, "lines")
    )
}

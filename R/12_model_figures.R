#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#              Step 12 - Model-related figures
#
#
#----------------------------------------------------------#
#
# The figures that accompany the models: the distribution of site area by
# occupancy and host plant abundance, and the distribution of counted
# specimens between the species.
#
# The bar charts of management, protection and host plant that the original
# model script also drew are not repeated here, because step 08 produces them
# from the same summary tables.
#
# Reads:  Data/Processed/<spec>/data_analysis.csv   (step 06)
# Writes: Outputs/<spec>/Figures/12_*.png
#         Outputs/<spec>/Reports/12_model_figures.md
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("12")

report_start(
  "12",
  "Model figures",
  paste(
    "Distributions behind the habitat and abundance models: site area by",
    "occupancy and host plant abundance, and counted specimens by species."
  )
)

data <- read_data_analysis()

#----------------------------------------------------------#
# Site area by occupancy and host plant abundance -----
#----------------------------------------------------------#

#' Boxplot of site area by occupancy, split by host plant abundance.
plot_area_by_plant <- function(species, species_label) {
  ggplot2::ggplot(
    data = data %>%
      dplyr::filter(DRUH == species) %>%
      dplyr::filter(AREA_SITE > 0),
    ggplot2::aes(
      x    = as.factor(POSITIVE),
      y    = log10(AREA_SITE),
      fill = as.factor(PLANT_QUANT)
    )
  ) +
    ggplot2::geom_boxplot(
      colour = phengaris_col("spot_black"), linewidth = 0.3, outlier.size = 0.4, width = 0.7
    ) +
    scale_y_count() +
    ggplot2::scale_x_discrete(labels = c("vacant sites", "occupied sites")) +
    # Host plant abundance is ordered: lighter to darker blue.
    ggplot2::scale_fill_manual(
      values = phengaris_fills(3, begin = 0.2),
      labels = c("single plants", "abundant", "dominant"),
      name   = "Host plant"
    ) +
    ggplot2::labs(
      x = species_title("Occupancy of the sites,", species_label),
      y = "Site area (m², log10)"
    ) +
    theme_phengaris(grid = "y")
}

if (any(!is.na(data$AREA_SITE))) {
  report_figure(
    plot_area_by_plant(SPECIES_NAU, "P. nausithous"),
    "Site area by occupancy and host plant abundance, P. nausithous",
    "12_area_by_plant_nausithous",
    size = "single", height = 8
  )

  report_figure(
    plot_area_by_plant(SPECIES_TEL, "P. teleius"),
    "Site area by occupancy and host plant abundance, P. teleius",
    "12_area_by_plant_teleius",
    size = "single", height = 8
  )
} else {
  report_warning(
    "Site area is not available in the analysis table, so the two site-area",
    "figures were not produced. Run steps 05 and 06 with access to the",
    "habitat mapping share."
  )
}

#----------------------------------------------------------#
# Counted specimens -----
#----------------------------------------------------------#

report_figure(
  ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x    = as.factor(DRUH),
      y    = log(POCET),
      fill = as.factor(DRUH)
    )
  ) +
    ggplot2::geom_boxplot(
      colour = phengaris_col("spot_black"), linewidth = 0.3, outlier.size = 0.4, width = 0.6
    ) +
    scale_y_count() +
    ggplot2::scale_x_discrete(labels = function(x) italic_labels(species_short(x))) +
    scale_fill_species(guide = "none") +
    ggplot2::labs(x = NULL, y = "Counted specimens (natural log)") +
    theme_phengaris(grid = "y"),
  "Counted specimens by species",
  "12_specimen_counts_by_species",
  size = "single"
)

report_figure(
  ggplot2::ggplot(
    data = data %>%
      dplyr::filter(is.na(POCET) == FALSE),
    ggplot2::aes(x = POCET)
  ) +
    ggplot2::geom_histogram(bins = 30, fill = phengaris_col("deep_blue"), colour = "white", linewidth = 0.2) +
    scale_y_count() +
    ggplot2::labs(x = "Counted specimens", y = "Number of records") +
    theme_phengaris(grid = "y"),
  "Distribution of counted specimens",
  "12_specimen_count_distribution",
  size = "single"
)

report_note(paste(
  "The y axis label of the specimen boxplot read \"log10(site area)\" in the",
  "original script, which was left over from the site-area figure above it.",
  "It now names what is actually plotted."
))

report_finish()

step_end("12")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

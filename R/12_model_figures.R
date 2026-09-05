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
# Reads:  Data/Processed/data_analysis.csv   (step 06)
# Writes: Outputs/Figures/12_*.png
#         Outputs/Reports/12_model_figures.md
#
#----------------------------------------------------------#

message("Step 12: model-related figures")

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
    ggplot2::geom_boxplot() +
    scale_y_count() +
    ggplot2::scale_x_discrete(labels = c("vacant sites", "occupied sites")) +
    ggplot2::scale_fill_discrete(
      labels = c("single plants", "abundant", "dominant"),
      name   = NULL
    ) +
    ggplot2::xlab(paste0("\noccupancy of ", species_label, " sites")) +
    ggplot2::ylab("log10(site area)\n") +
    ggplot2::theme_classic(base_size = 16)
}

if (any(!is.na(data$AREA_SITE))) {

  report_figure(
    plot_area_by_plant(SPECIES_NAU, "P. nausithous"),
    "Site area by occupancy and host plant abundance, P. nausithous",
    "12_area_by_plant_nausithous",
    width = 9
  )

  report_figure(
    plot_area_by_plant(SPECIES_TEL, "P. teleius"),
    "Site area by occupancy and host plant abundance, P. teleius",
    "12_area_by_plant_teleius",
    width = 9
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
      x = as.factor(DRUH),
      y = log(POCET)
    )
  ) +
    ggplot2::geom_boxplot() +
    scale_y_count() +
    ggplot2::xlab("") +
    ggplot2::ylab("log(counted specimens)\n") +
    ggplot2::theme_classic(base_size = 18),
  "Counted specimens by species",
  "12_specimen_counts_by_species"
)

report_figure(
  ggplot2::ggplot(
    data = data %>%
      dplyr::filter(is.na(POCET) == FALSE),
    ggplot2::aes(x = POCET)
  ) +
    ggplot2::geom_histogram(bins = 30, fill = "#595959") +
    scale_y_count() +
    ggplot2::xlab("\ncounted specimens") +
    ggplot2::ylab("number of records\n") +
    ggplot2::theme_classic(base_size = 14),
  "Distribution of counted specimens",
  "12_specimen_count_distribution"
)

report_note(paste(
  "The y axis label of the specimen boxplot read \"log10(site area)\" in the",
  "original script, which was left over from the site-area figure above it.",
  "It now names what is actually plotted."
))

report_finish()

message("Step 12 done")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#             Step 08 - Descriptive figures
#
#
#----------------------------------------------------------#
#
# The descriptive figures for the manuscript. Each one plots a table that step
# 07 also writes to CSV, so figure and table can be checked against each other.
#
# Reads:  Data/Processed/<spec>/data_analysis.csv   (step 06)
# Writes: Outputs/<spec>/Figures/08_*.png
#         Outputs/<spec>/Reports/08_descriptive_figures.md
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("08")

# A specification with the survey-design variables is described as its
# models see the records (step 07, "The records of the revised models"). The
# figures of the original analysis describe variables the revised models do
# not use (the Natura 2000 designation for either species, the combined
# management classes, all records regardless of the flight season) and are
# drawn for the other specifications.
revised_figures <- isTRUE(spec_settings()$design_variables)

report_start(
  "08",
  "Descriptive figures",
  if (revised_figures) {
    paste(
      "The records the revised models use: in the flight season, by outcome,",
      "by position in the season, and the share of records with the species",
      "at each value of the management model's variables in the three samples",
      "(with all imputed absences, with the plausible ones, observed records",
      "only). Each figure plots a table of step 07."
    )
  } else {
    paste(
      "Figures describing the monitoring effort and the distribution of the",
      "analysis variables. Site occupancy is shown with the same two greys",
      "throughout: light grey for records without the species, dark grey for",
      "records with it."
    )
  }
)

data <- read_data_analysis()

#----------------------------------------------------------#
# Figures of the revised specification -----
#----------------------------------------------------------#

if (revised_figures) {
  report_section("The records of the revised models")

  report_figure(
    plot_records_by_outcome(summarise_records_by_outcome(data)),
    "Records in the flight season by year and outcome",
    "08_records_by_outcome",
    width = 10, height = 5
  )

  report_figure(
    plot_visit_timing(summarise_visit_timing(data)),
    paste(
      "Observed visits by their position in the season (5-day classes; the",
      "vertical line is the year's median positive record)"
    ),
    "08_visit_timing",
    width = 8, height = 6
  )

  report_figure(
    plot_share_positive(summarise_share_positive(data)),
    paste(
      "Share of the records in the flight season with the species at each value",
      "of the management model's variables, by sample (Jeffreys 95% interval)"
    ),
    "08_share_positive",
    width = 11, height = 6.5
  )

  report_section("Observers")

  report_figure(
    plot_observers(summarise_observers(data)),
    "Records per observer",
    "08_observer_distribution",
    width = 7, height = 4.5
  )
} else {
  # The figures of the original analysis, for the other specifications.

  #----------------------------------------------------------#
  # Shared plot builders -----
  #----------------------------------------------------------#

  #' Grouped bar chart of counts split by site occupancy.
  #'
  #' @param x         summary table with COUNT and POSITIVE
  #' @param x_var     name of the column on the x axis
  #' @param x_labels  axis tick labels
  #' @param x_title,y_title  axis titles
  plot_counts_by_occupancy <- function(x, x_var, x_labels, x_title, y_title,
                                       base_size = 14) {
    ggplot2::ggplot(
      data = x,
      ggplot2::aes(
        x    = as.factor(.data[[x_var]]),
        y    = COUNT,
        fill = as.factor(POSITIVE)
      )
    ) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      scale_y_count() +
      scale_fill_occupancy() +
      ggplot2::scale_x_discrete(labels = x_labels) +
      ggplot2::xlab(x_title) +
      ggplot2::ylab(y_title) +
      ggplot2::theme_classic(base_size = base_size)
  }

  #' Bar chart of counts from a long summary table.
  plot_counts_ranked <- function(x, x_title, y_title, base_size = 16) {
    ggplot2::ggplot(
      data = x,
      ggplot2::aes(
        x = forcats::fct_reorder(name, COUNT, .desc = TRUE),
        y = COUNT
      )
    ) +
      ggplot2::geom_bar(stat = "identity", fill = "#595959") +
      scale_y_count() +
      ggplot2::xlab(x_title) +
      ggplot2::ylab(y_title) +
      ggplot2::theme_classic(base_size = base_size)
  }

  #----------------------------------------------------------#
  # General and temporal -----
  #----------------------------------------------------------#

  report_section("General and temporal")

  report_figure(
    plot_counts_by_occupancy(
      summarise_occupancy(data),
      "DRUH",
      c("P. nausithous", "P. teleius"),
      "\nSpecies",
      "number of sites\n"
    ) +
      ggplot2::theme(legend.position = "top"),
    "Site occupancy by species",
    "08_site_occupancy_by_species"
  )

  year_stats <- summarise_year_stats(data)

  report_figure(
    ggplot2::ggplot(
      data = year_stats,
      ggplot2::aes(
        y     = as.numeric(COUNT),
        x     = as.factor(YEAR),
        color = as.factor(POSITIVE)
      )
    ) +
      ggplot2::geom_point(size = 3, alpha = 0.7) +
      ggplot2::labs(x = "\nYear", y = "Observation Count\n", color = "Site Occupancy") +
      scale_colour_occupancy(name = "Site Occupancy") +
      ggplot2::theme_classic(base_size = 14) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)),
    "Records per year and site occupancy",
    "08_records_by_year"
  )

  #----------------------------------------------------------#
  # Observers -----
  #----------------------------------------------------------#

  report_section("Observers")

  observer_stats <- summarise_observers(data)
  observer_mean <- mean(observer_stats$obs_num, na.rm = TRUE)

  report_figure(
    ggplot2::ggplot(
      observer_stats,
      ggplot2::aes(x = obs_num)
    ) +
      ggplot2::geom_histogram(
        alpha  = 0.6,
        breaks = seq(0, max(observer_stats$obs_num, na.rm = TRUE), by = 25),
        fill   = "steelblue"
      ) +
      ggplot2::theme_minimal(base_size = 14) +
      scale_y_count() +
      ggplot2::geom_vline(
        xintercept = observer_mean,
        linetype   = "dotted",
        colour     = "steelblue",
        linewidth  = 1
      ) +
      ggplot2::annotate(
        "text",
        x     = observer_mean + 220,
        y     = 20,
        label = paste0("Mean observations: ", round(observer_mean, 1)),
        size  = 5
      ) +
      ggplot2::labs(
        x     = "\nNumber of Observations",
        y     = "Number of Observers\n",
        title = "Distribution of Observation Counts per Observer"
      ),
    "Distribution of record counts per observer",
    "08_observer_distribution"
  )

  #----------------------------------------------------------#
  # Occurrence patterns -----
  #----------------------------------------------------------#

  report_section("Occurrence patterns")

  report_figure(
    plot_counts_by_occupancy(
      summarise_species_cooccurrence(data) %>%
        dplyr::filter(SPEC_NUM == 0),
      "DRUH",
      c("P. nausithous\n(without P. teleius)", "P. teleius\n(without P. nausithous)"),
      "\nSpecies Occurrence Context",
      "number of sites\n"
    ),
    "Records of each species without the other species present",
    "08_species_occurrence_context"
  )

  #----------------------------------------------------------#
  # Host plant -----
  #----------------------------------------------------------#

  report_section("Host plant")

  data_plant_sum <- summarise_host_plant(data)

  report_figure(
    plot_counts_by_occupancy(
      data_plant_sum %>% dplyr::filter(DRUH == SPECIES_NAU),
      "PLANT_QUANT",
      c("single plants", "abundant", "dominant"),
      "\nHost Plant Quantity (P. nausithous)",
      "number of findings\n"
    ),
    "Host plant abundance, P. nausithous",
    "08_host_plant_nausithous"
  )

  report_figure(
    plot_counts_by_occupancy(
      data_plant_sum %>% dplyr::filter(DRUH == SPECIES_TEL),
      "PLANT_QUANT",
      c("single plants", "abundant", "dominant"),
      "\nHost Plant Quantity (P. teleius)",
      "number of sites\n"
    ),
    "Host plant abundance, P. teleius",
    "08_host_plant_teleius"
  )

  #----------------------------------------------------------#
  # Recorded habitats -----
  #----------------------------------------------------------#

  report_section("Recorded habitats")

  report_figure(
    plot_counts_ranked(
      summarise_recorded_habitats(data, SPECIES_NAU),
      "\nrecorded habitats preferred by P. nausithous",
      "number of sites\n",
      base_size = 14
    ),
    "Recorded habitats at occupied P. nausithous sites",
    "08_recorded_habitats_nausithous"
  )

  report_figure(
    plot_counts_ranked(
      summarise_recorded_habitats(data, SPECIES_TEL),
      "\nrecorded habitats preferred by P. teleius",
      "number of sites\n",
      base_size = 14
    ),
    "Recorded habitats at occupied P. teleius sites",
    "08_recorded_habitats_teleius"
  )

  #----------------------------------------------------------#
  # Management -----
  #----------------------------------------------------------#

  report_section("Management")

  data_man_sum <- summarise_management(data)

  management_labels <- c(
    "appropriate mow\n& timing",
    "appropriate\nmow only",
    "appropriate\ntiming only",
    "inappropriate mow\n& timing"
  )

  report_figure(
    plot_counts_by_occupancy(
      data_man_sum %>%
        dplyr::filter(DRUH == SPECIES_NAU, !is.na(MANAGEMENT)),
      "MANAGEMENT",
      management_labels,
      "\nRecorded management at sites with P. nausithous",
      "number of findings\n"
    ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12)),
    "Management appropriateness, P. nausithous",
    "08_management_appropriateness_nausithous",
    width = 9
  )

  report_figure(
    plot_counts_by_occupancy(
      data_man_sum %>%
        dplyr::filter(DRUH == SPECIES_TEL, !is.na(MANAGEMENT)),
      "MANAGEMENT",
      management_labels,
      "\nRecorded management at sites with P. teleius",
      "number of findings\n"
    ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12)),
    "Management appropriateness, P. teleius",
    "08_management_appropriateness_teleius",
    width = 9
  )

  report_figure(
    plot_counts_ranked(
      summarise_management_types(data, SPECIES_NAU),
      "\nManagement recorded at sites with P. nausithous",
      "number of sites\n"
    ),
    "Management types, P. nausithous",
    "08_management_types_nausithous"
  )

  report_figure(
    plot_counts_ranked(
      summarise_management_types(data, SPECIES_TEL),
      "\nManagement recorded at sites with P. teleius",
      "number of sites\n"
    ),
    "Management types, P. teleius",
    "08_management_types_teleius"
  )

  #----------------------------------------------------------#
  # Protected areas -----
  #----------------------------------------------------------#

  report_section("Protected areas")

  data_evl_sum <- summarise_protection(data)
  data_evltar_sum <- summarise_evl_target(data)
  data_evlcomb_sum <- summarise_evl_combined(data)
  data_mzchu_sum <- summarise_mzchu(data)

  report_figure(
    plot_counts_by_occupancy(
      data_evl_sum %>% dplyr::filter(DRUH == SPECIES_NAU),
      "EVL",
      c("outside Natura 2000", "Natura 2000 sites"),
      "\nPhengaris nausithous",
      "number of sites\n",
      base_size = 18
    ),
    "Natura 2000 membership, P. nausithous",
    "08_protection_evl_nausithous"
  )

  report_figure(
    plot_counts_by_occupancy(
      data_evl_sum %>% dplyr::filter(DRUH == SPECIES_TEL),
      "EVL",
      c("outside Natura 2000", "within Natura 2000"),
      "\nPhengaris teleius",
      "number of findings\n",
      base_size = 18
    ),
    "Natura 2000 membership, P. teleius",
    "08_protection_evl_teleius"
  )

  report_figure(
    plot_counts_by_occupancy(
      data_evltar_sum %>% dplyr::filter(DRUH == SPECIES_NAU),
      "EVL_target",
      c(
        "outside Natura 2000\ndesignated for Phengaris",
        "within Natura 2000\ndesignated for Phengaris"
      ),
      "\nPhengaris nausithous",
      "number of findings\n"
    ),
    "Natura 2000 designation, P. nausithous",
    "08_protection_evl_target_nausithous"
  )

  report_figure(
    plot_counts_by_occupancy(
      data_evltar_sum %>% dplyr::filter(DRUH == SPECIES_TEL),
      "EVL_target",
      c(
        "outside Natura 2000\ndesignated for Phengaris",
        "within Natura 2000\ndesignated for Phengaris"
      ),
      "\nPhengaris teleius",
      "number of findings\n"
    ),
    "Natura 2000 designation, P. teleius",
    "08_protection_evl_target_teleius"
  )

  report_figure(
    plot_counts_by_occupancy(
      data_evlcomb_sum %>% dplyr::filter(DRUH == SPECIES_NAU),
      "EVL_comb",
      c(
        "outside Natura 2000",
        "Natura 2000 NOT designated\nfor Phengaris",
        "Natura 2000 designated\nfor Phengaris"
      ),
      "\nPhengaris nausithous",
      "number of findings\n"
    ),
    "Combined Natura 2000 status, P. nausithous",
    "08_protection_evl_combined_nausithous"
  )

  report_figure(
    plot_counts_by_occupancy(
      data_evlcomb_sum %>% dplyr::filter(DRUH == SPECIES_TEL),
      "EVL_comb",
      c(
        "outside Natura 2000",
        "Natura 2000 NOT designated\nfor Phengaris",
        "Natura 2000 designated\nfor Phengaris"
      ),
      "\nPhengaris teleius",
      "number of sites\n"
    ),
    "Combined Natura 2000 status, P. teleius",
    "08_protection_evl_combined_teleius"
  )

  mzchu_labels <- c(
    "outside small-scale\nprotected site",
    "within small-scale\nprotected sites"
  )

  report_figure(
    plot_counts_by_occupancy(
      data_mzchu_sum %>% dplyr::filter(DRUH == SPECIES_NAU),
      "MZCHU",
      mzchu_labels,
      "\nPhengaris nausithous",
      "number of findings\n",
      base_size = 16
    ),
    "Small-scale protected areas, P. nausithous",
    "08_protection_mzchu_nausithous"
  )

  report_figure(
    plot_counts_by_occupancy(
      data_mzchu_sum %>% dplyr::filter(DRUH == SPECIES_TEL),
      "MZCHU",
      mzchu_labels,
      "\nPhengaris teleius",
      "number of findings\n",
      base_size = 16
    ),
    "Small-scale protected areas, P. teleius",
    "08_protection_mzchu_teleius"
  )
}

report_finish()

step_end("08")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

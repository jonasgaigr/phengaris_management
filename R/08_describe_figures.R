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
      "analysis variables. Site occupancy is shown with the same two colours",
      "throughout: buff for records without the species, deep blue for",
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
    height = 8.5
  )

  report_figure(
    plot_visit_timing(summarise_visit_timing(data)),
    paste(
      "Observed visits by their position in the season (5-day classes; the",
      "vertical line is the year's median positive record)"
    ),
    "08_visit_timing",
    size = "single", height = 9
  )

  report_figure(
    plot_share_positive(summarise_share_positive(data)),
    paste(
      "Share of the records in the flight season with the species at each value",
      "of the management model's variables, by sample (Jeffreys 95% interval)"
    ),
    "08_share_positive",
    height = 11
  )

  report_section("Observers")

  report_figure(
    plot_observers(summarise_observers(data)),
    "Records per observer",
    "08_observer_distribution",
    size = "single", height = 6
  )
} else {
  # The figures of the original analysis, for the other specifications.

  #----------------------------------------------------------#
  # Shared plot builders -----
  #----------------------------------------------------------#

  #' Grouped bar chart of counts split by site occupancy. The categories keep
  #' their own order (they are ordinal or a fixed classification).
  #'
  #' @param x         summary table with COUNT and POSITIVE
  #' @param x_var     name of the column on the x axis
  #' @param x_labels  axis tick labels (a function for the species names)
  #' @param x_title,y_title  axis titles
  plot_counts_by_occupancy <- function(x, x_var, x_labels, x_title, y_title) {
    ggplot2::ggplot(
      data = x,
      ggplot2::aes(
        x    = as.factor(.data[[x_var]]),
        y    = COUNT,
        fill = as.factor(POSITIVE)
      )
    ) +
      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
      scale_y_count() +
      scale_fill_occupancy(name = "Site occupancy") +
      ggplot2::scale_x_discrete(labels = x_labels) +
      ggplot2::xlab(x_title) +
      ggplot2::ylab(y_title) +
      theme_phengaris(grid = "y")
  }

  #' Horizontal bar chart of counts from a long summary table, the largest on
  #' top. The category title is short; the count title names the species.
  plot_counts_ranked <- function(x, category_title, count_title) {
    ggplot2::ggplot(
      data = x,
      ggplot2::aes(
        x = COUNT,
        y = forcats::fct_reorder(name, COUNT)
      )
    ) +
      ggplot2::geom_bar(stat = "identity", fill = phengaris_col("deep_blue"), width = 0.75) +
      scale_x_count() +
      ggplot2::labs(x = count_title, y = category_title) +
      theme_phengaris(grid = "x")
  }

  #' "Host plant abundance," + italic species name, for an axis title.
  species_axis <- function(text, species) species_title(text, species_short(species))

  #' The species name alone, in italics, for an axis title.
  species_only <- function(species) bquote(italic(.(species)))

  #----------------------------------------------------------#
  # General and temporal -----
  #----------------------------------------------------------#

  report_section("General and temporal")

  report_figure(
    plot_counts_by_occupancy(
      summarise_occupancy(data),
      "DRUH",
      function(x) italic_labels(species_short(x)),
      "Species",
      "Number of sites"
    ),
    "Site occupancy by species",
    "08_site_occupancy_by_species",
    size = "single"
  )

  year_stats <- summarise_year_stats(data)

  report_figure(
    ggplot2::ggplot(
      data = year_stats,
      ggplot2::aes(
        y     = as.numeric(COUNT),
        x     = as.factor(YEAR),
        color = as.factor(POSITIVE),
        shape = as.factor(POSITIVE)
      )
    ) +
      ggplot2::geom_point(size = 1.8, stroke = 0.6) +
      ggplot2::labs(x = "Year", y = "Records") +
      scale_colour_occupancy(name = "Site occupancy") +
      ggplot2::scale_shape_manual(values = c(1, 16), labels = OCCUPANCY_LABELS, name = "Site occupancy") +
      theme_phengaris(grid = "y") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)),
    "Records per year and site occupancy",
    "08_records_by_year",
    size = "single"
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
        breaks    = seq(0, max(observer_stats$obs_num, na.rm = TRUE), by = 25),
        fill      = phengaris_col("deep_blue"),
        colour    = "white",
        linewidth = 0.2
      ) +
      scale_y_count() +
      ggplot2::geom_vline(
        xintercept = observer_mean,
        linetype   = "dashed",
        colour     = phengaris_col("spot_black"),
        linewidth  = 0.4
      ) +
      ggplot2::annotate(
        "text",
        x     = observer_mean,
        y     = Inf,
        label = paste0("mean ", round(observer_mean, 1), " records"),
        hjust = -0.08,
        vjust = 1.5,
        size  = FIGURE_TEXT_MM
      ) +
      ggplot2::labs(x = "Records per observer", y = "Observers") +
      theme_phengaris(grid = "y"),
    "Distribution of record counts per observer",
    "08_observer_distribution",
    size = "single"
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
      c(
        expression(atop(italic("P. nausithous"), "(without" ~ italic("P. teleius") * ")")),
        expression(atop(italic("P. teleius"), "(without" ~ italic("P. nausithous") * ")"))
      ),
      "Species occurrence context",
      "Number of sites"
    ),
    "Records of each species without the other species present",
    "08_species_occurrence_context",
    size = "single"
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
      species_axis("Host plant quantity,", SPECIES_NAU),
      "Number of findings"
    ),
    "Host plant abundance, P. nausithous",
    "08_host_plant_nausithous",
    size = "single"
  )

  report_figure(
    plot_counts_by_occupancy(
      data_plant_sum %>% dplyr::filter(DRUH == SPECIES_TEL),
      "PLANT_QUANT",
      c("single plants", "abundant", "dominant"),
      species_axis("Host plant quantity,", SPECIES_TEL),
      "Number of sites"
    ),
    "Host plant abundance, P. teleius",
    "08_host_plant_teleius",
    size = "single"
  )

  #----------------------------------------------------------#
  # Recorded habitats -----
  #----------------------------------------------------------#

  report_section("Recorded habitats")

  report_figure(
    plot_counts_ranked(
      summarise_recorded_habitats(data, SPECIES_NAU),
      "Recorded habitat",
      species_axis("Number of sites,", SPECIES_NAU)
    ),
    "Recorded habitats at occupied P. nausithous sites",
    "08_recorded_habitats_nausithous",
    size = "single"
  )

  report_figure(
    plot_counts_ranked(
      summarise_recorded_habitats(data, SPECIES_TEL),
      "Recorded habitat",
      species_axis("Number of sites,", SPECIES_TEL)
    ),
    "Recorded habitats at occupied P. teleius sites",
    "08_recorded_habitats_teleius",
    size = "single"
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
      species_axis("Recorded management at sites with", SPECIES_NAU),
      "Number of findings"
    ),
    "Management appropriateness, P. nausithous",
    "08_management_appropriateness_nausithous",
    height = 7
  )

  report_figure(
    plot_counts_by_occupancy(
      data_man_sum %>%
        dplyr::filter(DRUH == SPECIES_TEL, !is.na(MANAGEMENT)),
      "MANAGEMENT",
      management_labels,
      species_axis("Recorded management at sites with", SPECIES_TEL),
      "Number of findings"
    ),
    "Management appropriateness, P. teleius",
    "08_management_appropriateness_teleius",
    height = 7
  )

  report_figure(
    plot_counts_ranked(
      summarise_management_types(data, SPECIES_NAU),
      "Recorded management",
      species_axis("Number of sites,", SPECIES_NAU)
    ),
    "Management types, P. nausithous",
    "08_management_types_nausithous",
    size = "single", height = 5
  )

  report_figure(
    plot_counts_ranked(
      summarise_management_types(data, SPECIES_TEL),
      "Recorded management",
      species_axis("Number of sites,", SPECIES_TEL)
    ),
    "Management types, P. teleius",
    "08_management_types_teleius",
    size = "single", height = 5
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
      species_only(SPECIES_NAU),
      "Number of sites"
    ),
    "Natura 2000 membership, P. nausithous",
    "08_protection_evl_nausithous",
    size = "single"
  )

  report_figure(
    plot_counts_by_occupancy(
      data_evl_sum %>% dplyr::filter(DRUH == SPECIES_TEL),
      "EVL",
      c("outside Natura 2000", "within Natura 2000"),
      species_only(SPECIES_TEL),
      "Number of findings"
    ),
    "Natura 2000 membership, P. teleius",
    "08_protection_evl_teleius",
    size = "single"
  )

  report_figure(
    plot_counts_by_occupancy(
      data_evltar_sum %>% dplyr::filter(DRUH == SPECIES_NAU),
      "EVL_target",
      c(
        "outside Natura 2000\ndesignated for Phengaris",
        "within Natura 2000\ndesignated for Phengaris"
      ),
      species_only(SPECIES_NAU),
      "Number of findings"
    ),
    "Natura 2000 designation, P. nausithous",
    "08_protection_evl_target_nausithous",
    size = "single"
  )

  report_figure(
    plot_counts_by_occupancy(
      data_evltar_sum %>% dplyr::filter(DRUH == SPECIES_TEL),
      "EVL_target",
      c(
        "outside Natura 2000\ndesignated for Phengaris",
        "within Natura 2000\ndesignated for Phengaris"
      ),
      species_only(SPECIES_TEL),
      "Number of findings"
    ),
    "Natura 2000 designation, P. teleius",
    "08_protection_evl_target_teleius",
    size = "single"
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
      species_only(SPECIES_NAU),
      "Number of findings"
    ),
    "Combined Natura 2000 status, P. nausithous",
    "08_protection_evl_combined_nausithous",
    height = 7
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
      species_only(SPECIES_TEL),
      "Number of sites"
    ),
    "Combined Natura 2000 status, P. teleius",
    "08_protection_evl_combined_teleius",
    height = 7
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
      species_only(SPECIES_NAU),
      "Number of findings"
    ),
    "Small-scale protected areas, P. nausithous",
    "08_protection_mzchu_nausithous",
    size = "single"
  )

  report_figure(
    plot_counts_by_occupancy(
      data_mzchu_sum %>% dplyr::filter(DRUH == SPECIES_TEL),
      "MZCHU",
      mzchu_labels,
      species_only(SPECIES_TEL),
      "Number of findings"
    ),
    "Small-scale protected areas, P. teleius",
    "08_protection_mzchu_teleius",
    size = "single"
  )
}

report_finish()

step_end("08")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

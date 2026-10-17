#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#              Step 19 - Threats and occupancy
#
#
#----------------------------------------------------------#
#
# P3.7: for each threat class and species, a binomial model of occupancy on
# whether the class was recorded at the visit, with the survey design of the
# revised models (grid square, year and observer random effects; field-form
# era and flight-season position), at record level. Replaces the ordination
# of four aggregated rows (REVIEW.md, item 12).
#
# The threat classes come from the harmonised vocabulary (config.yml
# `strukt_pozn: threat_items`), which maps the older 2019-2020 threat list to
# the classes of the current one. The two lists were filled in on different
# forms, so the era stays in every model. Records without a threat list are
# left out. p-values are Holm-adjusted within each species and sample.
#
# Reads:  Data/Processed/<spec>/data_analysis.csv   (step 06)
# Writes: Outputs/<spec>/Tables/19_*.csv, Figures/19_*.png
#         Outputs/<spec>/Reports/19_threat_models.md
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("19")

report_start(
  "19",
  "Threat models",
  paste(
    "Occupancy against each recorded threat class, one model per class and",
    "species, with the survey design of the revised models. Odds ratios above",
    "1 mean the species was recorded more often at visits where the threat was",
    "noted."
  )
)

report_warning(
  "The threats were recorded at the same visit as the butterflies, by the same",
  "surveyor, so these are associations, not effects: a surveyor may note some",
  "threats more readily at a site where the species is absent. The 2019-2020",
  "threat list is mapped to the classes of the current list by a proposed",
  "mapping that awaits checking (config.yml `strukt_pozn: threat_items`)."
)

data <- read_data_analysis()
species_names <- c(nau = "P. nausithous", tel = "P. teleius")
species_files <- c(nau = "nausithous", tel = "teleius")

#----------------------------------------------------------#
# Fit -----
#----------------------------------------------------------#

estimates <- list()
counts <- list()

for (species in c(SPECIES_NAU, SPECIES_TEL)) {
  code <- species_code(species)
  report_section(paste("Threats and occupancy of", species_names[[code]]))
  models <- list()
  for (sample in REVISED_SAMPLES) {
    threat_set <- revised_threat_specs(revised_model_data(data, species, sample), species, sample)
    counts[[paste(species, sample)]] <- dplyr::mutate(threat_set$counts, species = species, .before = 1)
    fitted <- fit_tmb_set(threat_set$specs)
    models <- c(models, fitted)

    coefs <- tmb_coef_table(fitted)
    if (nrow(coefs)) {
      estimates[[paste(species, sample)]] <- coefs %>%
        dplyr::filter(term %in% names(CONFIG$strukt_pozn$threat_items)) %>%
        dplyr::mutate(species = species, p_holm = stats::p.adjust(p_value, "holm"), .before = 1)
    }
  }
  report_tmb_set(models, paste0("19_", species_files[[code]]))
}

#----------------------------------------------------------#
# Results -----
#----------------------------------------------------------#

report_section("Summary")

counts <- dplyr::bind_rows(counts)
report_table(
  counts,
  sprintf(
    paste(
      "Records listing each threat class, by outcome; a class is modelled when",
      "both outcomes have at least %d records"
    ),
    CONFIG$revised_models$threats$min_records_per_outcome
  ),
  "19_threat_counts",
  max_rows = nrow(counts)
)

estimates <- dplyr::bind_rows(estimates)
if (nrow(estimates)) {
  summary_table <- estimates %>%
    dplyr::transmute(
      species, sample,
      threat = term,
      odds_ratio = sprintf("%.2f (%.2f-%.2f)", ratio, ratio_low, ratio_high),
      p_value, p_holm
    )
  report_table(
    summary_table,
    "Odds ratio (95% CI) of recording the species at visits with the threat, with Holm-adjusted p",
    "19_threat_estimates",
    max_rows = nrow(summary_table)
  )

  plot <- ggplot2::ggplot(
    estimates %>%
      dplyr::mutate(species = sub("Phengaris", "P.", species)),
    ggplot2::aes(x = ratio, y = term, colour = sample)
  ) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_pointrange(
      ggplot2::aes(xmin = ratio_low, xmax = ratio_high),
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::facet_wrap(~species) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_colour_manual(values = c("black", "grey40", "grey70"), name = NULL) +
    ggplot2::labs(x = "\nodds ratio (95% CI, log scale)", y = NULL) +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")
  report_figure(plot, "Odds ratios of occupancy by threat class", "19_threat_estimates", width = 10, height = 7)
}

report_finish()

step_end("19")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

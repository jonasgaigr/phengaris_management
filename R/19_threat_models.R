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
# the classes of the current one, and are labelled with their Article 17
# pressure codes (`strukt_pozn: threat_article17`, P4.11). The two lists were
# filled in on different forms, so the era stays in every model. Records
# without a threat list are left out. p-values are adjusted within each
# species and sample: Holm for the threats with an expected direction,
# Benjamini-Hochberg for the others (P4.11). A stacked model of all classes
# at once, with the threat as the response, completes them
# (R/functions/threats.R).
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
stacked <- list()

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
      coefs <- coefs %>%
        dplyr::filter(term %in% names(CONFIG$strukt_pozn$threat_items))
      adjusted <- adjust_threat_p(coefs$p_value, coefs$term)
      estimates[[paste(species, sample)]] <- coefs %>%
        dplyr::mutate(
          species = species, p_adjusted = adjusted$adjusted, adjustment = adjusted$method,
          .before = 1
        )
    }

    # All classes in one stacked model (P4.11): which threats are recorded
    # more or less often at the visits where the species was found.
    if (isTRUE(CONFIG$revised_models$threats$stacked)) {
      classes <- threat_set$counts$class[threat_set$counts$modelled]
      d <- revised_model_data(data, species, sample)
      stacked_model <- fit_stacked_threats(
        stack_threats(d, classes),
        id = paste0(code, "_threats_stacked", sample_suffix(sample)), sample = sample
      )
      models[[stacked_model$id]] <- stacked_model
      stacked_coefs <- tmb_coef_table(list(stacked_model))
      if (nrow(stacked_coefs)) {
        stacked_coefs <- stacked_coefs %>%
          dplyr::filter(grepl(":POSITIVE$", term)) %>%
          dplyr::mutate(class = sub("^CLASS(.*):POSITIVE$", "\\1", term))
        adjusted <- adjust_threat_p(stacked_coefs$p_value, stacked_coefs$class)
        stacked[[paste(species, sample)]] <- stacked_coefs %>%
          dplyr::mutate(species = species, p_adjusted = adjusted$adjusted, adjustment = adjusted$method)
      }
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
      article17 = article17_code(term),
      odds_ratio = sprintf("%.2f (%.2f-%.2f)", ratio, ratio_low, ratio_high),
      p_value, p_adjusted, adjustment
    )
  report_table(
    summary_table,
    paste(
      "Odds ratio (95% CI) of recording the species at visits with the threat,",
      "with the Article 17 pressure code of the class and p adjusted within",
      "species and sample: Holm for the threats with an expected direction",
      "(config.yml `revised_models: threats: confirmatory`), Benjamini-Hochberg",
      "for the others"
    ),
    "19_threat_estimates",
    max_rows = nrow(summary_table)
  )

  stacked <- dplyr::bind_rows(stacked)
  if (nrow(stacked)) {
    stacked_table <- stacked %>%
      dplyr::transmute(
        species, sample,
        threat = class, article17 = article17_code(class),
        odds_ratio = sprintf("%.2f (%.2f-%.2f)", ratio, ratio_low, ratio_high),
        p_value, p_adjusted, adjustment
      )
    report_table(
      stacked_table,
      paste(
        "Stacked model of all threat classes (P4.11): the odds ratio (95% CI) of",
        "the threat being recorded at a visit where the species was found against",
        "one where it was not, each class with its own intercept and era effect,",
        "random intercepts for the visit, the grid square and the observer;",
        "p adjusted as above. The direction is the reverse of the per-threat",
        "models: here the threat is the response"
      ),
      "19_threat_stacked",
      max_rows = nrow(stacked_table)
    )
  }

  report_figure(
    plot_threat_estimates(estimates),
    sprintf(
      paste(
        "Odds ratios (95%% CI) of recording the species at a visit where each",
        "threat class (with its Article 17 code) is noted, in the three samples;",
        "filled marks have an adjusted p below 0.05 (Holm for the confirmatory",
        "threats, Benjamini-Hochberg for the others); fits without a finite",
        "interval, or whose upper limit is %g or more times the lower one, are",
        "left out"
      ),
      CONFIG$synthesis$max_interval_ratio
    ),
    "19_threat_estimates",
    height = 12
  )
}

report_finish()

step_end("19")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

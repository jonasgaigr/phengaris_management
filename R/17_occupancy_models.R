#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#            Step 17 - Revised occupancy models
#
#
#----------------------------------------------------------#
#
# The revised occupancy models of the Phase 3 review (REVIEW.md):
#
#   P3.1  one record per species x site x visit; random intercepts for the
#         mapping grid square, the year and the observer, and a persistent
#         site as a sensitivity check at three matching distances;
#   P3.2  every model fitted with the imputed absences, with only the
#         plausible ones (P4.3) and on the observed records only;
#   P3.3  the position of the visit in the flight season (a quadratic in the
#         deviation from the year's median positive record) and the observer;
#   P4.1  the flight season cut by the flight curve of step 16; the
#         quadratic compared with the log relative activity of the curve as
#         the season term, and the management model refitted on fixed
#         windows of 28 and 45 days around the median;
#   P3.4  one multivariable management model per species: mowing timing x
#         method, host-plant abundance and protection designated for the
#         species, with the field-form era; for 2021-2024 polygon sites also
#         grazing and site area; diagnostics, GVIF, separation checks with a
#         bias-reduced fallback, and marginal predicted probabilities;
#   P3.5  connectivity, with alpha and b chosen by profile likelihood over a
#         grid (P4.2); the choice is written for the later steps;
#   item 26  co-occurrence as one species' occupancy against the other's
#         recorded outcome at the same visit, observed records only;
#   item 22  occupancy compared between the species.
#
# The models are naive occupancy models (item 11). The model set is in
# R/functions/model_specs_revised.R, the helpers in R/functions/model_revised.R.
#
# Reads:  Data/Processed/<spec>/data_analysis.csv   (step 06)
#         Data/Processed/common/flight_curve.csv   (step 16)
# Writes: Data/Processed/<spec>/connectivity_selected.csv
#         Outputs/<spec>/Tables/17_*.csv, Figures/17_*.png
#         Outputs/<spec>/Reports/17_occupancy_models.md
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("17")

report_start(
  "17",
  "Occupancy models",
  paste(
    "Multivariable occupancy models of the two species, with the survey design",
    "in every model: random intercepts for the mapping grid square, the year and",
    "the observer, and the field-form era and the position of the visit in the",
    "flight season as covariates. Each model is fitted with the imputed",
    "absences, with only the plausible ones (P4.3) and on the observed records only."
  )
)

report_warning(
  "These are naive occupancy models. The monitoring is one visit per site and",
  "year, so a negative record mixes absence with non-detection; the observer",
  "and the timing of the visit within the flight season are in every model, but",
  "detection is not estimated separately. Odds ratios describe the probability",
  "of recording the species at a visit."
)

data <- read_data_analysis()
flight_curve <- readr::read_csv(processed_path("flight_curve.csv", "common"), show_col_types = FALSE)

#----------------------------------------------------------#
# Records -----
#----------------------------------------------------------#

species_names <- c(nau = "P. nausithous", tel = "P. teleius")
species_files <- c(nau = "nausithous", tel = "teleius")

model_data <- list()
for (species in c(SPECIES_NAU, SPECIES_TEL)) {
  for (sample in REVISED_SAMPLES) {
    model_data[[paste(species, sample)]] <- revised_model_data(data, species, sample)
  }
}

report_table(
  dplyr::bind_rows(lapply(model_data, function(d) {
    complete <- stats::complete.cases(d[, c("TIMING", "METHOD", "PLANT", "EVL_species")])
    tibble::tibble(
      species = d$DRUH[1],
      records = nrow(d),
      positive = sum(d$POSITIVE == 1),
      imputed = sum(d$IMPUTED),
      management_recorded = sum(complete),
      management_recorded_positive = sum(complete & d$POSITIVE == 1),
      polygon_2021_2024_with_grazing = sum(
        complete & d$YEAR >= CONFIG$design$protocol_change_year & !is.na(d$GRAZE) & !is.na(d$LOG_AREA)
      )
    )
  }), .id = "sample") %>%
    dplyr::mutate(sample = sub(paste0("^(", SPECIES_NAU, "|", SPECIES_TEL, ") "), "", sample)),
  paste(
    "In-season records by species and sample, with the records complete on the",
    "management variables (mowing timing and method, host plant, protection)",
    "and those of the 2021-2024 polygon model"
  ),
  "17_records"
)

by_year <- data %>%
  dplyr::filter(IN_FLIGHT_WINDOW) %>%
  dplyr::mutate(record = dplyr::case_when(
    POSITIVE == 1 ~ "positive",
    IMPUTED ~ "imputed negative",
    TRUE ~ "observed negative"
  )) %>%
  dplyr::count(DRUH, YEAR, record) %>%
  tidyr::pivot_wider(names_from = record, values_from = n, values_fill = 0)
report_table(
  by_year,
  paste(
    "In-season records by species, year and outcome. A year with few observed",
    "negatives cannot be modelled on the observed records alone: the year",
    "random effect then separates the outcome."
  ),
  "17_records_by_year"
)

#----------------------------------------------------------#
# Models of each species -----
#----------------------------------------------------------#

#' Point-range plot of the marginal predictions of mowing timing x method.
plot_management_predictions <- function(pred, species_label) {
  pred <- pred %>%
    dplyr::mutate(
      timing = factor(TIMING, levels = c(0, 1), labels = c("harmful timing", "appropriate timing")),
      method = factor(METHOD, levels = c(0, 1), labels = c("whole area mown", "patches left uncut"))
    )
  ggplot2::ggplot(pred, ggplot2::aes(x = timing, y = estimate, colour = method)) +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = conf_low, ymax = conf_high),
      position = ggplot2::position_dodge(width = 0.4)
    ) +
    ggplot2::facet_wrap(~sample) +
    ggplot2::scale_colour_manual(values = c("grey55", "black"), name = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      x = NULL,
      y = paste0("predicted probability of recording ", species_label, "\n")
    ) +
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(legend.position = "bottom")
}

#' Predicted occupancy across the flight season.
plot_season <- function(curve, species_label) {
  ggplot2::ggplot(curve, ggplot2::aes(x = DOY_DEV, y = estimate, linetype = sample)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = conf_low, ymax = conf_high, group = sample),
      fill = "grey85", colour = NA
    ) +
    ggplot2::geom_line() +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      x = "\ndays from the year's median positive record",
      y = paste0("predicted probability of recording ", species_label, "\n"),
      linetype = NULL
    ) +
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(legend.position = "bottom")
}

#' Profile of the connectivity: delta AIC against alpha, one line per b.
plot_connectivity_profile <- function(profile, species_label) {
  ggplot2::ggplot(
    profile,
    ggplot2::aes(x = alpha_per_km, y = delta_AIC, linetype = factor(area_exponent), shape = factor(area_exponent))
  ) +
    ggplot2::geom_hline(yintercept = 2, colour = "grey60", linetype = "dashed") +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~sample, scales = "free_y") +
    ggplot2::scale_x_log10(breaks = unlist(CONFIG$connectivity$alpha_grid_per_km)) +
    ggplot2::labs(
      x = "\nalpha (per km; mean dispersal distance 1 / alpha)",
      y = paste0("delta AIC, ", species_label, "\n"),
      linetype = "b", shape = "b"
    ) +
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(legend.position = "bottom")
}

all_models <- list()
connectivity_selected <- list()

for (species in c(SPECIES_NAU, SPECIES_TEL)) {
  code <- species_code(species)
  prefix <- paste0("17_", species_files[[code]])
  report_section(paste("Occupancy of", species_names[[code]]))

  #--------------------------------------------------#
  ## Connectivity: alpha and b by profile likelihood -----
  #--------------------------------------------------#

  # The management model with the connectivity at every alpha x b pair
  # (P4.2). The pair with the lowest AIC on the sample with the imputed
  # absences becomes LOG_CONN in every sample, unless config.yml fixes it.
  profile_specs <- unlist(lapply(REVISED_SAMPLES, function(s) {
    connectivity_profile_specs(model_data[[paste(species, s)]], species, s)
  }), recursive = FALSE)
  profile_models <- fit_tmb_set(profile_specs)
  profile <- dplyr::bind_rows(lapply(REVISED_SAMPLES, function(s) {
    connectivity_profile(profile_models, Filter(function(p) p$sample == s, profile_specs))
  }))
  conn_settings <- CONFIG$connectivity
  main <- profile[profile$sample == REVISED_SAMPLES[["with_imputed"]], ]
  chosen <- if (conn_settings$choice == "profile") {
    main[which.min(main$AIC), ]
  } else {
    main[main$alpha_per_km == conn_settings$alpha_per_km & main$area_exponent == conn_settings$area_exponent, ]
  }
  aic_of <- function(a, b) main$AIC[main$alpha_per_km == a & main$area_exponent == b]
  connectivity_selected[[code]] <- data.frame(
    DRUH = species, choice = conn_settings$choice,
    alpha_per_km = chosen$alpha_per_km, area_exponent = chosen$area_exponent,
    mean_dispersal_m = 1000 / chosen$alpha_per_km,
    column = connectivity_column(chosen$alpha_per_km, chosen$area_exponent),
    delta_AIC_of_fixed_pair = aic_of(conn_settings$alpha_per_km, conn_settings$area_exponent) - chosen$AIC,
    delta_AIC_of_phase3_pair = if (length(aic_of(1, 0.5))) aic_of(1, 0.5) - chosen$AIC else NA_real_,
    pairs_within_2_AIC = paste(
      sprintf("a%g/b%g", main$alpha_per_km[main$delta_AIC <= 2], main$area_exponent[main$delta_AIC <= 2]),
      collapse = ", "
    ),
    stringsAsFactors = FALSE
  )
  for (s in REVISED_SAMPLES) {
    d <- model_data[[paste(species, s)]]
    d$LOG_CONN <- d[[paste0("LOG_", connectivity_selected[[code]]$column)]]
    model_data[[paste(species, s)]] <- d
  }

  report_note(sprintf(
    paste(
      "Connectivity (P4.2): the management model was refitted with the",
      "connectivity at each of %d pairs of alpha (%s per km) and b (%s). On the",
      "sample with the imputed absences the %s pair is alpha = %g per km (mean",
      "dispersal distance %g m) and b = %g; pairs within 2 AIC: %s. The",
      "connectivity models below use it."
    ),
    nrow(main), paste(unlist(conn_settings$alpha_grid_per_km), collapse = ", "),
    paste(unlist(conn_settings$area_exponent_grid), collapse = ", "),
    if (conn_settings$choice == "profile") "best" else "fixed",
    chosen$alpha_per_km, round(1000 / chosen$alpha_per_km), chosen$area_exponent,
    connectivity_selected[[code]]$pairs_within_2_AIC
  ))
  report_table(
    profile %>%
      dplyr::select(
        sample, alpha_per_km, area_exponent, mean_dispersal_m, n_obs, logLik, AIC, delta_AIC,
        conn_odds_ratio, conn_or_low, conn_or_high, converged
      ) %>%
      dplyr::arrange(sample, delta_AIC),
    paste(
      "Profile likelihood of the connectivity: the management model with the",
      "connectivity at each alpha and b, by sample (all on the same records",
      "within a sample); conn_odds_ratio per tenfold connectivity"
    ),
    paste0(prefix, "_connectivity_profile"),
    max_rows = 25
  )
  report_figure(
    plot_connectivity_profile(profile, species_names[[code]]),
    paste("Profile of the connectivity parameters (dashed: 2 AIC above the best),", species_names[[code]]),
    paste0(prefix, "_connectivity_profile"),
    width = 10
  )

  specs <- unlist(lapply(REVISED_SAMPLES, function(s) {
    revised_occupancy_specs(model_data[[paste(species, s)]], species, s)
  }), recursive = FALSE)
  models <- fit_tmb_set(specs)
  all_models <- c(all_models, models)

  #--------------------------------------------------#
  ## Key estimates -----
  #--------------------------------------------------#

  coefs <- tmb_coef_table(models)
  key_models <- paste0(code, "_", c("management", "management_conn", "management_2021"))
  key_models <- c(key_models, paste0(key_models, "_pls"), paste0(key_models, "_obs"))
  key <- key_estimates(coefs %>% dplyr::filter(model %in% key_models))
  report_table(
    key,
    paste(
      "Odds ratios (95% CI) of the management model, the same with connectivity,",
      "and the 2021-2024 model with grazing and site area; with the imputed",
      "absences, the plausible ones only and the observed records only. TIMING 1 = mowing timed",
      "appropriately, METHOD 1 = patches left uncut, PLANT against single",
      "plants, EVL_species = inside an EVL designated for the species, LOG_CONN",
      "per tenfold connectivity, LOG_AREA per tenfold site area"
    ),
    paste0(prefix, "_key_estimates"),
    max_rows = nrow(key)
  )

  #--------------------------------------------------#
  ## Imputation sensitivity -----
  #--------------------------------------------------#

  sensitivity <- key %>%
    dplyr::filter(model %in% paste0(code, c("_management", "_management_pls", "_management_obs"))) %>%
    dplyr::select(term, sample, ratio_ci) %>%
    tidyr::pivot_wider(names_from = sample, values_from = ratio_ci)
  report_table(
    sensitivity,
    "The management model with all, with the plausible and without the imputed absences (odds ratio, 95% CI)",
    paste0(prefix, "_imputation_sensitivity")
  )

  #--------------------------------------------------#
  ## Flight-season window sensitivity -----
  #--------------------------------------------------#

  # The management model refitted on the visits within fixed windows around
  # the year's median instead of the flight season of the curve (P4.1).
  window_days <- unlist(CONFIG$design$flight_window$sensitivity_days)
  window_models <- fit_tmb_set(unlist(lapply(window_days, function(days) {
    lapply(REVISED_SAMPLES, function(sample) {
      d <- revised_model_data(data, species, sample, window = days)
      d <- d[stats::complete.cases(d[, c("TIMING", "METHOD", "PLANT", "EVL_species")]), , drop = FALSE]
      list(
        id = paste0(code, "_management_w", days, sample_suffix(sample)),
        label = sprintf("Management model, visits within %d days of the median", days),
        group = "Window", formula = revised_formula(terms = MANAGEMENT_TERMS), data = d, sample = sample
      )
    })
  }), recursive = FALSE))
  window_key <- dplyr::bind_rows(
    key %>%
      dplyr::filter(model %in% paste0(code, c("_management", "_management_pls", "_management_obs"))) %>%
      dplyr::mutate(window = "flight season (curve)"),
    key_estimates(tmb_coef_table(window_models)) %>%
      dplyr::mutate(window = paste(sub("^.*_w([0-9]+).*$", "\\1", model), "days"))
  ) %>%
    dplyr::left_join(
      tmb_fit_table(c(models, window_models))[, c("model", "n_obs")],
      by = "model"
    )
  window_key <- dplyr::bind_rows(
    dplyr::select(window_key, sample, window, term, ratio_ci),
    window_key %>%
      dplyr::distinct(sample, window, n_obs) %>%
      dplyr::transmute(sample, window, term = "records", ratio_ci = as.character(n_obs))
  ) %>%
    tidyr::pivot_wider(names_from = window, values_from = ratio_ci)
  report_table(
    window_key,
    paste(
      "The management model on the flight season of the curve and on fixed",
      "windows around the year's median (odds ratio, 95% CI; and the records",
      "each is fitted on)"
    ),
    paste0(prefix, "_window_sensitivity"),
    max_rows = nrow(window_key)
  )
  report_tmb_set(window_models, paste0(prefix, "_window"))

  #--------------------------------------------------#
  ## Model comparisons -----
  #--------------------------------------------------#

  for (sample in REVISED_SAMPLES) {
    for (cmp in revised_occupancy_comparisons(species, sample)) {
      tab <- aic_table_tmb(models, cmp$ids)
      report_table(
        tab,
        sprintf("%s, %s (all on the same %d records)", cmp$caption, sample, if (nrow(tab)) tab$n_obs[1] else 0L),
        paste0(prefix, "_aic_", cmp$stem, sample_suffix(sample))
      )
    }
  }

  #--------------------------------------------------#
  ## Marginal predictions -----
  #--------------------------------------------------#

  management <- models[paste0(code, c("_management", "_management_pls", "_management_obs"))]
  pred_mgmt <- dplyr::bind_rows(lapply(management, marginal_predictions, by = c("TIMING", "METHOD")))
  report_table(
    pred_mgmt,
    paste(
      "Marginal predicted probability of recording the species by mowing timing",
      "and method (random effects at zero, averaged over the records; 95% CI)"
    ),
    paste0(prefix, "_predictions_management")
  )
  if (nrow(pred_mgmt)) {
    report_figure(
      plot_management_predictions(pred_mgmt, species_names[[code]]),
      paste("Predicted occupancy by mowing timing and method,", species_names[[code]]),
      paste0(prefix, "_predictions_management"),
      width = 11
    )
  }

  pred_other <- dplyr::bind_rows(unname(c(
    lapply(management, marginal_predictions, by = "PLANT"),
    lapply(management, marginal_predictions, by = "EVL_species")
  )))
  report_table(
    pred_other,
    "Marginal predicted probability by host-plant abundance and by protection designated for the species",
    paste0(prefix, "_predictions_plant_protection")
  )

  curve <- dplyr::bind_rows(lapply(
    models[paste0(code, c("_baseline", "_baseline_pls", "_baseline_obs"))], season_curve,
    curve = flight_curve
  ))
  if (nrow(curve)) {
    report_figure(
      plot_season(curve, species_names[[code]]),
      paste("Predicted occupancy across the flight season (baseline model),", species_names[[code]]),
      paste0(prefix, "_season")
    )
  }

  #--------------------------------------------------#
  ## Full tables and diagnostics -----
  #--------------------------------------------------#

  report_section(paste("All models of", species_names[[code]]), level = 3)
  report_tmb_set(models, prefix)
}

#----------------------------------------------------------#
# Co-occurrence -----
#----------------------------------------------------------#

report_section("Co-occurrence of the two species")

report_note(paste(
  "Does finding one species at a visit go with finding the other? Each",
  "species' occupancy is modelled against the other species' recorded outcome",
  "at the same visit, on the visits where both species have an observed record.",
  "Imputed absences are left out on both sides: an absence is imputed exactly",
  "when the other species was recorded and this one was not, so including them",
  "would build the answer into the data (REVIEW.md, item 26)."
))

observed_both <- revised_model_data(data, NULL, REVISED_SAMPLES[["observed"]])
cooccurrence_specs <- lapply(c(SPECIES_NAU, SPECIES_TEL), function(species) {
  code <- species_code(species)
  list(
    id = paste0(code, "_cooccurrence"),
    label = paste(species_names[[code]], "against the other species at the same visit"),
    group = "Co-occurrence",
    formula = revised_formula(terms = "OTHER_POSITIVE"),
    data = cooccurrence_data(observed_both, species),
    sample = REVISED_SAMPLES[["observed"]]
  )
})
cooccurrence_models <- fit_tmb_set(cooccurrence_specs)

report_table(
  dplyr::bind_rows(lapply(cooccurrence_models, function(m) {
    tibble::as_tibble(as.data.frame(table(
      this_species = m$data$POSITIVE, other_species = m$data$OTHER_POSITIVE
    ))) %>%
      dplyr::mutate(model = m$id, .before = 1)
  })),
  "Visits with observed records of both species, by the outcome of each",
  "17_cooccurrence_counts"
)
report_table(
  key_estimates(tmb_coef_table(cooccurrence_models)),
  "Odds ratio of recording the species when the other species was recorded at the same visit (95% CI)",
  "17_cooccurrence_estimates"
)
report_tmb_set(cooccurrence_models, "17_cooccurrence")

#----------------------------------------------------------#
# Occupancy of the two species compared -----
#----------------------------------------------------------#

report_section("Occupancy of the two species compared")

report_note(paste(
  "The share of positive records of each species, compared with a test of two",
  "proportions, and with a model of both species with the species as a fixed",
  "effect and the survey design as in the models above. The original",
  "Wilcoxon test on the 0/1 outcome is not a comparison of occupancy (REVIEW.md,",
  "item 22)."
))

proportions <- dplyr::bind_rows(lapply(REVISED_SAMPLES, function(sample) {
  d <- revised_model_data(data, NULL, sample)
  positive <- tapply(d$POSITIVE == 1, d$DRUH, sum)
  total <- tapply(d$POSITIVE, d$DRUH, length)
  test <- stats::prop.test(positive, total)
  tibble::tibble(
    sample = sample,
    nausithous_positive = positive[[SPECIES_NAU]], nausithous_records = total[[SPECIES_NAU]],
    teleius_positive = positive[[SPECIES_TEL]], teleius_records = total[[SPECIES_TEL]],
    nausithous_share = positive[[SPECIES_NAU]] / total[[SPECIES_NAU]],
    teleius_share = positive[[SPECIES_TEL]] / total[[SPECIES_TEL]],
    chi_squared = unname(test$statistic), p_value = test$p.value
  )
}))
report_table(proportions, "Share of positive records by species, test of two proportions", "17_species_proportions")

species_models <- fit_tmb_set(lapply(REVISED_SAMPLES, function(sample) {
  list(
    id = paste0("both_species", sample_suffix(sample)),
    label = "Species as a fixed effect", group = "Species",
    formula = revised_formula(terms = "DRUH"),
    data = revised_model_data(data, NULL, sample),
    sample = sample
  )
}))
report_table(
  key_estimates(tmb_coef_table(species_models)),
  "Odds ratio of recording P. teleius against P. nausithous (95% CI)",
  "17_species_estimates"
)
report_tmb_set(species_models, "17_both_species")

#----------------------------------------------------------#
# Export the chosen connectivity -----
#----------------------------------------------------------#

connectivity_selected <- dplyr::bind_rows(connectivity_selected)
report_table(
  connectivity_selected,
  paste(
    "The connectivity the revised models use, by species: alpha and b, and how",
    "much worse the fixed pair of config.yml and the Phase 3 pair (alpha = 1,",
    "b = 0.5) fit (delta AIC)"
  ),
  "17_connectivity_selected"
)
validate_table(
  connectivity_selected, "connectivity_selected",
  key = "DRUH", not_na = c("alpha_per_km", "area_exponent", "column"), n_rows = 2
)
readr::write_csv(connectivity_selected, processed_path("connectivity_selected.csv"))

report_finish()

message("  ", length(all_models) + length(cooccurrence_models) + length(species_models), " models fitted")
step_end("17")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

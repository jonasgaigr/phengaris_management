#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#           Step 23 - Detection sensitivity of the occupancy models
#
#
#----------------------------------------------------------#
#
# P4.4: how much could the management estimates of the naive occupancy
# models (step 17) owe to imperfect detection? Two sensitivity analyses, as
# the literature review proposed (R/functions/detection.R):
#
#   1. Known detection. The management model refitted with the detection
#      fixed at each p of config.yml `detection: fixed_p`, so that
#      P(record) = p * occupancy. A GLM with the year as a fixed factor,
#      compared with the same GLM at p = 1 (and with the GLMM of step 17).
#   2. Mixed design. A Bayesian occupancy model of the persistent site x
#      year, with every visit to it as a replicate (spOccupancy::PGOcc):
#      management in the occupancy part with random intercepts for the grid
#      square and the year; the field-form era and the season in the
#      detection part with a random intercept for the observer. The
#      site-years visited more than once identify the detection, with a
#      weakly informative prior centred on p = 0.7; refitted with a vague
#      prior, and on the observed records only.
#
# Neither replaces the models of step 17. With essentially one visit per
# site and year, a single-visit occupancy model identifies detection only
# through its parametric form (Knape & Korner-Nievergelt 2015), so the
# headline stays the probability of recording the species.
#
# Reads:  Data/Processed/<spec>/data_analysis.csv   (step 06)
# Writes: Outputs/<spec>/Tables/23_*.csv, Figures/23_*.png
#         Outputs/<spec>/Reports/23_detection_models.md
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("23")

settings <- CONFIG$detection

report_start(
  "23",
  "Detection sensitivity",
  paste(
    "How far the management estimates of the naive occupancy models could owe",
    "to imperfect detection: the management model refitted with a known",
    "detection probability, and a Bayesian occupancy model in which the",
    "site-years visited more than once identify the detection."
  )
)

report_warning(
  "Both analyses rest on assumptions the data can barely check. Few",
  "site-years were visited more than once, so the detection estimate of the",
  "occupancy model leans on its prior and on the parametric form of the",
  "model; no per-visit detection probability of adult Phengaris is published",
  "(the closest are daily mark-recapture capture probabilities of 0.4-0.5,",
  "Nowicki & Vrabec 2011). Read these as bounds on how the estimates could",
  "move, not as corrected estimates."
)

data <- read_data_analysis()
species_names <- c(nau = "P. nausithous", tel = "P. teleius")
species_files <- c(nau = "nausithous", tel = "teleius")
mgmt_vars <- c("TIMING", "METHOD", "PLANT", "EVL_species")

#' The odds ratio and its interval as one string.
format_ratio <- function(ratio, low, high) sprintf("%.2f (%.2f-%.2f)", ratio, low, high)

for (species in c(SPECIES_NAU, SPECIES_TEL)) {
  code <- species_code(species)
  prefix <- paste0("23_", species_files[[code]])
  report_section(paste("Detection sensitivity,", species_names[[code]]))

  model_data <- lapply(REVISED_SAMPLES, function(sample) {
    d <- revised_model_data(data, species, sample)
    d[stats::complete.cases(d[, c(mgmt_vars, "OBSERVER")]), , drop = FALSE]
  })
  naive <- fit_tmb_set(lapply(names(REVISED_SAMPLES), function(s) {
    list(
      id = paste0(code, "_management", sample_suffix(REVISED_SAMPLES[[s]])),
      label = "Management model (step 17)", group = "Naive",
      formula = revised_formula(terms = MANAGEMENT_TERMS), data = model_data[[s]],
      sample = REVISED_SAMPLES[[s]]
    )
  }))

  #--------------------------------------------------#
  ## Known detection -----
  #--------------------------------------------------#

  known <- dplyr::bind_rows(lapply(naive, function(m) {
    dplyr::bind_rows(lapply(c(1, unlist(settings$fixed_p)), function(p) fit_known_detection(m, p)))
  }))
  naive_coefs <- tmb_coef_table(naive)
  terms_shown <- unique(naive_coefs$term[!grepl("^\\(Intercept\\)$|^ERA|DOY_STD|LOG_ACTIVITY", naive_coefs$term)])

  known_table <- dplyr::bind_rows(
    naive_coefs %>%
      dplyr::filter(term %in% terms_shown) %>%
      dplyr::transmute(sample, term, fit = "GLMM (step 17)", value = format_ratio(ratio, ratio_low, ratio_high)),
    known %>%
      dplyr::filter(term %in% terms_shown) %>%
      dplyr::transmute(
        sample, term,
        fit = sprintf("GLM, p = %g", p),
        value = dplyr::if_else(status == "ok", format_ratio(ratio, ratio_low, ratio_high), paste0("(", status, ")"))
      )
  ) %>%
    tidyr::pivot_wider(names_from = fit, values_from = value)
  report_table(
    known_table,
    paste(
      "Odds ratios of occupancy (95% CI) of the management model: the GLMM of",
      "step 17, and a GLM with the year as a fixed factor with the detection",
      "probability fixed at p (p = 1 is the naive GLM). A p for which the",
      "occupancy of some records would have to exceed 1 cannot be fitted"
    ),
    paste0(prefix, "_known_detection"),
    max_rows = nrow(known_table)
  )
  report_csv(known, "Known-detection refits: every coefficient", paste0(prefix, "_known_detection_coefficients"))

  #--------------------------------------------------#
  ## Mixed-design occupancy model -----
  #--------------------------------------------------#

  fits <- list(
    informative = list(sample = "with_imputed", prior_var = settings$prior_var, label = "with imputed, prior on p"),
    vague = list(sample = "with_imputed", prior_var = settings$prior_var_vague, label = "with imputed, vague prior"),
    plausible = list(
      sample = "plausible", prior_var = settings$prior_var, label = "with plausible imputed, prior on p"
    ),
    observed = list(sample = "observed", prior_var = settings$prior_var, label = "observed only, prior on p")
  )
  design <- list()
  posterior <- list()
  detection <- list()
  runs <- list()
  for (name in names(fits)) {
    f <- fits[[name]]
    arrays <- occupancy_arrays(
      model_data[[f$sample]], MANAGEMENT_TERMS, c("ERA", season_terms()),
      unit = unlist(settings$unit)
    )
    design[[name]] <- data.frame(fit = f$label, arrays$design, stringsAsFactors = FALSE)
    message("  ", species_names[[code]], ": occupancy model, ", f$label)
    run <- fit_mixed_occupancy_converged(arrays, settings$prior_p, f$prior_var, settings$mcmc)
    fit <- run$fit
    runs[[name]] <- data.frame(
      model = f$label, extended = run$extended, samples = run$mcmc$samples, burn = run$mcmc$burn,
      thin = run$mcmc$thin, stringsAsFactors = FALSE
    )
    posterior[[name]] <- mixed_occupancy_table(fit, f$label)
    era <- grep("^ERA", arrays$det_columns, value = TRUE)
    detection[[name]] <- detection_probability(fit, f$label, if (length(era)) era[1] else NULL)
  }
  design <- dplyr::bind_rows(design)
  posterior <- dplyr::bind_rows(posterior)
  detection <- dplyr::bind_rows(detection)

  report_table(
    design,
    sprintf(
      paste(
        "The occupancy units (%s) and their visits: the units visited more than",
        "once, and among them those where the species was found at some visits",
        "and not at others, are what identifies the detection"
      ),
      paste(unlist(settings$unit), collapse = " x ")
    ),
    paste0(prefix, "_occupancy_design")
  )

  if (nrow(posterior)) {
    naive_main <- naive_coefs %>%
      dplyr::filter(sample %in% REVISED_SAMPLES[c("with_imputed", "observed")], term %in% terms_shown) %>%
      dplyr::transmute(sample, key = .matrix_names(term), term, naive = format_ratio(ratio, ratio_low, ratio_high))
    occupancy <- posterior %>%
      dplyr::filter(part == "occupancy", term != "(Intercept)") %>%
      dplyr::transmute(fit = model, key = term, value = format_ratio(odds_ratio, or_low, or_high)) %>%
      tidyr::pivot_wider(names_from = fit, values_from = value)
    comparison <- naive_main %>%
      dplyr::filter(sample == REVISED_SAMPLES[["with_imputed"]]) %>%
      dplyr::select(key, term, `naive GLMM, with imputed` = naive) %>%
      dplyr::left_join(
        naive_main %>%
          dplyr::filter(sample == REVISED_SAMPLES[["observed"]]) %>%
          dplyr::select(key, `naive GLMM, observed only` = naive),
        by = "key"
      ) %>%
      dplyr::left_join(occupancy, by = "key") %>%
      dplyr::select(-key)
    report_table(
      comparison,
      paste(
        "Odds ratios of occupancy: the naive GLMM (95% CI) against the occupancy",
        "part of the mixed-design model (posterior mean, 95% credible interval)"
      ),
      paste0(prefix, "_occupancy_estimates"),
      max_rows = nrow(comparison)
    )

    sensitivity <- dplyr::bind_rows(
      naive_coefs %>%
        dplyr::transmute(
          fit = "GLMM (step 17)", sample, term, odds_ratio = ratio, or_low = ratio_low, or_high = ratio_high
        ),
      known %>%
        dplyr::filter(status == "ok") %>%
        dplyr::transmute(
          fit = sprintf("GLM, detection fixed at p = %g", p), sample, term,
          odds_ratio = ratio, or_low = ratio_low, or_high = ratio_high
        ),
      posterior %>%
        dplyr::filter(part == "occupancy") %>%
        dplyr::transmute(
          fit = paste0("occupancy model, ", sub("^[^,]*, ", "", model)),
          sample = sub(",.*$", "", model), term = model_term(term), odds_ratio, or_low, or_high
        )
    )
    report_figure(
      plot_detection_sensitivity(sensitivity, unique(sensitivity$fit), species_names[[code]]),
      sprintf(
        paste(
          "Odds ratios (95%% interval) of the key terms as detection is allowed",
          "for: the GLMM of step 17, GLMs with the detection probability fixed at",
          "p, and the occupancy part of the mixed-design model (posterior mean",
          "and credible interval), in the three samples; fits without a finite",
          "interval, or whose upper limit is %g or more times the lower one, are",
          "left out, %s"
        ),
        CONFIG$synthesis$max_interval_ratio, species_names[[code]]
      ),
      paste0(prefix, "_known_detection"),
      height = 10
    )

    report_table(
      detection %>%
        dplyr::mutate(
          prior_mean = settings$prior_p,
          .after = quantity
        ),
      sprintf(
        paste(
          "Detection probability at the year's median day (posterior mean, 95%%",
          "credible interval). The prior on the detection intercept is normal on",
          "the logit scale with mean logit(%g) and variance %g (vague: %g); a",
          "posterior close to its prior means the repeated visits say little"
        ),
        settings$prior_p, settings$prior_var, settings$prior_var_vague
      ),
      paste0(prefix, "_detection_probability")
    )
    report_figure(
      plot_detection_probability(detection, settings$prior_p, species_names[[code]]),
      paste(
        "Detection probability at the year's median day by field-form era",
        "(posterior mean, 95% credible interval); the vertical line is the mean",
        "of the prior,", species_names[[code]]
      ),
      paste0(prefix, "_detection_probability"),
      height = 6
    )

    report_table(
      posterior %>%
        dplyr::group_by(model) %>%
        dplyr::summarise(max_rhat = max(rhat, na.rm = TRUE), min_ess = min(ess, na.rm = TRUE), .groups = "drop") %>%
        dplyr::left_join(dplyr::bind_rows(runs), by = "model") %>%
        dplyr::mutate(converged = max_rhat <= settings$max_rhat),
      sprintf(
        paste(
          "Convergence of the MCMC: the largest Rhat (at most %g counts as",
          "converged) and the smallest effective sample size of the coefficients;",
          "%d chains of %d samples, %d burn-in, thinned by %d, and %d times longer",
          "where the first run had not converged (extended)"
        ),
        settings$max_rhat, settings$mcmc$chains, settings$mcmc$samples, settings$mcmc$burn,
        settings$mcmc$thin, settings$extend_factor
      ),
      paste0(prefix, "_mcmc_diagnostics")
    )
    report_csv(
      posterior, "Mixed-design occupancy models: posterior of every coefficient",
      paste0(prefix, "_occupancy_posterior")
    )
  }
}

report_finish()

step_end("23")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

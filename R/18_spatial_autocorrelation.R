#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#         Step 18 - Spatial autocorrelation of the residuals
#
#
#----------------------------------------------------------#
#
# P3.6: is there spatial autocorrelation left in the residuals of the revised
# occupancy models, once the mapping grid square is a random effect?
#
#   1. The baseline and management models of step 17 are refitted.
#   2. DHARMa's simulated residuals are aggregated per persistent site, and
#      Moran's I is tested on the sites' coordinates.
#   3. Where it is significant (config.yml `revised_models: autocorrelation`),
#      the model is refitted with a smooth of the coordinates in place of the
#      grid random effect (mgcv), and the two are compared: the residual
#      autocorrelation left, and the estimates.
#
# Reads:  Data/Processed/<spec>/data_analysis.csv   (step 06)
# Writes: Outputs/<spec>/Tables/18_*.csv
#         Outputs/<spec>/Reports/18_spatial_autocorrelation.md
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("18")

settings <- CONFIG$revised_models$autocorrelation

report_start(
  "18",
  "Spatial autocorrelation",
  paste(
    "Moran's I of the residuals of the revised occupancy models, aggregated per",
    "persistent site, and where it is significant, a comparison of the grid",
    "random effect with a smooth of the coordinates."
  )
)

data <- read_data_analysis()

#----------------------------------------------------------#
# Test and compare -----
#----------------------------------------------------------#

tests <- list()
estimates <- list()

for (species in c(SPECIES_NAU, SPECIES_TEL)) {
  code <- species_code(species)
  for (sample in REVISED_SAMPLES) {
    d <- revised_model_data(data, species, sample)
    specs <- revised_occupancy_specs(d, species, sample)
    ids <- vapply(specs, `[[`, character(1), "id")
    suffix <- sample_suffix(sample)
    models <- fit_tmb_set(specs[ids %in% paste0(code, c("_baseline", "_management"), suffix)])

    for (m in models) {
      grid_test <- spatial_autocorrelation_test(m)
      smooth_needed <- settings$fit_smooth == "always" ||
        isTRUE(grid_test$p_value < settings$alpha)
      smooth <- if (smooth_needed) fit_spatial_smooth(m) else NULL
      smooth_test <- if (!is.null(smooth)) spatial_autocorrelation_test(m, fit = smooth) else NULL

      tests[[m$id]] <- data.frame(
        grid_test,
        smooth_fitted = !is.null(smooth),
        moran_observed_smooth = if (!is.null(smooth_test)) smooth_test$moran_observed else NA_real_,
        p_value_smooth = if (!is.null(smooth_test)) smooth_test$p_value else NA_real_,
        AIC_grid = if (!is.null(m$fit)) stats::AIC(m$fit) else NA_real_,
        AIC_smooth = if (!is.null(smooth)) stats::AIC(smooth) else NA_real_,
        stringsAsFactors = FALSE
      )
      if (!is.null(smooth)) {
        estimates[[m$id]] <- dplyr::bind_rows(
          key_estimates(tmb_coef_table(list(m))) %>% dplyr::mutate(random_effect = "grid (1 | SITMAP)"),
          key_estimates(gam_coef_table(smooth, m$id, m$sample)) %>%
            dplyr::mutate(random_effect = sprintf("smooth s(X, Y, k = %d)", settings$smooth_k))
        )
      }
    }
  }
}

#----------------------------------------------------------#
# Results -----
#----------------------------------------------------------#

tests <- dplyr::bind_rows(tests)
report_table(
  tests,
  sprintf(
    paste(
      "Moran's I of the residuals aggregated per persistent site, for the model",
      "with the grid random effect and, where p < %g, for the model with a",
      "smooth of the coordinates instead. AIC_smooth is mgcv's AIC of the",
      "penalised fit, which is not on exactly the same footing as glmmTMB's",
      "marginal AIC; it is indicative only."
    ),
    settings$alpha
  ),
  "18_moran",
  max_rows = nrow(tests)
)

n_autocorrelated <- sum(tests$p_value < settings$alpha, na.rm = TRUE)
report_note(sprintf(
  "%d of %d models show residual spatial autocorrelation at p < %g with the grid random effect.",
  n_autocorrelated, nrow(tests), settings$alpha
))

if (length(estimates)) {
  estimates <- dplyr::bind_rows(estimates) %>%
    dplyr::select(model, sample, term, random_effect, ratio_ci, p_value)
  report_table(
    estimates,
    "Odds ratios (95% CI) with the grid random effect and with the smooth of the coordinates",
    "18_grid_vs_smooth",
    max_rows = nrow(estimates)
  )
}

report_finish()

step_end("18")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

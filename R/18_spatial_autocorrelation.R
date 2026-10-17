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
#   4. The management model is refitted with a spatial random field (SPDE,
#      sdmTMB) in place of the grid random effect, and with spatial+, its
#      management and protection columns stripped of their smooth spatial
#      part, to see whether the estimates are confounded with space (P4.12;
#      R/functions/spatial.R).
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
spde_fits <- list()
spde_estimates <- list()
spatial_plus <- list()

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

      # The management model with a spatial random field in place of the
      # grid square, and with the spatial part of its management and
      # protection terms removed (P4.12).
      if (grepl("_management", m$id)) {
        spde <- fit_spde_model(m)
        if (!is.null(spde)) {
          spde_fits[[m$id]] <- data.frame(
            model = m$id, sample = m$sample, n_obs = m$n_input,
            AIC_grid = if (!is.null(m$fit)) stats::AIC(m$fit) else NA_real_,
            AIC_spde = spde$AIC, sanity_ok = spde$sanity,
            range_km = spde$range_km, sigma_field = spde$sigma,
            stringsAsFactors = FALSE
          )
          spde_estimates[[m$id]] <- dplyr::bind_rows(
            key_estimates(tmb_coef_table(list(m))) %>% dplyr::mutate(random_effect = "grid (1 | SITMAP)"),
            key_estimates(spde$coefficients) %>% dplyr::mutate(model = m$id, random_effect = "SPDE spatial field")
          )
        }
        plus <- fit_spatial_plus(m, MANAGEMENT_TERMS)
        spatial_plus[[m$id]] <- spatial_plus_comparison(m, plus)
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

#----------------------------------------------------------#
# SPDE spatial field and spatial+ -----
#----------------------------------------------------------#

report_section("A spatial random field and spatial+ (P4.12)")

spde_fits <- dplyr::bind_rows(spde_fits)
if (nrow(spde_fits)) {
  spde_fits$delta_AIC_spde_vs_grid <- spde_fits$AIC_spde - spde_fits$AIC_grid
  report_table(
    spde_fits,
    sprintf(
      paste(
        "The management model with a spatial random field (SPDE, sdmTMB; mesh",
        "cutoff %g km) in place of the grid random effect: marginal AIC of both",
        "(both Laplace approximations on the same records), the range at which",
        "the field's correlation falls to about 0.13 and its SD; sanity_ok when",
        "sdmTMB::sanity() finds no problem"
      ),
      settings$spde_mesh_cutoff_km
    ),
    "18_spde",
    max_rows = nrow(spde_fits)
  )
  spde_estimates <- dplyr::bind_rows(spde_estimates) %>%
    dplyr::select(model, sample, term, random_effect, ratio_ci, p_value)
  report_table(
    spde_estimates,
    "Odds ratios (95% CI) of the management model with the grid random effect and with the spatial field",
    "18_grid_vs_spde",
    max_rows = nrow(spde_estimates)
  )
}

spatial_plus <- dplyr::bind_rows(spatial_plus)
if (nrow(spatial_plus)) {
  report_table(
    spatial_plus %>%
      dplyr::transmute(
        model, sample, term,
        odds_ratio = sprintf("%.2f (%.2f-%.2f)", ratio, ratio_low, ratio_high),
        odds_ratio_spatialplus = sprintf("%.2f (%.2f-%.2f)", ratio_spatialplus, ratio_low_spatialplus, ratio_high_spatialplus),
        shift_in_se, flag
      ),
    sprintf(
      paste(
        "spatial+ (Dupont et al. 2022): each management and protection column",
        "replaced by its residual from a smooth of the coordinates (k = %d) and",
        "the model refitted; flag where the estimate moves by more than %g",
        "standard errors, i.e. where it is confounded with space"
      ),
      settings$spatial_plus_k, settings$spatial_plus_flag_se
    ),
    "18_spatial_plus",
    max_rows = nrow(spatial_plus)
  )
}

report_finish()

step_end("18")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

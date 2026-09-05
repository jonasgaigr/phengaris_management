#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#      Step 09 - Occupancy models for Phengaris nausithous
#
#
#----------------------------------------------------------#
#
# Binomial occupancy models for P. nausithous. The response is site occupancy,
# as.factor(POSITIVE). Mixed models carry a random intercept for the year and
# one for the site, the latter written as (1 | X:Y): lme4 turns the two
# coordinate columns into factors and crosses them, so the term is the identity
# of the coordinate pair.
#
# Every formula, family, engine and data subset below is exactly the one used
# in the original Management_analysis_stats.R. What is new is that each model
# is named, its coefficients are written to a table, and a model that fails to
# fit is recorded rather than stopping the script.
#
# Reads:  Data/Processed/data_analysis.csv   (step 06)
# Writes: Outputs/Tables/09_*.csv
#         Outputs/Reports/09_models_p_nausithous.md
#
#----------------------------------------------------------#

message("Step 09: fitting P. nausithous models")

report_start(
  "09",
  "Models P nausithous",
  paste(
    "Binomial occupancy models for Phengaris nausithous, grouped by the",
    "hypothesis each set addresses: baseline space and time, habitat extent",
    "and host plant, management, and conservation status."
  )
)

data <- read_data_analysis()

#----------------------------------------------------------#
# Data subsets -----
#----------------------------------------------------------#

d_nau <- data %>%
  dplyr::filter(DRUH == SPECIES_NAU)

# Models of site area need a positive area.
d_nau_area <- d_nau %>%
  dplyr::filter(AREA_SITE > 0)

# The area null model was fitted on all records with a known area.
d_nau_area_known <- d_nau %>%
  dplyr::filter(is.na(AREA_SITE) == FALSE)

# Management models are restricted to records where mowing was assessed.
d_nau_mow <- d_nau %>%
  dplyr::filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE)

# The grazing null model is restricted to records where grazing was assessed.
d_nau_graze <- d_nau %>%
  dplyr::filter(is.na(GRAZE_MET) == FALSE)

habitat_available <- any(!is.na(data$AREA_SITE))

if (!habitat_available) {
  report_warning(
    "The analysis table carries no habitat attributes, so the models using",
    "AREA_SITE, FSB or HET_OUT cannot be fitted. They are listed below as",
    "failed with the reason recorded. Run step 05 from a machine with access",
    "to the habitat mapping share and re-run steps 06 and 09."
  )
}

#----------------------------------------------------------#
# Model specifications -----
#----------------------------------------------------------#

specs_nau <- list(

  #--------------------------------------------------#
  ## Baseline: space and time -----
  #--------------------------------------------------#
  list(
    id = "nau_null", group = "Baseline",
    label = "Null model with year and site random effects",
    formula = as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
    data = d_nau, engine = "glmer"
  ),
  list(
    id = "nau_year_factor", group = "Baseline",
    label = "Year as a factor",
    formula = as.factor(POSITIVE) ~ as.factor(YEAR),
    data = d_nau, engine = "glm"
  ),
  list(
    id = "nau_year_linear", group = "Baseline",
    label = "Year as a linear trend",
    formula = as.factor(POSITIVE) ~ as.numeric(YEAR),
    data = d_nau, engine = "glm"
  ),
  list(
    id = "nau_year_poly", group = "Baseline",
    label = "Year as a quadratic trend",
    formula = as.factor(POSITIVE) ~ poly(as.numeric(YEAR), 2),
    data = d_nau, engine = "glm"
  ),
  list(
    id = "nau_spatial", group = "Baseline",
    label = "Spatial position",
    formula = as.factor(POSITIVE) ~ X:Y,
    data = d_nau, engine = "glm"
  ),
  list(
    id = "nau_spatiotemporal", group = "Baseline",
    label = "Year and spatial position",
    formula = as.factor(POSITIVE) ~ as.factor(YEAR) + X:Y,
    data = d_nau, engine = "glm"
  ),

  #--------------------------------------------------#
  ## Habitat extent and host plant -----
  #--------------------------------------------------#
  list(
    id = "nau_area_null", group = "Habitat",
    label = "Null model on records with a positive site area",
    formula = as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
    data = d_nau_area, engine = "glmer"
  ),
  list(
    id = "nau_area", group = "Habitat",
    label = "Site area",
    formula = as.factor(POSITIVE) ~ log10(AREA_SITE) + (1 | YEAR) + (1 | X:Y),
    data = d_nau_area, engine = "glmer"
  ),
  list(
    # In the original script this overwrote the mixed area null model under the
    # same name. Both are kept here, under distinct names.
    id = "nau_area_null_glm", group = "Habitat",
    label = "Intercept-only model on records with a known site area",
    formula = as.factor(POSITIVE) ~ 1,
    data = d_nau_area_known, engine = "glm"
  ),
  list(
    id = "nau_area_poly", group = "Habitat",
    label = "Site area, quadratic",
    formula = as.factor(POSITIVE) ~ poly(log10(AREA_SITE), 2) + (1 | YEAR) + (1 | X:Y),
    data = d_nau_area, engine = "glmer"
  ),
  list(
    id = "nau_plant", group = "Habitat",
    label = "Host plant abundance",
    formula = as.factor(POSITIVE) ~ PLANT_QUANT + (1 | YEAR) + (1 | X:Y),
    data = d_nau_area, engine = "glmer"
  ),
  list(
    id = "nau_plant_poly", group = "Habitat",
    label = "Host plant abundance, quadratic",
    formula = as.factor(POSITIVE) ~ poly(PLANT_QUANT, 2) + (1 | YEAR) + (1 | X:Y),
    data = d_nau, engine = "glmer"
  ),
  list(
    id = "nau_resource_density", group = "Habitat",
    label = "Site area by host plant abundance",
    formula = as.factor(POSITIVE) ~ log(AREA_SITE) * as.numeric(PLANT_QUANT) + (1 | YEAR) + (1 | X:Y),
    data = d_nau_area, engine = "glmer"
  ),

  #--------------------------------------------------#
  ## Management -----
  #--------------------------------------------------#
  list(
    id = "nau_mow_null", group = "Management",
    label = "Null model on records where mowing was assessed",
    formula = as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
    data = d_nau_mow, engine = "glmer"
  ),
  list(
    id = "nau_timing", group = "Management",
    label = "Mowing timing",
    formula = as.factor(POSITIVE) ~ as.factor(TIMING) + (1 | YEAR) + (1 | X:Y),
    data = d_nau_mow, engine = "glmer"
  ),
  list(
    id = "nau_method", group = "Management",
    label = "Mowing method",
    formula = as.factor(POSITIVE) ~ as.factor(METHOD) + (1 | YEAR) + (1 | X:Y),
    data = d_nau_mow, engine = "glmer"
  ),
  list(
    id = "nau_method_timing", group = "Management",
    label = "Mowing method by timing (selected management model)",
    formula = as.factor(POSITIVE) ~ as.factor(METHOD) * as.factor(TIMING) + (1 | YEAR) + (1 | X:Y),
    data = d_nau_mow, engine = "glmer"
  ),
  list(
    id = "nau_mow_null_glm", group = "Management",
    label = "Intercept-only model on records where mowing was assessed",
    formula = as.factor(POSITIVE) ~ 1,
    data = d_nau_mow, engine = "glm"
  ),
  list(
    id = "nau_graze_null", group = "Management",
    label = "Null model on records where grazing was assessed",
    formula = as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
    data = d_nau_graze, engine = "glmer"
  ),
  list(
    id = "nau_graze", group = "Management",
    label = "Grazing present",
    formula = as.factor(POSITIVE) ~ as.factor(GRAZE) + (1 | YEAR) + (1 | X:Y),
    data = d_nau, engine = "glmer"
  ),
  list(
    id = "nau_graze_method", group = "Management",
    label = "Grazing intensity",
    formula = as.factor(POSITIVE) ~ as.factor(GRAZE_MET) + (1 | YEAR) + (1 | X:Y),
    data = d_nau, engine = "glmer"
  ),
  list(
    id = "nau_management_het", group = "Management",
    label = "Mowing timing by method by within-site heterogeneity",
    formula = as.factor(POSITIVE) ~ as.factor(TIMING) * as.factor(METHOD) * HET_INN + (1 | YEAR) + (1 | X:Y),
    data = d_nau, engine = "glmer"
  ),

  #--------------------------------------------------#
  ## Conservation status and environment -----
  #--------------------------------------------------#
  list(
    id = "nau_protect", group = "Conservation",
    label = "Any protection",
    formula = as.factor(POSITIVE) ~ as.factor(PROTECT) + (1 | YEAR) + (1 | X:Y),
    data = d_nau, engine = "glmer"
  ),
  list(
    id = "nau_evl", group = "Conservation",
    label = "Natura 2000 membership",
    formula = as.factor(POSITIVE) ~ as.factor(EVL) + (1 | YEAR) + (1 | X:Y),
    data = d_nau, engine = "glmer"
  ),
  list(
    id = "nau_evl_target", group = "Conservation",
    label = "Natura 2000 designated for Phengaris",
    formula = as.factor(POSITIVE) ~ as.factor(EVL_target) + (1 | YEAR) + (1 | X:Y),
    data = d_nau, engine = "glmer"
  ),
  list(
    id = "nau_evl_combined", group = "Conservation",
    label = "Natura 2000 membership and designation (selected protection model)",
    formula = as.factor(POSITIVE) ~ as.factor(EVL) + as.factor(EVL_target) + (1 | YEAR) + (1 | X:Y),
    data = d_nau, engine = "glmer"
  ),
  list(
    # No year random effect here, because year enters as a fixed effect.
    id = "nau_evl_year", group = "Conservation",
    label = "Natura 2000 membership by year",
    formula = as.factor(POSITIVE) ~ as.factor(EVL) * as.numeric(YEAR) + (1 | X:Y),
    data = d_nau, engine = "glmer"
  ),
  list(
    id = "nau_mzchu", group = "Conservation",
    label = "Small-scale protected area",
    formula = as.factor(POSITIVE) ~ as.factor(MZCHU) + (1 | YEAR) + (1 | X:Y),
    data = d_nau, engine = "glmer"
  ),
  list(
    id = "nau_mzchu_evl", group = "Conservation",
    label = "Small-scale protected area by Natura 2000",
    formula = as.factor(POSITIVE) ~ as.factor(MZCHU) * as.factor(EVL) + (1 | YEAR) + (1 | X:Y),
    data = d_nau, engine = "glmer"
  ),
  list(
    id = "nau_ttp", group = "Conservation",
    label = "Regularly managed grassland",
    formula = as.factor(POSITIVE) ~ as.factor(TTP) + (1 | YEAR) + (1 | X:Y),
    data = d_nau, engine = "glmer"
  ),
  list(
    id = "nau_management_ttp", group = "Conservation",
    label = "Mowing timing by method by grassland type",
    formula = as.factor(POSITIVE) ~ as.factor(TIMING) * as.factor(METHOD) * as.factor(TTP) + (1 | YEAR) + (1 | X:Y),
    data = d_nau, engine = "glmer"
  ),
  list(
    id = "nau_fsb", group = "Conservation",
    label = "Habitat quality evaluation",
    formula = as.factor(POSITIVE) ~ as.factor(FSB) + (1 | YEAR) + (1 | X:Y),
    data = d_nau, engine = "glmer"
  ),
  list(
    # The original repeats (1 | YEAR) twice. Kept as written.
    id = "nau_het_inner", group = "Conservation",
    label = "Within-site habitat heterogeneity",
    formula = as.factor(POSITIVE) ~ HET_INN + (1 | YEAR) + (1 | YEAR) + (1 | X:Y),
    data = d_nau, engine = "glmer"
  ),
  list(
    id = "nau_het_outer", group = "Conservation",
    label = "Between-habitat heterogeneity",
    formula = as.factor(POSITIVE) ~ as.numeric(HET_OUT) + (1 | YEAR) + (1 | X:Y),
    data = d_nau, engine = "glmer"
  ),
  list(
    id = "nau_threats", group = "Conservation",
    label = "Number of threats and pressures",
    formula = as.factor(POSITIVE) ~ SUM_THREATS + (1 | YEAR) + (1 | X:Y),
    data = d_nau_area, engine = "glmer"
  )
)

#----------------------------------------------------------#
# Fit -----
#----------------------------------------------------------#

models_nau <- fit_binomial_set(specs_nau)

#----------------------------------------------------------#
# Results -----
#----------------------------------------------------------#

report_model_set(models_nau, "09_nausithous")

#--------------------------------------------------#
## Model comparisons -----
#--------------------------------------------------#

report_section("Model comparisons")

report_note(paste(
  "The comparisons below are the ones the original script printed to the",
  "console, now ordered by AIC with the difference from the best model added."
))

report_table(
  aic_comparison(
    models_nau,
    c("nau_mow_null", "nau_timing", "nau_method", "nau_method_timing", "nau_management_het")
  ),
  "Management models",
  "09_nausithous_aic_management",
  max_rows = 20
)

report_table(
  aic_comparison(
    models_nau,
    c("nau_evl", "nau_evl_target", "nau_evl_combined")
  ),
  "Natura 2000 models",
  "09_nausithous_aic_natura2000",
  max_rows = 20
)

report_table(
  aic_comparison(
    models_nau,
    c("nau_ttp", "nau_management_ttp")
  ),
  "Grassland type models",
  "09_nausithous_aic_grassland",
  max_rows = 20
)

report_table(
  aic_comparison(
    models_nau,
    c("nau_area_null", "nau_area", "nau_area_poly", "nau_plant", "nau_resource_density")
  ),
  "Habitat extent and host plant models",
  "09_nausithous_aic_habitat",
  max_rows = 20
)

report_finish()

message("Step 09 done: ", length(models_nau), " models fitted")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

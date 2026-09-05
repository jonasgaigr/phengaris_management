#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#       Step 10 - Occupancy models for Phengaris teleius
#
#
#----------------------------------------------------------#
#
# Binomial occupancy models for P. teleius, built the same way as the
# P. nausithous models in step 09 and reported in the same format.
#
# Where the original script fitted the same specification twice under different
# names, both are kept here under distinct identifiers, so that nothing is
# silently dropped and the duplication is visible in the results table.
#
# Reads:  Data/Processed/data_analysis.csv   (step 06)
# Writes: Outputs/Tables/10_*.csv
#         Outputs/Reports/10_models_p_teleius.md
#
#----------------------------------------------------------#

message("Step 10: fitting P. teleius models")

report_start(
  "10",
  "Models P teleius",
  paste(
    "Binomial occupancy models for Phengaris teleius, in the same four groups",
    "as for P. nausithous: baseline space and time, habitat extent and host",
    "plant, management, and conservation status."
  )
)

data <- read_data_analysis()

#----------------------------------------------------------#
# Data subsets -----
#----------------------------------------------------------#

d_tel <- data %>%
  dplyr::filter(DRUH == SPECIES_TEL)

d_tel_area <- d_tel %>%
  dplyr::filter(AREA_SITE > 0)

d_tel_mow <- d_tel %>%
  dplyr::filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE)

d_tel_graze <- d_tel %>%
  dplyr::filter(is.na(GRAZE_MET) == FALSE)

# The spatiotemporal model excluded 2018, which the current cleaning step
# removes anyway. The filter is kept so the specification stays as written.
d_tel_no2018 <- data %>%
  dplyr::filter(YEAR != "2018") %>%
  dplyr::filter(DRUH == SPECIES_TEL)

# Restricted to the three habitat evaluations with enough records.
d_tel_fsb <- d_tel %>%
  dplyr::filter(FSB %in% c("T", "X", "moz."))

habitat_available <- any(!is.na(data$AREA_SITE))

if (!habitat_available) {
  report_warning(
    "The analysis table carries no habitat attributes, so the models using",
    "AREA_SITE, FSB or HET_OUT cannot be fitted. They are listed below as",
    "failed with the reason recorded."
  )
}

#----------------------------------------------------------#
# Model specifications -----
#----------------------------------------------------------#

specs_tel <- list(

  #--------------------------------------------------#
  ## Baseline: space and time -----
  #--------------------------------------------------#
  list(
    id = "tel_null", group = "Baseline",
    label = "Null model with year and site random effects",
    formula = as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),
  list(
    # Overwrote the mixed null model under the same name in the original.
    id = "tel_null_glm", group = "Baseline",
    label = "Intercept-only model",
    formula = as.factor(POSITIVE) ~ 1,
    data = d_tel, engine = "glm"
  ),
  list(
    id = "tel_year_factor", group = "Baseline",
    label = "Year as a factor",
    formula = as.factor(POSITIVE) ~ as.factor(YEAR),
    data = d_tel, engine = "glm"
  ),
  list(
    id = "tel_year_linear", group = "Baseline",
    label = "Year as a linear trend",
    formula = as.factor(POSITIVE) ~ as.numeric(YEAR),
    data = d_tel, engine = "glm"
  ),
  list(
    id = "tel_year_poly", group = "Baseline",
    label = "Year as a quadratic trend",
    formula = as.factor(POSITIVE) ~ poly(as.numeric(YEAR), 2),
    data = d_tel, engine = "glm"
  ),
  list(
    id = "tel_spatial", group = "Baseline",
    label = "Spatial position",
    formula = as.factor(POSITIVE) ~ X:Y,
    data = d_tel, engine = "glm"
  ),
  list(
    id = "tel_spatiotemporal", group = "Baseline",
    label = "Year and spatial position, 2018 excluded",
    formula = as.factor(POSITIVE) ~ as.numeric(YEAR) + X:Y,
    data = d_tel_no2018, engine = "glm"
  ),
  list(
    id = "tel_mapping_field", group = "Baseline",
    label = "Mapping field as a fixed effect",
    formula = as.factor(POSITIVE) ~ as.factor(SITMAP),
    data = d_tel, engine = "glm"
  ),
  list(
    id = "tel_random_only", group = "Baseline",
    label = "Year and site random effects only",
    formula = as.factor(POSITIVE) ~ (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),

  #--------------------------------------------------#
  ## Habitat extent and host plant -----
  #--------------------------------------------------#
  list(
    id = "tel_area_null", group = "Habitat",
    label = "Null model on records with a positive site area",
    formula = as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
    data = d_tel_area, engine = "glmer"
  ),
  list(
    id = "tel_area", group = "Habitat",
    label = "Site area (selected habitat model)",
    formula = as.factor(POSITIVE) ~ log10(AREA_SITE) + (1 | YEAR) + (1 | X:Y),
    data = d_tel_area, engine = "glmer"
  ),
  list(
    id = "tel_area_null_glm", group = "Habitat",
    label = "Intercept-only model on records with a positive site area",
    formula = as.factor(POSITIVE) ~ 1,
    data = d_tel_area, engine = "glm"
  ),
  list(
    id = "tel_plant_area_subset", group = "Habitat",
    label = "Host plant abundance, records with a positive site area",
    formula = as.factor(POSITIVE) ~ PLANT_QUANT + (1 | YEAR) + (1 | X:Y),
    data = d_tel_area, engine = "glmer"
  ),
  list(
    id = "tel_plant", group = "Habitat",
    label = "Host plant abundance, all records",
    formula = as.factor(POSITIVE) ~ PLANT_QUANT + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),
  list(
    id = "tel_plant_poly", group = "Habitat",
    label = "Host plant abundance, quadratic",
    formula = as.factor(POSITIVE) ~ poly(PLANT_QUANT, 2) + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),
  list(
    # The original fitted this on all P. teleius records, while the matching
    # P. nausithous model was restricted to AREA_SITE > 0. Because log(0) is
    # -Inf, the unrestricted version cannot be fitted; it is kept as written and
    # reported as a failure rather than quietly given the other model's filter.
    id = "tel_resource_density", group = "Habitat",
    label = "Site area by host plant abundance",
    formula = as.factor(POSITIVE) ~ log(AREA_SITE) * as.numeric(PLANT_QUANT) + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),

  #--------------------------------------------------#
  ## Management -----
  #--------------------------------------------------#
  list(
    id = "tel_mow_null", group = "Management",
    label = "Null model on records where mowing was assessed",
    formula = as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
    data = d_tel_mow, engine = "glmer"
  ),
  list(
    id = "tel_timing_mow_subset", group = "Management",
    label = "Mowing timing, records where mowing was assessed",
    formula = as.factor(POSITIVE) ~ as.factor(TIMING) + (1 | YEAR) + (1 | X:Y),
    data = d_tel_mow, engine = "glmer"
  ),
  list(
    id = "tel_method_mow_subset", group = "Management",
    label = "Mowing method, records where mowing was assessed",
    formula = as.factor(POSITIVE) ~ as.factor(METHOD) + (1 | YEAR) + (1 | X:Y),
    data = d_tel_mow, engine = "glmer"
  ),
  list(
    id = "tel_method_timing_mow_subset", group = "Management",
    label = "Mowing method by timing, records where mowing was assessed",
    formula = as.factor(POSITIVE) ~ as.factor(METHOD) * as.factor(TIMING) + (1 | YEAR) + (1 | X:Y),
    data = d_tel_mow, engine = "glmer"
  ),
  list(
    id = "tel_mow_null_glm", group = "Management",
    label = "Intercept-only model on records where mowing was assessed",
    formula = as.factor(POSITIVE) ~ 1,
    data = d_tel_mow, engine = "glm"
  ),
  list(
    id = "tel_graze_null", group = "Management",
    label = "Null model on records where grazing was assessed",
    formula = as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
    data = d_tel_graze, engine = "glmer"
  ),
  list(
    id = "tel_graze", group = "Management",
    label = "Grazing present",
    formula = as.factor(POSITIVE) ~ as.factor(GRAZE) + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),
  list(
    id = "tel_graze_method", group = "Management",
    label = "Grazing intensity",
    formula = as.factor(POSITIVE) ~ as.factor(GRAZE_MET) + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),
  list(
    id = "tel_method", group = "Management",
    label = "Mowing method, all records",
    formula = as.factor(POSITIVE) ~ as.factor(METHOD) + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),
  list(
    id = "tel_timing", group = "Management",
    label = "Mowing timing, all records",
    formula = as.factor(POSITIVE) ~ as.factor(TIMING) + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),
  list(
    id = "tel_method_timing", group = "Management",
    label = "Mowing method by timing, all records",
    formula = as.factor(POSITIVE) ~ as.factor(METHOD) * as.factor(TIMING) + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),

  #--------------------------------------------------#
  ## Conservation status and environment -----
  #--------------------------------------------------#
  list(
    id = "tel_protect", group = "Conservation",
    label = "Any protection",
    formula = as.factor(POSITIVE) ~ as.factor(PROTECT) + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),
  list(
    id = "tel_evl", group = "Conservation",
    label = "Natura 2000 membership",
    formula = as.factor(POSITIVE) ~ as.factor(EVL) + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),
  list(
    id = "tel_evl_target", group = "Conservation",
    label = "Natura 2000 designated for Phengaris",
    formula = as.factor(POSITIVE) ~ as.factor(EVL_target) + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),
  list(
    id = "tel_evl_combined", group = "Conservation",
    label = "Natura 2000 membership and designation (selected protection model)",
    formula = as.factor(POSITIVE) ~ as.factor(EVL) + as.factor(EVL_target) + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),
  list(
    id = "tel_protection_combined", group = "Conservation",
    label = "Natura 2000 membership, designation and small-scale protection",
    formula = as.factor(POSITIVE) ~ as.factor(EVL) + as.factor(EVL_target) + as.factor(MZCHU) + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),
  list(
    id = "tel_mzchu", group = "Conservation",
    label = "Small-scale protected area",
    formula = as.factor(POSITIVE) ~ as.factor(MZCHU) + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),
  list(
    id = "tel_fsb", group = "Conservation",
    label = "Habitat quality evaluation, all levels",
    formula = as.factor(POSITIVE) ~ as.factor(FSB) + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),
  list(
    id = "tel_fsb_subset", group = "Conservation",
    label = "Habitat quality evaluation, T / X / mosaic only",
    formula = as.factor(POSITIVE) ~ as.factor(FSB) + (1 | YEAR) + (1 | X:Y),
    data = d_tel_fsb, engine = "glmer"
  ),
  list(
    id = "tel_het_inner", group = "Conservation",
    label = "Within-site habitat heterogeneity",
    formula = as.factor(POSITIVE) ~ HET_INN + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),
  list(
    # The original repeats (1 | YEAR) twice in this second version. Kept as written.
    id = "tel_het_inner_dup_year", group = "Conservation",
    label = "Within-site habitat heterogeneity, year term repeated",
    formula = as.factor(POSITIVE) ~ HET_INN + (1 | YEAR) + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  ),
  list(
    id = "tel_het_outer", group = "Conservation",
    label = "Between-habitat heterogeneity",
    formula = as.factor(POSITIVE) ~ HET_OUT + (1 | YEAR) + (1 | X:Y),
    data = d_tel, engine = "glmer"
  )
)

#----------------------------------------------------------#
# Fit -----
#----------------------------------------------------------#

models_tel <- fit_binomial_set(specs_tel)

#----------------------------------------------------------#
# Results -----
#----------------------------------------------------------#

report_model_set(models_tel, "10_teleius")

n_tel_zero_area <- sum(d_tel$AREA_SITE == 0, na.rm = TRUE)

report_warning(
  "`tel_resource_density` cannot be fitted.", n_tel_zero_area, "of the",
  nrow(d_tel), "P. teleius records have AREA_SITE = 0, and the formula takes",
  "log(AREA_SITE), so the model matrix contains -Inf. The equivalent",
  "P. nausithous model (`nau_resource_density`) runs only because the original",
  "restricted it to AREA_SITE > 0 while leaving the P. teleius version",
  "unrestricted. Applying the same filter here would make the two species",
  "comparable and the model fittable, but it changes the specification, so it",
  "is left as written for you to decide."
)

#--------------------------------------------------#
## Model comparisons -----
#--------------------------------------------------#

report_section("Model comparisons")

report_table(
  aic_comparison(
    models_tel,
    c("tel_timing", "tel_method", "tel_method_timing")
  ),
  "Management models, all records",
  "10_teleius_aic_management",
  max_rows = 20
)

report_table(
  aic_comparison(
    models_tel,
    c("tel_mow_null", "tel_timing_mow_subset", "tel_method_mow_subset",
      "tel_method_timing_mow_subset")
  ),
  "Management models, records where mowing was assessed",
  "10_teleius_aic_management_subset",
  max_rows = 20
)

report_table(
  aic_comparison(
    models_tel,
    c("tel_evl", "tel_evl_target", "tel_evl_combined", "tel_protection_combined")
  ),
  "Natura 2000 and protection models",
  "10_teleius_aic_protection",
  max_rows = 20
)

report_table(
  aic_comparison(
    models_tel,
    c("tel_area_null", "tel_area", "tel_plant", "tel_plant_poly",
      "tel_resource_density")
  ),
  "Habitat extent and host plant models",
  "10_teleius_aic_habitat",
  max_rows = 20
)

#--------------------------------------------------#
## Records under appropriate management inside Natura 2000 -----
#--------------------------------------------------#

n_tel_managed_in_evl <-
  d_tel %>%
  dplyr::filter(POSITIVE == 1) %>%
  dplyr::filter(METHOD == 1 & TIMING == 1) %>%
  dplyr::filter(EVL == 1) %>%
  nrow()

report_note(sprintf(
  paste(
    "%d occupied P. teleius records combine appropriate mowing method with",
    "appropriate timing inside a Natura 2000 site."
  ),
  n_tel_managed_in_evl
))

report_finish()

message("Step 10 done: ", length(models_tel), " models fitted")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

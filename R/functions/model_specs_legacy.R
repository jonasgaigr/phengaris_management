#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Model sets: the original (legacy) specification
#
#----------------------------------------------------------#
#
# The model sets of the original analysis, one function per step. Each takes
# the analysis table and returns
#
#   specs        the models to fit: id, label, group, formula, data subset and
#                engine, the arguments of fit_binomial()
#   comparisons  the AIC comparison tables to report: model ids, caption and
#                file stem
#
# Every formula, family, engine and data subset is exactly the one used in the
# original Management_analysis_stats.R; the comments mark the places where
# that original specification is known to be questionable (REVIEW.md). Do not
# change a model here: a revised model set goes into a specification of its
# own, so that the two can be compared side by side.
#
# The response is site occupancy, as.factor(POSITIVE). Mixed models carry a
# random intercept for the year and one for the site, the latter written as
# (1 | X:Y): lme4 turns the two coordinate columns into factors and crosses
# them, so the term is the identity of the coordinate pair.
#
#----------------------------------------------------------#


#--------------------------------------------------#
## P. nausithous (step 09) -----
#--------------------------------------------------#

#' The original P. nausithous model set.
#'
#' @param data  the analysis table, both species
legacy_models_nausithous <- function(data) {
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

  specs <- list(

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

  comparisons <- list(
    list(
      ids = c("nau_mow_null", "nau_timing", "nau_method", "nau_method_timing", "nau_management_het"),
      caption = "Management models",
      stem = "09_nausithous_aic_management"
    ),
    list(
      ids = c("nau_evl", "nau_evl_target", "nau_evl_combined"),
      caption = "Natura 2000 models",
      stem = "09_nausithous_aic_natura2000"
    ),
    list(
      ids = c("nau_ttp", "nau_management_ttp"),
      caption = "Grassland type models",
      stem = "09_nausithous_aic_grassland"
    ),
    list(
      ids = c("nau_area_null", "nau_area", "nau_area_poly", "nau_plant", "nau_resource_density"),
      caption = "Habitat extent and host plant models",
      stem = "09_nausithous_aic_habitat"
    )
  )

  list(specs = specs, comparisons = comparisons)
}

#--------------------------------------------------#
## P. teleius (step 10) -----
#--------------------------------------------------#

#' The original P. teleius model set.
#'
#' @param data  the analysis table, both species
legacy_models_teleius <- function(data) {
  d_tel <- data %>%
    dplyr::filter(DRUH == SPECIES_TEL)

  d_tel_area <- d_tel %>%
    dplyr::filter(AREA_SITE > 0)

  d_tel_mow <- d_tel %>%
    dplyr::filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE)

  d_tel_graze <- d_tel %>%
    dplyr::filter(is.na(GRAZE_MET) == FALSE)

  # Restricted to the three habitat evaluations with enough records.
  d_tel_fsb <- d_tel %>%
    dplyr::filter(FSB %in% c("T", "X", "moz."))

  specs <- list(

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
      # The original excluded 2018; the analysed years start in 2019, so this
      # is all P. teleius records. The label keeps the original wording.
      data = d_tel, engine = "glm"
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

  comparisons <- list(
    list(
      ids = c("tel_timing", "tel_method", "tel_method_timing"),
      caption = "Management models, all records",
      stem = "10_teleius_aic_management"
    ),
    list(
      ids = c("tel_mow_null", "tel_timing_mow_subset", "tel_method_mow_subset", "tel_method_timing_mow_subset"),
      caption = "Management models, records where mowing was assessed",
      stem = "10_teleius_aic_management_subset"
    ),
    list(
      ids = c("tel_evl", "tel_evl_target", "tel_evl_combined", "tel_protection_combined"),
      caption = "Natura 2000 and protection models",
      stem = "10_teleius_aic_protection"
    ),
    list(
      ids = c("tel_area_null", "tel_area", "tel_plant", "tel_plant_poly", "tel_resource_density"),
      caption = "Habitat extent and host plant models",
      stem = "10_teleius_aic_habitat"
    )
  )

  list(specs = specs, comparisons = comparisons)
}

#--------------------------------------------------#
## Both species (step 11) -----
#--------------------------------------------------#

#' The original both-species model set.
#'
#' @param data  the analysis table, both species
legacy_models_both <- function(data) {
  specs <- list(
    list(
      id = "both_species", group = "Both species",
      label = "Species only",
      formula = as.factor(POSITIVE) ~ as.factor(DRUH) + (1 | YEAR) + (1 | X:Y),
      data = data, engine = "glmer"
    ),
    list(
      id = "both_management_type", group = "Both species",
      label = "Species, mowing and grazing",
      formula = as.factor(POSITIVE) ~ as.factor(DRUH) + as.factor(MOW) + as.factor(GRAZE) + (1 | YEAR) + (1 | X:Y),
      data = data, engine = "glmer"
    ),
    list(
      id = "both_mapped_habitat", group = "Both species",
      label = "Species and mapped habitat code",
      formula = as.factor(POSITIVE) ~ as.factor(DRUH) + as.factor(BIOTOP) + (1 | YEAR) + (1 | X:Y),
      data = data, engine = "glmer"
    ),
    list(
      # ZARUST appears twice in the original formula. R drops the repeat, so the
      # fitted model is unaffected, but the term is left in place as written.
      id = "both_recorded_habitat", group = "Both species",
      label = "Species and recorded habitat types",
      formula = as.factor(POSITIVE) ~ as.factor(DRUH) +
        as.factor(TTP) + as.factor(ZARUST) + as.factor(PRIKOP) + as.factor(JINY) +
        as.factor(MOW) + as.factor(ZARUST) +
        (1 | YEAR) + (1 | X:Y),
      data = data, engine = "glmer"
    ),
    list(
      id = "both_cooccurrence", group = "Both species",
      label = "Co-occurrence of the two species by species",
      formula = as.factor(POSITIVE) ~ as.factor(SPEC_NUM) * as.factor(DRUH) + (1 | YEAR) + (1 | (X:Y)),
      data = data, engine = "glmer"
    )
  )

  comparisons <- list(
    list(
      ids = c("both_species", "both_management_type", "both_recorded_habitat", "both_mapped_habitat", "both_cooccurrence"),
      caption = "Both-species models",
      stem = "11_both_species_aic"
    )
  )

  list(specs = specs, comparisons = comparisons)
}

#--------------------------------------------------#
## Choosing a model set -----
#--------------------------------------------------#

#' The model set of one step under one specification.
#'
#' @param set      the specification's model set, config.yml `model_set`
#' @param species  "nausithous", "teleius" or "both"
#' @param data     the analysis table
model_set_for <- function(set, species = c("nausithous", "teleius", "both"), data) {
  species <- match.arg(species)
  builder <- switch(
    set,
    legacy = switch(
      species,
      nausithous = legacy_models_nausithous,
      teleius    = legacy_models_teleius,
      both       = legacy_models_both
    ),
    stop("Unknown model set in config.yml: ", set, call. = FALSE)
  )
  builder(data)
}

#----------------------------------------------------------#
# End model sets -----
#----------------------------------------------------------#

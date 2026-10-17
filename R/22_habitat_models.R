#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#           Step 22 - Habitat and landscape models
#
#
#----------------------------------------------------------#
#
# The management model of step 17 extended by the habitat and the landscape,
# as the literature review proposed (R/functions/habitat_models.R):
#
#   P4.5  the mowing timing the surveyors recorded, checked against the notes
#         saying the site was freshly mown at a visit between 1 July and
#         1 September, when no cut should fall (AOPK);
#   P4.6  host-plant abundance as a linear (ordered) term against unordered;
#         the shares of Sanguisorba meadows, wet meadows (a proxy for the
#         Myrmica host ants) and X habitats of the site; on the 2021-2024
#         polygon sites, the share near the edge;
#   P4.7  the landscape at the radius each variable fits best (0.2-4 km), and
#         a model with the management, the local habitat and the landscape
#         variables that improve the management model.
#
# Reads:  Data/Processed/<spec>/data_analysis.csv   (step 06)
# Writes: Outputs/<spec>/Tables/22_*.csv, Figures/22_*.png
#         Outputs/<spec>/Reports/22_habitat_models.md
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("22")

report_start(
  "22",
  "Habitat and landscape models",
  paste(
    "The management model extended by the local habitat of the site and by the",
    "landscape around it, with the radius of each landscape variable chosen by",
    "AIC, and a check of the recorded mowing timing."
  )
)

report_warning(
  "Habitat and landscape describe the site, not the visit, and several of them",
  "were not chosen in advance: read the estimates as associations, and the",
  "radii chosen by AIC as the scale at which each variable relates to the",
  "records best, not as a tested hypothesis."
)

data <- read_data_analysis()
species_names <- c(nau = "P. nausithous", tel = "P. teleius")
species_files <- c(nau = "nausithous", tel = "teleius")
radii <- unlist(CONFIG$landscape$radii_m)
landscape_available <- all(as.vector(outer(landscape_variables(), radii, landscape_column)) %in% names(data))

#----------------------------------------------------------#
# Mowing timing against the notes -----
#----------------------------------------------------------#

report_section("Mowing timing against the notes (P4.5)")

report_note(sprintf(
  paste(
    "An objective timing rule, no cut between %s and %s (AOPK), needs the",
    "dates of the cuts, which the field form does not record. The only",
    "objective evidence of a cut inside that window is a note saying the site",
    "was freshly mown at a visit inside it. Such visits should have their",
    "timing recorded as inappropriate (TIMING = 0). Recording the cut dates",
    "would allow the rule itself."
  ),
  CONFIG$management$no_cut_window[[1]], CONFIG$management$no_cut_window[[2]]
))
observed <- data %>%
  dplyr::filter(!IMPUTED) %>%
  dplyr::mutate(
    freshly_mown_in_window = fresh_mown(POZNAMKA) & in_no_cut_window(DATUM_OD),
    timing = dplyr::case_when(
      is.na(TIMING) ~ "not recorded", TIMING == 1 ~ "appropriate", TRUE ~ "inappropriate"
    )
  )
report_table(
  observed %>%
    dplyr::filter(freshly_mown_in_window) %>%
    dplyr::count(DRUH, timing, name = "records"),
  paste(
    "Observed records whose note says the site was freshly mown at a visit inside",
    "the no-cut window, by the recorded timing"
  ),
  "22_timing_check"
)

#----------------------------------------------------------#
# Models of each species -----
#----------------------------------------------------------#

all_models <- list()
for (species in c(SPECIES_NAU, SPECIES_TEL)) {
  code <- species_code(species)
  prefix <- paste0("22_", species_files[[code]])
  report_section(paste("Habitat and landscape,", species_names[[code]]))
  model_data <- lapply(REVISED_SAMPLES, function(s) habitat_model_data(revised_model_data(data, species, s)))
  suffix_of <- function(s) sample_suffix(s)

  #--------------------------------------------------#
  ## Host plant: linear or unordered -----
  #--------------------------------------------------#

  plant_models <- fit_tmb_set(unlist(lapply(REVISED_SAMPLES, function(s) {
    d <- model_data[[match(s, REVISED_SAMPLES)]]
    other <- setdiff(MANAGEMENT_TERMS, "PLANT")
    list(
      list(
        id = paste0(code, "_plant_unordered", suffix_of(s)), label = "Host plant unordered (2 df)",
        group = "Host plant", formula = revised_formula(terms = c(other, "PLANT")), data = d, sample = s
      ),
      list(
        id = paste0(code, "_plant_linear", suffix_of(s)), label = "Host plant linear in its class (1 df)",
        group = "Host plant", formula = revised_formula(terms = c(other, "PLANT_LINEAR")), data = d, sample = s
      ),
      list(
        id = paste0(code, "_plant_ordered", suffix_of(s)), label = "Host plant ordered: linear and quadratic contrasts",
        group = "Host plant", formula = revised_formula(terms = c(other, "PLANT_ORDERED")), data = d, sample = s
      )
    )
  }), recursive = FALSE))
  plant_aic <- dplyr::bind_rows(lapply(REVISED_SAMPLES, function(s) {
    aic_table_tmb(plant_models, paste0(code, c("_plant_unordered", "_plant_linear"), suffix_of(s)))
  }))
  report_table(
    plant_aic,
    paste(
      "Host-plant abundance (single, abundant, dominant) as an unordered factor",
      "against a linear term (on the same records)"
    ),
    paste0(prefix, "_host_plant_aic")
  )
  report_table(
    key_estimates(tmb_coef_table(plant_models[grepl("_plant_(linear|ordered)", names(plant_models))])) %>%
      dplyr::filter(grepl("^PLANT", term)),
    paste(
      "Host plant as a linear term (odds ratio per class) and as an ordered",
      "factor (.L linear, .Q quadratic orthogonal contrasts)"
    ),
    paste0(prefix, "_host_plant_estimates")
  )

  #--------------------------------------------------#
  ## Local habitat -----
  #--------------------------------------------------#

  local_terms <- c("SANGUISORBA_10", "WET_10", "X_10")
  local_specs <- unlist(lapply(REVISED_SAMPLES, function(s) {
    d <- model_data[[match(s, REVISED_SAMPLES)]]
    d <- d[stats::complete.cases(d[, local_terms]), , drop = FALSE]
    d_2021 <- d %>%
      dplyr::filter(
        YEAR >= CONFIG$design$protocol_change_year, GEOMETRY_TYPE == "polygon",
        !is.na(GRAZE), !is.na(LOG_AREA), !is.na(EDGE_10)
      )
    list(
      list(
        id = paste0(code, "_local_null", suffix_of(s)), label = "Management model, records with the habitat",
        group = "Local habitat", formula = revised_formula(terms = MANAGEMENT_TERMS), data = d, sample = s
      ),
      list(
        id = paste0(code, "_local", suffix_of(s)), label = "+ Sanguisorba, wet and X shares of the site",
        group = "Local habitat", formula = revised_formula(terms = c(MANAGEMENT_TERMS, local_terms)),
        data = d, sample = s
      ),
      list(
        id = paste0(code, "_local_2021_null", suffix_of(s)), label = "2021-2024 polygon model",
        group = "Local habitat",
        formula = revised_formula(terms = c(MANAGEMENT_TERMS, "GRAZE", "LOG_AREA"), era = FALSE),
        data = d_2021, sample = s
      ),
      list(
        id = paste0(code, "_local_2021", suffix_of(s)), label = "2021-2024 polygon model + shares + edge share",
        group = "Local habitat",
        formula = revised_formula(
          terms = c(MANAGEMENT_TERMS, "GRAZE", "LOG_AREA", local_terms, "EDGE_10"), era = FALSE
        ),
        data = d_2021, sample = s
      )
    )
  }), recursive = FALSE)
  local_models <- fit_tmb_set(local_specs)
  report_table(
    dplyr::bind_rows(lapply(REVISED_SAMPLES, function(s) {
      dplyr::bind_rows(
        aic_table_tmb(local_models, paste0(code, c("_local_null", "_local"), suffix_of(s))),
        aic_table_tmb(local_models, paste0(code, c("_local_2021_null", "_local_2021"), suffix_of(s)))
      )
    })),
    "The local habitat added to the management model and to the 2021-2024 polygon model (AIC on the same records)",
    paste0(prefix, "_local_aic")
  )
  report_table(
    key_estimates(tmb_coef_table(local_models[!grepl("_null", names(local_models), fixed = TRUE)])),
    paste(
      "Odds ratios (95% CI) with the local habitat: SANGUISORBA_10, WET_10,",
      "X_10 and EDGE_10 per 10 percentage points of the site in Sanguisorba",
      "meadows, wet meadows, X habitats and within the edge zone"
    ),
    paste0(prefix, "_local_estimates"),
    max_rows = 80
  )

  #--------------------------------------------------#
  ## Landscape: the scale of each variable -----
  #--------------------------------------------------#

  landscape_models <- list()
  if (landscape_available) {
    scale_specs <- landscape_scale_specs(model_data[["with_imputed"]], species, REVISED_SAMPLES[["with_imputed"]])
    scale_models <- fit_tmb_set(scale_specs)
    scales <- landscape_scales(scale_models, scale_specs)
    report_table(
      scales$table %>%
        dplyr::select(
          variable, radius_m, n_obs, AIC, delta_AIC_vs_management, odds_ratio, or_low, or_high, p_value, converged
        ),
      paste(
        "Each landscape variable at each radius added to the management model",
        "(sample with the imputed absences, all on the same records); odds ratio",
        "per 10 percentage points of the circle"
      ),
      paste0(prefix, "_landscape_scales"),
      max_rows = nrow(scales$table)
    )
    report_figure(
      ggplot2::ggplot(scales$table, ggplot2::aes(x = radius_m, y = delta_AIC_vs_management)) +
        ggplot2::geom_hline(yintercept = 0, colour = "grey60") +
        ggplot2::geom_hline(yintercept = -CONFIG$landscape$min_aic_gain, colour = "grey60", linetype = "dashed") +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(~variable) +
        ggplot2::scale_x_log10(breaks = radii) +
        ggplot2::labs(x = "\nradius (m)", y = "delta AIC against the management model\n") +
        ggplot2::theme_classic(base_size = 12),
      paste("Scale of effect of the landscape variables,", species_names[[code]]),
      paste0(prefix, "_landscape_scales"),
      width = 10, height = 7
    )
    chosen <- scales$best %>% dplyr::filter(delta_AIC_vs_management < -CONFIG$landscape$min_aic_gain)
    report_table(
      scales$best %>% dplyr::mutate(in_combined_model = variable %in% chosen$variable),
      sprintf(
        paste(
          "The radius of each landscape variable with the lowest AIC; a variable",
          "enters the combined model when it improves the management model by",
          "more than %g AIC"
        ),
        CONFIG$landscape$min_aic_gain
      ),
      paste0(prefix, "_landscape_best")
    )

    landscape_terms <- landscape_column(chosen$variable, chosen$radius_m)
    landscape_models <- fit_tmb_set(lapply(REVISED_SAMPLES, function(s) {
      d <- model_data[[match(s, REVISED_SAMPLES)]]
      list(
        id = paste0(code, "_habitat_landscape", suffix_of(s)),
        label = "Management + local habitat + landscape at the chosen radii",
        group = "Habitat and landscape",
        formula = revised_formula(terms = c(MANAGEMENT_TERMS, local_terms, landscape_terms)),
        data = d, sample = s
      )
    }))
    combined <- key_estimates(tmb_coef_table(landscape_models))
    report_table(
      combined,
      paste(
        "Odds ratios (95% CI) of the combined model: management, local habitat",
        "and the landscape variables at their chosen radius (per 10 percentage",
        "points)"
      ),
      paste0(prefix, "_habitat_landscape_estimates"),
      max_rows = nrow(combined)
    )
  } else {
    report_warning(
      "No landscape variables in the analysis table: step 21 had no landscape",
      "rasters (R/tools/build_landscape_layers.R). The landscape models are skipped."
    )
  }

  models <- c(plant_models, local_models, landscape_models)
  all_models <- c(all_models, models)
  report_section(paste("All models of", species_names[[code]]), level = 3)
  report_tmb_set(models, prefix)
}

report_finish()

message("  ", length(all_models), " models fitted")
step_end("22")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

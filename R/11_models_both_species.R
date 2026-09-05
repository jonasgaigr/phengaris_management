#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#    Step 11 - Models and tests across both species
#
#
#----------------------------------------------------------#
#
# Models fitted on both species at once, with species as a fixed effect, plus
# the non-parametric comparisons of abundance and occupancy between the two.
#
# Reads:  Data/Processed/data_analysis.csv   (step 06)
# Writes: Outputs/Tables/11_*.csv
#         Outputs/Reports/11_models_both_species.md
#
#----------------------------------------------------------#

message("Step 11: fitting both-species models")

report_start(
  "11",
  "Models both species",
  paste(
    "Models fitted on the two species jointly, with species as a fixed effect,",
    "and the non-parametric tests comparing abundance and occupancy between",
    "P. nausithous and P. teleius."
  )
)

data <- read_data_analysis()

#----------------------------------------------------------#
# Model specifications -----
#----------------------------------------------------------#

specs_both <- list(
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

models_both <- fit_binomial_set(specs_both)

#----------------------------------------------------------#
# Results -----
#----------------------------------------------------------#

report_model_set(models_both, "11_both_species")

report_table(
  aic_comparison(
    models_both,
    c("both_species", "both_management_type", "both_recorded_habitat",
      "both_mapped_habitat", "both_cooccurrence")
  ),
  "Both-species models",
  "11_both_species_aic",
  max_rows = 20
)

#----------------------------------------------------------#
# Non-parametric comparisons between the species -----
#----------------------------------------------------------#

report_section("Comparisons between the species")

#' One row of a test result table.
test_row <- function(description, test) {
  data.frame(
    comparison = description,
    method     = test$method,
    statistic  = unname(test$statistic),
    parameter  = if (is.null(test$parameter)) NA_real_ else unname(test$parameter),
    p_value    = test$p.value,
    stringsAsFactors = FALSE
  )
}

pocet_nau <- data %>%
  dplyr::filter(DRUH == SPECIES_NAU) %>%
  dplyr::pull(POCET)

pocet_tel <- data %>%
  dplyr::filter(DRUH == SPECIES_TEL) %>%
  dplyr::pull(POCET)

positive_nau <- data %>%
  dplyr::filter(DRUH == SPECIES_NAU) %>%
  dplyr::pull(POSITIVE)

positive_tel <- data %>%
  dplyr::filter(DRUH == SPECIES_TEL) %>%
  dplyr::pull(POSITIVE)

species_tests <- dplyr::bind_rows(
  test_row(
    "Counted specimens, P. nausithous vs P. teleius",
    stats::wilcox.test(pocet_nau, pocet_tel)
  ),
  test_row(
    "Counted specimens by species",
    stats::kruskal.test(
      data %>% dplyr::pull(POCET),
      data %>% dplyr::pull(DRUH)
    )
  ),
  test_row(
    "Site occupancy, P. nausithous vs P. teleius",
    stats::wilcox.test(positive_nau, positive_tel)
  )
)

report_table(
  species_tests,
  "Non-parametric comparisons between the species",
  "11_species_comparisons"
)

report_warning(
  "The original script also ran a Kruskal-Wallis test comparing POSITIVE",
  "against POSITIVE on an object that was never created. That test compared a",
  "vector with itself and could not return anything meaningful, so it is not",
  "reproduced here."
)

report_finish()

message("Step 11 done: ", length(models_both), " models fitted")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

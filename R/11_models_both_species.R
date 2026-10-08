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
# Reads:  Data/Processed/<spec>/data_analysis.csv   (step 06)
# Writes: Outputs/<spec>/Tables/11_*.csv
#         Outputs/<spec>/Reports/11_models_both_species.md
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("11")

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
# Model set -----
#----------------------------------------------------------#

# The models and the AIC comparisons of this specification; see
# R/functions/model_specs_legacy.R for the original model set.
model_set <- model_set_for(spec_settings()$model_set, "both", data)

#----------------------------------------------------------#
# Fit -----
#----------------------------------------------------------#

models_both <- fit_binomial_set(model_set$specs)

#----------------------------------------------------------#
# Results -----
#----------------------------------------------------------#

report_model_set(models_both, "11_both_species")

report_comparisons(models_both, model_set$comparisons)

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

message("  ", length(models_both), " models fitted")
step_end("11")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

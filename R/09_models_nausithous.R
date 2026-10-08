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
# Reads:  Data/Processed/<spec>/data_analysis.csv   (step 06)
# Writes: Outputs/<spec>/Tables/09_*.csv
#         Outputs/<spec>/Reports/09_models_p_nausithous.md
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("09")

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
# Data -----
#----------------------------------------------------------#

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
# Model set -----
#----------------------------------------------------------#

# The models and the AIC comparisons of this specification; see
# R/functions/model_specs_legacy.R for the original model set.
model_set <- model_set_for(spec_settings()$model_set, "nausithous", data)

#----------------------------------------------------------#
# Fit -----
#----------------------------------------------------------#

models_nau <- fit_binomial_set(model_set$specs)

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
  "console, ordered by AIC with the difference from the best model added.",
  "Every model of a comparison is refitted on the same records: those in",
  "every model's own subset and complete on all the variables of the set.",
  "n_own_data is the number of records the model's own fit used."
))

report_comparisons(model_set$specs, model_set$comparisons)

report_finish()

message("  ", length(models_nau), " models fitted")
step_end("09")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

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
# Reads:  Data/Processed/<spec>/data_analysis.csv   (step 06)
# Writes: Outputs/<spec>/Tables/10_*.csv
#         Outputs/<spec>/Reports/10_models_p_teleius.md
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("10")

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
# Data -----
#----------------------------------------------------------#

# P. teleius records, for the notes at the end of this report.
d_tel <- data %>%
  dplyr::filter(DRUH == SPECIES_TEL)

habitat_available <- any(!is.na(data$AREA_SITE))

if (!habitat_available) {
  report_warning(
    "The analysis table carries no habitat attributes, so the models using",
    "AREA_SITE, FSB or HET_OUT cannot be fitted. They are listed below as",
    "failed with the reason recorded."
  )
}

#----------------------------------------------------------#
# Model set -----
#----------------------------------------------------------#

# The models and the AIC comparisons of this specification; see
# R/functions/model_specs_legacy.R for the original model set.
model_set <- model_set_for(spec_settings()$model_set, "teleius", data)

#----------------------------------------------------------#
# Fit -----
#----------------------------------------------------------#

models_tel <- fit_binomial_set(model_set$specs)

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

report_comparisons(models_tel, model_set$comparisons)

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

message("  ", length(models_tel), " models fitted")
step_end("10")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

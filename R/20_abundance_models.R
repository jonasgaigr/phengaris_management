#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#            Step 20 - Counts at occupied sites
#
#
#----------------------------------------------------------#
#
# P3.8, a supplement to the occupancy models: the number of adults counted
# (POCET) at the visits where the species was found. Counts exist only for
# positive records (at least one adult; no zeros), so they are modelled with
# a zero-truncated negative binomial, with the management terms and the survey
# design of the revised models. Only observed records count: an imputed
# absence has no count.
#
# P4.10 adds the counts standardised by the flight curve (its log relative
# activity as the offset, in place of the season terms), and a hurdle model
# of every observed visit: the zeros of the negative visits and the
# truncated counts of the positive ones together.
#
# Reads:  Data/Processed/<spec>/data_analysis.csv   (step 06)
# Writes: Outputs/<spec>/Tables/20_*.csv
#         Outputs/<spec>/Reports/20_abundance_models.md
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("20")

report_start(
  "20",
  "Abundance models",
  paste(
    "Zero-truncated negative-binomial models of the number of adults counted at",
    "the visits where the species was found, as a supplement to the occupancy",
    "models. Rate ratios are ratios of the mean count before truncation; the",
    "predictions are expected counts given that the species was found."
  )
)

report_warning(
  "A count from one visit is an index of the population, not its size, and",
  "depends on the site area surveyed and on the weather; the area is in the",
  "2021-2024 polygon model only, because the 2019-2020 sites are points."
)

data <- read_data_analysis()
species_names <- c(nau = "P. nausithous", tel = "P. teleius")
species_files <- c(nau = "nausithous", tel = "teleius")

counts <- list()
for (species in c(SPECIES_NAU, SPECIES_TEL)) {
  code <- species_code(species)
  prefix <- paste0("20_", species_files[[code]])
  report_section(paste("Counts of", species_names[[code]]))

  d <- revised_model_data(data, species, REVISED_SAMPLES[["observed"]])
  positive <- d %>% dplyr::filter(POSITIVE == 1)
  counts[[code]] <- tibble::tibble(
    species = species,
    positive_records = nrow(positive),
    with_count = sum(!is.na(positive$POCET)),
    median = stats::median(positive$POCET, na.rm = TRUE),
    mean = mean(positive$POCET, na.rm = TRUE),
    max = max(positive$POCET, na.rm = TRUE)
  )

  models <- fit_tmb_set(revised_abundance_specs(d, species))
  report_table(
    key_estimates(tmb_coef_table(models)),
    "Rate ratios (95% CI) of the count",
    paste0(prefix, "_key_estimates"),
    max_rows = 40
  )

  # The counts standardised by the flight curve, and the hurdle model of
  # every visit (P4.10).
  hurdle_models <- fit_tmb_set(hurdle_specs(d, species))
  hurdle_estimates <- key_estimates(tmb_coef_table(hurdle_models)) %>%
    dplyr::mutate(
      part = ifelse(grepl("^zi:", term), "zero part: odds ratio of a negative visit", "count part: rate ratio"),
      term = sub("^zi:", "", term),
      .after = sample
    )
  report_table(
    hurdle_estimates,
    paste(
      "Counts with the log relative activity of the flight curve as their offset",
      "(the count at the peak of the season): at the positive visits",
      "(abundance_offset), and the hurdle model of every observed visit, whose",
      "zero part is the chance of a negative record (an odds ratio below 1 means",
      "the species was recorded more often) and whose count part is the",
      "truncated count (95% CI)"
    ),
    paste0(prefix, "_hurdle_estimates"),
    max_rows = nrow(hurdle_estimates)
  )
  models <- c(models, hurdle_models)
  pred <- marginal_predictions(models[[paste0(code, "_abundance")]], by = c("TIMING", "METHOD"))
  report_table(
    pred,
    "Expected count given presence by mowing timing and method (random effects at zero; 95% CI)",
    paste0(prefix, "_predictions_management")
  )
  report_tmb_set(models, prefix, ratio = "rate ratio")
}

report_section("Counts")
report_table(dplyr::bind_rows(counts), "Counted adults at the in-season positive records", "20_count_summary")

report_finish()

step_end("20")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

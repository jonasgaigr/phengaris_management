#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#           Step 25 - Robustness of the key estimates
#
#
#----------------------------------------------------------#
#
# The odds ratios of the management terms and of the EVL designated for the
# species, collected from the result tables of steps 17-24: the management
# model in its three samples, with connectivity, on the fixed windows of the
# flight season, on the 2021-2024 polygons, with the habitat and the
# landscape, with a spatial field and under spatial+, as the occupancy part
# of the mixed-design model and with a known detection, and after matching
# (R/functions/synthesis.R). Nothing is refitted.
#
# Reads:  Outputs/<spec>/Tables/17_*, 18_*, 22_*, 23_*, 24_*   (steps 17-24)
# Writes: Outputs/<spec>/Tables/25_*.csv, Figures/25_*.png
#         Outputs/<spec>/Reports/25_robustness_of_the_key_estimates.md
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("25")

report_start(
  "25",
  "Robustness of the key estimates",
  paste(
    "The odds ratios of the management terms and of the EVL designated for the",
    "species, from every analysis of steps 17-24, side by side."
  )
)

report_note(paste(
  "Each row is one analysis of one sample. The management model (step 17) is",
  "the reference; the others change one thing each: the records (samples,",
  "windows, 2021-2024 polygons), the covariates (connectivity, habitat and",
  "landscape), the spatial structure (SPDE field, spatial+), the detection",
  "(occupancy part of the mixed-design model, known detection p = 0.7), or the",
  "comparison (matching, for the EVL only). The occupancy model estimates",
  "occupancy rather than the probability of recording, so its odds ratios are",
  "expected to lie farther from 1."
))

species_names <- c(nau = "P. nausithous", tel = "P. teleius")
species_files <- c(nau = "nausithous", tel = "teleius")
all_rows <- list()

for (species in c(SPECIES_NAU, SPECIES_TEL)) {
  code <- species_code(species)
  prefix <- paste0("25_", species_files[[code]])
  report_section(paste("Robustness,", species_names[[code]]))

  robustness <- collect_robustness(code, species_files[[code]])
  all_rows[[code]] <- dplyr::mutate(robustness, species = species, .before = 1)

  summary_table <- summarise_robustness(robustness)
  report_table(
    summary_table,
    sprintf(
      paste(
        "Per sample and term: the number of analyses, the range of their odds",
        "ratios, how many lie above 1 and how many 95%% intervals exclude 1.",
        "not_estimable: fits whose upper limit is %g or more times the lower one",
        "(separated terms), which are left out of the other columns"
      ),
      CONFIG$synthesis$max_interval_ratio
    ),
    paste0(prefix, "_robustness_summary"),
    max_rows = nrow(summary_table)
  )

  wide <- robustness %>%
    dplyr::mutate(estimable = is_estimable(robustness)) %>%
    dplyr::filter(sample == REVISED_SAMPLES[[if (code == "tel") "plausible" else "with_imputed"]]) %>%
    dplyr::mutate(value = dplyr::case_when(
      is.na(odds_ratio) ~ NA_character_,
      !estimable ~ "not estimable",
      TRUE ~ sprintf("%.2f (%.2f-%.2f)", odds_ratio, or_low, or_high)
    )) %>%
    dplyr::select(analysis, term_label, value) %>%
    tidyr::pivot_wider(names_from = term_label, values_from = value)
  report_table(
    wide,
    sprintf(
      "Odds ratios (95%% interval) by analysis, sample %s",
      if (code == "tel") "with plausible imputed (the one to read for P. teleius)" else "with imputed"
    ),
    paste0(prefix, "_robustness"),
    max_rows = nrow(wide)
  )

  # Separated fits stay in the tables but are not drawn.
  report_figure(
    plot_robustness(robustness),
    sprintf(
      paste(
        "Odds ratios (95%% interval) of the key terms across the analyses, grouped",
        "by what each changes against the management model of step 17: the",
        "covariates (step 17, step 22), the records (step 17), the spatial",
        "structure (step 18), the detection (step 23) and, for the EVL, the",
        "comparison (step 24); in the three samples. Fits without a finite",
        "interval, or whose upper limit is %g or more times the lower one, are",
        "left out, %s"
      ),
      CONFIG$synthesis$max_interval_ratio, species_names[[code]]
    ),
    paste0(prefix, "_robustness"),
    height = 17
  )
}

all_rows <- dplyr::bind_rows(all_rows)
report_csv(all_rows, "Every odds ratio of the synthesis, with its source table", "25_robustness_all")
validate_table(
  all_rows, "robustness",
  key = c("species", "analysis", "sample", "term"),
  not_na = c("species", "analysis", "sample", "term")
)

report_finish()

step_end("25")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

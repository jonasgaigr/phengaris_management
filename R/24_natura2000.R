#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#           Step 24 - Natura 2000 sites and the persistence of populations
#
#
#----------------------------------------------------------#
#
# P4.8 and P4.9: is the species found more often inside the Natura 2000 sites
# (EVL) designated for it, once the selection of good sites for designation
# is accounted for, and did the populations known in 2012-2018 persist into
# 2019-2024 more often inside them (R/functions/natura2000.R)?
#
#   matching     the records inside an EVL designated for their species
#                matched to records outside on covariates designation cannot
#                have changed (config.yml `natura2000: covariates`): the
#                site's natural and Sanguisorba shares, its elevation, the
#                grassland in the landscape, the connectivity and the
#                position; exactly within the field-form era. Full matching
#                on the propensity score (MatchIt), then the occupancy model
#                with the matching weights. Compared with the unmatched and
#                the covariate-adjusted model.
#   persistence  the visits of 2019-2024 to sites where the species was found
#                in 2012-2018 (within the matching distance): the chance of
#                finding it again, inside and outside the EVLs; and on all
#                visits, whether the EVL association differs between the
#                sites with and without a known earlier population.
#
# Both are associations: the EVLs predate both periods, and a population
# known before 2019 is known because someone looked there.
#
# Reads:  Data/Processed/<spec>/data_analysis.csv   (step 06)
#         Data/Processed/<spec>/connectivity_selected.csv   (step 17)
# Writes: Outputs/<spec>/Tables/24_*.csv, Figures/24_*.png
#         Outputs/<spec>/Reports/24_natura2000.md
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("24")

settings <- CONFIG$natura2000

report_start(
  "24",
  "Natura 2000 and persistence",
  paste(
    "The association of the species with the Natura 2000 sites designated for",
    "it, after matching the records inside them to comparable records outside,",
    "and the persistence of the populations known in 2012-2018."
  )
)

report_warning(
  "These are associations, not effects of protection. Matching balances only",
  "the covariates it is given; the persistence analysis compares sites where",
  "someone found the species before, inside and outside the EVLs."
)

data <- read_data_analysis()
selected <- readr::read_csv(processed_path("connectivity_selected.csv"), show_col_types = FALSE)
species_names <- c(nau = "P. nausithous", tel = "P. teleius")
species_files <- c(nau = "nausithous", tel = "teleius")
season <- c("ERA", season_terms())

format_ratio <- function(ratio, low, high) sprintf("%.2f (%.2f-%.2f)", ratio, low, high)

for (species in c(SPECIES_NAU, SPECIES_TEL)) {
  code <- species_code(species)
  prefix <- paste0("24_", species_files[[code]])
  report_section(paste("Natura 2000,", species_names[[code]]))
  conn_column <- paste0("LOG_", selected$column[selected$DRUH == species])

  #--------------------------------------------------#
  ## Matching -----
  #--------------------------------------------------#

  estimates <- list()
  balance <- list()
  sizes <- list()
  for (s in names(REVISED_SAMPLES)[names(REVISED_SAMPLES) %in% unlist(settings$samples)]) {
    sample <- REVISED_SAMPLES[[s]]
    d <- revised_model_data(data, species, sample)
    d$LOG_CONN <- d[[conn_column]]
    covariates <- intersect(unlist(settings$covariates), names(d))
    missing <- setdiff(unlist(settings$covariates), names(d))
    if (length(missing)) {
      report_warning("Matching covariates missing from the analysis table, left out:", paste(missing, collapse = ", "))
    }
    d <- d[stats::complete.cases(d[, c(covariates, "EVL_species", "OBSERVER", "DOY_STD")]), , drop = FALSE]
    # Standardised: the coordinates in metres (about 1e6) would leave the
    # outcome models badly conditioned. Neither the matching nor the
    # designation estimate depends on it.
    d[covariates] <- lapply(d[covariates], function(x) as.numeric(scale(x)))

    m <- match_designation(d, covariates, exact = unlist(settings$exact))
    balance[[s]] <- dplyr::mutate(balance_table(m), sample = sample, .before = 1)
    sizes[[s]] <- dplyr::mutate(matched_sizes(m), sample = sample, .before = 1)

    reference <- fit_tmb_set(list(
      list(
        id = paste0(code, "_evl_unadjusted", sample_suffix(sample)), label = "EVL only",
        formula = revised_formula(terms = "EVL_species"), data = d, sample = sample
      ),
      list(
        id = paste0(code, "_evl_adjusted", sample_suffix(sample)), label = "EVL + the matching covariates",
        formula = revised_formula(terms = c("EVL_species", covariates)), data = d, sample = sample
      )
    ))
    reference_coefs <- tmb_coef_table(reference) %>% dplyr::filter(term == "EVL_species")
    estimates[[s]] <- dplyr::bind_rows(
      reference_coefs %>%
        dplyr::transmute(
          sample,
          outcome_model = ifelse(
            grepl("_evl_unadjusted", model), "GLMM, unmatched", "GLMM, covariate-adjusted, unmatched"
          ),
          records = nrow(d), odds_ratio = ratio, or_low = ratio_low, or_high = ratio_high, p_value
        ),
      designation_estimates(m, covariates, season) %>%
        dplyr::transmute(sample = sample, outcome_model, records, odds_ratio, or_low, or_high, p_value)
    )
  }
  estimates <- dplyr::bind_rows(estimates)
  report_table(
    estimates %>% dplyr::mutate(odds_ratio_ci = format_ratio(odds_ratio, or_low, or_high), .after = records),
    sprintf(
      paste(
        "Odds ratio of recording the species inside an EVL designated for it",
        "(95%% CI): unmatched, adjusted for the matching covariates, and after full",
        "matching on the propensity score (exact on %s). Matching covariates: %s"
      ),
      paste(unlist(settings$exact), collapse = ", "), paste(unlist(settings$covariates), collapse = ", ")
    ),
    paste0(prefix, "_designation"),
    max_rows = nrow(estimates)
  )
  report_figure(
    plot_designation(estimates, species_names[[code]]),
    paste(
      "Odds ratio (95% CI) of recording the species inside an EVL designated for",
      "it: unmatched, adjusted for the matching covariates, and after full",
      "matching, in the three samples,", species_names[[code]]
    ),
    paste0(prefix, "_designation"),
    height = 6
  )
  balance <- dplyr::bind_rows(balance)
  report_table(
    balance,
    paste(
      "Covariate balance before and after matching: standardised mean",
      "difference (|SMD| below 0.1 is balanced) and variance ratio"
    ),
    paste0(prefix, "_balance"),
    max_rows = nrow(balance)
  )
  report_table(
    dplyr::bind_rows(sizes),
    paste(
      "Records inside (treated) and outside (control) an EVL designated for the",
      "species, all and effective after matching"
    ),
    paste0(prefix, "_matched_sizes")
  )

  #--------------------------------------------------#
  ## Persistence of the 2012-2018 populations -----
  #--------------------------------------------------#

  if (!"HIST_POSITIVE" %in% names(data)) {
    report_warning(
      "No HIST_POSITIVE in the analysis table (step 16 without the 2012-2018",
      "records); persistence skipped."
    )
    next
  }
  persistence <- list()
  counts <- list()
  for (s in names(REVISED_SAMPLES)[names(REVISED_SAMPLES) %in% unlist(settings$samples)]) {
    sample <- REVISED_SAMPLES[[s]]
    d <- revised_model_data(data, species, sample)
    d <- d[stats::complete.cases(d[, c("EVL_species", "HIST_POSITIVE", "OBSERVER", "DOY_STD")]), , drop = FALSE]
    known <- d[d$HIST_POSITIVE == 1, , drop = FALSE]
    counts[[s]] <- d %>%
      dplyr::group_by(known_2012_2018 = HIST_POSITIVE == 1, inside_evl = EVL_species == 1) %>%
      dplyr::summarise(
        visits = dplyr::n(), sites = dplyr::n_distinct(SITE_ID), positive = sum(POSITIVE == 1),
        share_positive = mean(POSITIVE == 1), .groups = "drop"
      ) %>%
      dplyr::mutate(sample = sample, .before = 1)
    fits <- fit_tmb_set(list(
      list(
        id = paste0(code, "_persistence", sample_suffix(sample)),
        label = "Visits to sites with the species in 2012-2018: EVL",
        formula = revised_formula(terms = "EVL_species"), data = known, sample = sample
      ),
      list(
        id = paste0(code, "_persistence_all", sample_suffix(sample)),
        label = "All visits: EVL x known earlier population",
        formula = revised_formula(terms = "EVL_species * HIST_POSITIVE"), data = d, sample = sample
      )
    ))
    persistence[[s]] <- fits
  }
  counts <- dplyr::bind_rows(counts)
  report_table(
    counts,
    paste(
      "Visits of 2019-2024 by whether the species was found at the site in",
      "2012-2018 (within the matching distance) and whether the site lies in an",
      "EVL designated for it"
    ),
    paste0(prefix, "_persistence_counts")
  )
  report_figure(
    plot_persistence(counts, species_names[[code]]),
    paste(
      "Share of the visits of 2019-2024 that found the species (Jeffreys 95%",
      "interval), at sites with and without a record of it in 2012-2018, inside",
      "and outside the EVLs designated for it,", species_names[[code]]
    ),
    paste0(prefix, "_persistence"),
    size = "single", height = 8
  )
  persistence_models <- unlist(persistence, recursive = FALSE)
  names(persistence_models) <- vapply(persistence_models, `[[`, character(1), "id")
  report_table(
    key_estimates(tmb_coef_table(persistence_models)),
    paste(
      "Persistence: the odds ratio of finding the species again at a site known",
      "from 2012-2018 inside an EVL designated for it against outside; and on all",
      "visits, the EVL association at sites without (EVL_species) and the",
      "difference at sites with a known earlier population (EVL_species:HIST_POSITIVE)"
    ),
    paste0(prefix, "_persistence_estimates")
  )
  report_tmb_set(persistence_models, paste0(prefix, "_persistence"))
}

report_finish()

step_end("24")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

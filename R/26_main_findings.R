#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#                 Step 26 - Main findings
#
#
#----------------------------------------------------------#
#
# The variables associated with the probability of recording each species,
# and what the analyses of the revision add to them, collected from the
# result tables of steps 04 and 17-25 (R/functions/findings.R):
#
#   variables  every variable of the multivariable models, of the
#              persistence and co-occurrence models and of the per-threat
#              models, each read from the model it is the subject of, in the
#              three samples: which are significant in the species' primary
#              sample (config.yml `synthesis`), in how many samples, and in
#              how many analyses their interval excludes 1;
#   findings   the detection, the imputed absences, the Natura 2000
#              association and persistence, the connectivity, the spatial
#              structure, the threats, the counts and the mowing timing, in
#              numbers, with the figures of their steps.
#
# Step 15 puts this report at the top of Outputs/REPORT.md. Nothing is
# refitted.
#
# Reads:  Outputs/<spec>/Tables/04_*, 17_*-25_* and Figures/17_*-25_*
#         (steps 04, 17-25)
# Writes: Outputs/<spec>/Tables/26_*.csv, Figures/26_*.png
#         Outputs/<spec>/Reports/26_main_findings.md
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("26")

alpha <- CONFIG$synthesis$alpha
species_all <- c(SPECIES_NAU, SPECIES_TEL)
species_label <- stats::setNames(species_short(species_all), species_all)
primary <- stats::setNames(vapply(species_all, primary_sample, character(1)), species_all)

#' "a, b and c".
and_list <- function(x) {
  if (length(x) < 2) {
    return(paste(x, collapse = ""))
  }
  paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
}

#' A result table of steps 04 and 17-25; step_begin() has checked it is
#' there.
read_table <- function(stem) .read_result_table(stem)

report_start(
  "26",
  "Main findings",
  paste(
    "The variables associated with the probability of recording each species, and what the",
    "analyses of the revision add to them, collected from the result tables of steps 17-25.",
    "Nothing is refitted."
  )
)

report_warning(
  "The models are naive occupancy models. An odds ratio is the association of a variable with the",
  "probability of recording the species at a visit, which mixes occupancy with detection; step 23",
  "bounds how far detection could move the management estimates. The variables were noted at the",
  "visit or describe the site and its surroundings, so they are associations, not effects."
)

report_note(paste(
  sprintf("A variable is significant when p < %g:", alpha),
  "the Wald p of the model it is read from, or for a threat class the p adjusted within its family",
  "(Holm for the threats with an expected direction, Benjamini-Hochberg for the others; step 19).",
  "Every model is fitted in three samples: with all imputed absences, with only the plausible ones",
  "and on the observed records only. Each species is read from one of them:",
  paste0(and_list(sprintf("*%s* from the sample \"%s\"", species_label, primary)), "."),
  "Odds ratios are against single host plants, against mowing timed inappropriately and no patches",
  "left uncut, against the field form of 2019-2020, and per 10 percentage points of a share or per",
  "tenfold connectivity or site area."
))

#--------------------------------------------------#
## The variables of each species -----
#--------------------------------------------------#

results <- list()
for (species in species_all) {
  findings <- collect_findings(species, alpha)
  results[[species]] <- list(
    findings = findings,
    summary = summarise_findings(
      findings, primary[[species]],
      robustness = read_table(sprintf("25_%s_robustness_summary", species_key(species))),
      fits = findings_fits(species)
    )
  )
}
records <- read_table("17_records")

#--------------------------------------------------#
## In brief -----
#--------------------------------------------------#

report_section("The main variables in brief")

for (species in species_all) {
  s <- results[[species]]$summary
  n <- records[records$species == species & records$sample == primary[[species]], ]
  words <- findings_in_words(s)
  sig <- s[s$significant & s$group != "survey design", ]
  some_samples <- sig[sig$samples_significant < sig$samples_estimable, ]
  some_analyses <- sig[sig$analyses_basis %in% "step 25" & sig$analyses_excluding_1 < sig$analyses, ]
  survey <- s[s$significant & s$group == "survey design", ]

  report_note(c(
    sprintf(
      "**%s** (sample \"%s\": %d in-season records, %d with the species) is recorded",
      species_label[[species]], primary[[species]], n$records, n$positive
    ),
    sprintf("- more often with %s;", if (length(words$more)) and_list(words$more) else "no variable"),
    sprintf("- less often with %s.", if (length(words$less)) and_list(words$less) else "no variable"),
    if (length(findings_interaction(s))) {
      paste0("- The interaction is significant: ", and_list(findings_interaction(s)), ".")
    },
    if (nrow(some_samples)) {
      sprintf(
        "- Not significant in every sample in which it is estimable: %s.",
        and_list(sprintf(
          "%s (%d of %d)", plain_label(some_samples$label), some_samples$samples_significant,
          some_samples$samples_estimable
        ))
      )
    },
    if (nrow(some_analyses)) {
      sprintf(
        "- With the interval excluding 1 in only some of the analyses of step 25: %s.",
        and_list(sprintf(
          "%s (%d of %d)", some_analyses$label, some_analyses$analyses_excluding_1, some_analyses$analyses
        ))
      )
    },
    if (nrow(survey)) {
      sprintf(
        "- Of the survey design, significant: %s.",
        and_list(label_with_ratio(survey$label, survey$odds_ratio))
      )
    }
  ))
}

#--------------------------------------------------#
## The variables, species by species -----
#--------------------------------------------------#

by_sample <- list()
for (species in species_all) {
  key <- species_key(species)
  prefix <- paste0("26_", key)
  findings <- results[[species]]$findings
  s <- results[[species]]$summary
  by_sample[[species]] <- dplyr::mutate(findings, species = species, .before = 1)

  report_section(paste("Variables associated with", species_label[[species]]))

  sig <- s[s$significant, , drop = FALSE]
  report_table(
    findings_table(sig),
    sprintf(
      paste(
        "Variables significant (p < %g) in the sample \"%s\" (the co-occurrence: observed records",
        "only): odds ratio (95%% CI) and p in that sample; the samples in which the variable is",
        "significant, of those in which it is estimable (a fit flagged for possible separation is not);",
        "and the analyses in which its interval excludes 1: those of step 25 for the management,",
        "host-plant and EVL terms, every model of steps 17 and 22 that includes it for the others. Only",
        "the threat classes are adjusted for multiplicity"
      ),
      alpha, primary[[species]]
    ),
    paste0(prefix, "_significant_variables"),
    max_rows = nrow(sig)
  )

  tested <- s[!s$significant & s$estimable & s$group != "threats noted at the visit", , drop = FALSE]
  threats <- s[s$group == "threats noted at the visit" & s$estimable, , drop = FALSE]
  separated <- s[!s$estimable, , drop = FALSE]
  report_note(paste(
    sprintf(
      "Tested and not significant in the sample \"%s\": %s.",
      primary[[species]], if (nrow(tested)) and_list(tested$label) else "none"
    ),
    sprintf(
      "Of the %d threat classes modelled, %d %s significant after the adjustment.",
      nrow(threats), sum(threats$significant), if (sum(threats$significant) == 1) "is" else "are"
    ),
    if (nrow(separated)) {
      sprintf(
        "Not estimable (no finite interval, a separated term, or a fit flagged for possible separation): %s.",
        and_list(separated$label)
      )
    } else {
      ""
    }
  ))

  shown <- findings_shown(findings)
  report_figure(
    plot_findings(findings, species_label[[species]]),
    sprintf(
      paste(
        "Odds ratios (95%% CI) of every variable, by group, in the three samples; filled marks are",
        "significant (p < %g; the threat classes after adjustment, shown only where significant in",
        "some sample). Fits flagged for possible separation by the diagnostics of their step, and fits",
        "without a finite interval or whose upper limit is %g or more times the lower one, are left",
        "out, %s"
      ),
      alpha, CONFIG$synthesis$max_interval_ratio, species_label[[species]]
    ),
    paste0(prefix, "_variables"),
    height = min(23, 3 + 0.5 * dplyr::n_distinct(shown$label) + 0.3 * dplyr::n_distinct(shown$group))
  )

  alone <- landscape_alone_table(
    read_table(sprintf("22_%s_landscape_best", key)), findings, primary[[species]]
  )
  if (!is.null(alone)) {
    report_table(
      alone,
      sprintf(
        paste(
          "Each landscape variable at the radius step 22 chose for it (sample \"%s\"): its odds ratio",
          "per 10 percentage points added alone to the management model, and the AIC it gains; and",
          "its odds ratio in the combined habitat and landscape model (sample \"%s\"), which it enters",
          "when it gains more than %g AIC. The land covers are correlated, so the combined estimates",
          "are adjusted for each other"
        ),
        REVISED_SAMPLES[["with_imputed"]], primary[[species]], CONFIG$landscape$min_aic_gain
      ),
      paste0(prefix, "_landscape"),
      max_rows = nrow(alone)
    )
  }

  report_existing_figure(
    paste(
      "Marginal predicted probability of recording the species (95% CI) by mowing timing and method,",
      "host-plant abundance and the EVL designated for it, in the three samples (step 17),",
      species_label[[species]]
    ),
    paste0("17_", key, "_predictions")
  )
  report_existing_figure(
    paste(
      "Odds ratios (95% interval) of the management, host-plant and EVL terms across the analyses of",
      "steps 17-24 (step 25),", species_label[[species]]
    ),
    paste0("25_", key, "_robustness")
  )
  report_existing_figure(
    paste(
      "Scale of effect of the landscape variables: delta AIC of the management model with each",
      "variable at each radius (step 22),", species_label[[species]]
    ),
    paste0("22_", key, "_landscape_scales"),
    optional = TRUE
  )

  report_csv(
    findings_table(s), "Every variable, significant or not, in the sample the species is read from",
    paste0(prefix, "_variables")
  )
}

by_sample <- dplyr::bind_rows(by_sample)
report_csv(by_sample, "Every variable of both species in the three samples", "26_variables_by_sample")
validate_table(
  by_sample, "findings",
  key = c("species", "term", "sample"),
  not_na = c("species", "term", "label", "group", "sample")
)

#--------------------------------------------------#
## What the analyses of the revision add -----
#--------------------------------------------------#

report_section("What the analyses of the revision add")

# Detection (step 23).
report_section("Detection", level = 3)
detection <- vapply(species_all, function(species) {
  d <- read_table(sprintf("23_%s_detection_probability", species_key(species)))
  describe <- function(model) {
    r <- d[d$model == model, , drop = FALSE]
    if (!nrow(r)) {
      return("not estimated")
    }
    and_list(sprintf(
      "%.2f (%.2f-%.2f) with the %s", r$mean, r$q2.5, r$q97.5, sub("^p at the median day, ", "", r$quantity)
    ))
  }
  sprintf(
    "*%s*: %s in the sample \"%s\"; %s on the observed records only.",
    species_label[[species]], describe(paste0(primary[[species]], ", prior on p")), primary[[species]],
    describe(paste0(REVISED_SAMPLES[["observed"]], ", prior on p"))
  )
}, character(1))
report_note(sprintf(
  paste(
    "The detection probability at the year's median day, identified by the site-years visited more",
    "than once (posterior mean and 95%% credible interval; the prior mean is %g):"
  ),
  CONFIG$detection$prior_p
))
report_bullets(detection)
report_note(paste(
  "The management estimates with the detection fixed at a known probability, and on the occupancy",
  "scale of the mixed-design model, are among the analyses of the robustness figures above."
))
for (species in species_all) {
  report_existing_figure(
    paste(
      "Odds ratios (95% interval) of the key terms as detection is allowed for: the GLMM of step 17,",
      "GLMs with the detection fixed at p, and the occupancy part of the mixed-design model (step 23),",
      species_label[[species]]
    ),
    paste0("23_", species_key(species), "_known_detection")
  )
}

# The imputed absences (step 04, P4.3).
report_section("The imputed absences", level = 3)
imputed <- read_table("04_imputed_plausible")
report_bullets(vapply(species_all, function(species) {
  r <- imputed[imputed$DRUH == species, , drop = FALSE]
  sprintf(
    paste(
      "*%s*: %d of the %d imputed absences in the flight season (%.0f %%) are plausible, within %g km",
      "of a record of the species of 2012-2024; the median distance to the nearest such record is %s."
    ),
    species_label[[species]], sum(r$plausible), sum(r$in_flight_season),
    100 * sum(r$plausible) / sum(r$in_flight_season), CONFIG$imputation$plausible_radius_km,
    and_list(sprintf("%.1f km in %s", r$median_km_to_positive, r$PROTOCOL_ERA))
  )
}, character(1)))

# Natura 2000 and persistence (step 24).
report_section("Natura 2000 and persistence", level = 3)
for (species in species_all) {
  key <- species_key(species)
  code <- species_code(species)
  suffix <- sample_suffix(primary[[species]])
  designation <- read_table(sprintf("24_%s_designation", key))
  designation <- designation[designation$sample == primary[[species]], , drop = FALSE]
  persistence <- read_table(sprintf("24_%s_persistence_estimates", key))
  known_sites <- persistence[persistence$model == paste0(code, "_persistence", suffix), , drop = FALSE]
  interaction <- persistence[persistence$model == paste0(code, "_persistence_all", suffix), , drop = FALSE]
  interaction <- interaction[interaction$term == "EVL_species:HIST_POSITIVE", , drop = FALSE]
  evl <- results[[species]]$summary
  evl <- evl[evl$term == "EVL_species", , drop = FALSE]
  report_note(paste(
    sprintf(
      "*%s* (sample \"%s\"). Odds ratio of recording the species inside an EVL designated for it: %s.",
      species_label[[species]], primary[[species]],
      paste(
        sprintf(
          "%s %s", designation$outcome_model,
          format_ratio_ci(designation$odds_ratio, designation$or_low, designation$or_high)
        ),
        collapse = "; "
      )
    ),
    if (nrow(evl) && !is.na(evl$analyses)) {
      sprintf(
        "Its interval excludes 1 in %d of the %d analyses of step 25.", evl$analyses_excluding_1, evl$analyses
      )
    },
    if (nrow(known_sites)) {
      sprintf(
        paste(
          "At the sites where it was recorded in 2012-2018, the odds of finding it again inside against",
          "outside such an EVL: %s; the EVL association at those sites against the others (interaction): %s."
        ),
        known_sites$ratio_ci[1], if (nrow(interaction)) interaction$ratio_ci[1] else "not estimated"
      )
    }
  ))
  report_existing_figure(
    paste(
      "Odds ratio (95% CI) of recording the species inside an EVL designated for it: unmatched,",
      "adjusted for the matching covariates and after matching, in the three samples (step 24),",
      species_label[[species]]
    ),
    paste0("24_", key, "_designation")
  )
  report_existing_figure(
    paste(
      "Share of the visits of 2019-2024 that found the species, at sites with and without a record",
      "of it in 2012-2018, inside and outside the EVLs designated for it (step 24),", species_label[[species]]
    ),
    paste0("24_", key, "_persistence")
  )
}

# Connectivity (step 17, P4.2).
report_section("Connectivity", level = 3)
selected <- read_table("17_connectivity_selected")
report_bullets(vapply(species_all, function(species) {
  r <- selected[selected$DRUH == species, , drop = FALSE]
  conn <- results[[species]]$summary
  conn <- conn[conn$term == "LOG_CONN", , drop = FALSE]
  sprintf(
    paste(
      "*%s*: alpha = %g per km (mean dispersal distance %g m) and b = %g by profile likelihood;",
      "odds ratio %s per tenfold connectivity (sample \"%s\"). The fixed pair of config.yml fits",
      "%.1f AIC worse, the Phase 3 pair (alpha = 1, b = 0.5) %.1f."
    ),
    species_label[[species]], r$alpha_per_km, r$mean_dispersal_m, r$area_exponent,
    format_ratio_ci(conn$odds_ratio, conn$or_low, conn$or_high), conn$sample,
    r$delta_AIC_of_fixed_pair, r$delta_AIC_of_phase3_pair
  )
}, character(1)))

# Space (step 18, P3.6, P4.12).
report_section("Spatial structure", level = 3)
moran <- read_table("18_moran")
spde <- read_table("18_spde")
plus <- read_table("18_spatial_plus")
report_bullets(c(
  sprintf(
    "Moran's I of the residuals, aggregated per persistent site, is significant (p < 0.05) in %d of %d models.",
    sum(moran$p_value < 0.05, na.rm = TRUE), nrow(moran)
  ),
  sprintf(
    paste(
      "An SPDE spatial field in place of the grid square changes the AIC of the management model by",
      "%.1f to %.1f, with a range of %.0f-%.0f km; %d of %d fits meet the convergence criteria."
    ),
    min(spde$delta_AIC_spde_vs_grid), max(spde$delta_AIC_spde_vs_grid), min(spde$range_km), max(spde$range_km),
    sum(spde$converged), nrow(spde)
  ),
  sprintf(
    "spatial+ moves %d of %d estimates of the management model by more than %g of their standard errors.",
    sum(plus$flag), nrow(plus), CONFIG$revised_models$autocorrelation$spatial_plus_flag_se
  )
))

# Threats (step 19, P4.11).
report_section("Threats", level = 3)
stacked <- read_table("19_threat_stacked")
report_bullets(vapply(species_all, function(species) {
  s <- results[[species]]$summary
  threats <- s[s$group == "threats noted at the visit" & s$estimable, , drop = FALSE]
  passes <- threats[threats$significant, , drop = FALSE]
  st <- stacked[stacked$species == species & stacked$sample == primary[[species]], , drop = FALSE]
  st <- st[!is.na(st$p_adjusted) & st$p_adjusted < alpha, , drop = FALSE]
  sprintf(
    paste(
      "*%s* (sample \"%s\"), significant after the adjustment: %s. In the stacked model, with the",
      "threat as the response, the odds of a threat being noted at a visit that found the species",
      "against one that did not, significant after the adjustment: %s."
    ),
    species_label[[species]], primary[[species]],
    if (nrow(passes)) {
      and_list(sprintf(
        "%s, odds ratio %s, p %s", passes$label,
        format_ratio_ci(passes$odds_ratio, passes$or_low, passes$or_high), format_p(passes$p_value, passes$p_basis)
      ))
    } else {
      sprintf("none of %d classes", nrow(threats))
    },
    if (nrow(st)) {
      and_list(sprintf("%s %s", threat_label(st$threat), st$odds_ratio))
    } else {
      "none"
    }
  )
}, character(1)))
report_existing_figure(
  paste(
    "Odds ratios (95% CI) of recording the species at a visit where each threat class is noted, in the",
    "three samples; filled marks pass the adjustment (step 19)"
  ),
  "19_threat_estimates"
)

# Counts (step 20, P4.10).
report_section("Counts", level = 3)
report_bullets(vapply(species_all, function(species) {
  key <- species_key(species)
  counts <- read_table(sprintf("20_%s_key_estimates", key))
  counts <- counts[counts$model == paste0(species_code(species), "_abundance"), , drop = FALSE]
  sig <- counts[counts$p_value < alpha, , drop = FALSE]
  sprintf(
    "*%s*: rate ratios of the count at the visits that found the species, significant: %s.",
    species_label[[species]],
    if (nrow(sig)) {
      and_list(sprintf(
        "%s %s", ifelse(sig$term %in% names(SYNTHESIS_TERMS), SYNTHESIS_TERMS[sig$term], sig$term), sig$ratio_ci
      ))
    } else {
      "none"
    }
  )
}, character(1)))

# The recorded mowing timing (step 22, P4.5).
report_section("The recorded mowing timing", level = 3)
timing <- read_table("22_timing_check")
report_note(sprintf(
  paste(
    "Observed records whose note says the site was freshly mown at a visit inside the no-cut window",
    "(%s to %s), by the timing recorded: %s. The field form does not record the dates of the cuts."
  ),
  CONFIG$management$no_cut_window[[1]], CONFIG$management$no_cut_window[[2]],
  if (nrow(timing)) {
    paste(
      vapply(split(timing, timing$DRUH), function(r) {
        sprintf("*%s* %s", species_short(r$DRUH[1]), and_list(sprintf("%d %s", r$records, r$timing)))
      }, character(1)),
      collapse = "; "
    )
  } else {
    "none"
  }
))

report_finish()

step_end("26")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

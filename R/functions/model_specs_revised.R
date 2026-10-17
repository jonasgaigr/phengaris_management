#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Model sets: the revised specification
#
#----------------------------------------------------------#
#
# The revised models approved in the Phase 3 review (REVIEW.md, P3.1-P3.8).
# Every model:
#
#   * is a naive occupancy model: one visit per site and year, so detection
#     is not separated from occupancy (item 11);
#   * has random intercepts for the mapping grid square, the year and the
#     observer, config.yml `revised_models: random_effects` (P3.1, P3.3);
#   * has the detection covariates: the field-form era and the position of
#     the visit in the flight season of its species and year, as the log
#     relative activity of the flight curve or as a quadratic in DOY_DEV
#     (config.yml `revised_models: season_term`; P3.3, P4.1, item 20);
#   * is fitted on the records inside the flight season only;
#   * is fitted three times: with the imputed absences, with only the
#     plausible ones (P4.3) and on the observed records only (P3.2).
#
# Each builder returns specs, the arguments of fit_tmb(); the steps fit them
# with fit_tmb_set().
#
#----------------------------------------------------------#

#--------------------------------------------------#
## Data -----
#--------------------------------------------------#

#' The samples every revised model is fitted on: with the imputed absences,
#' with only the plausible ones (P4.3), and on the observed records only.
REVISED_SAMPLES <- c(with_imputed = "with imputed", plausible = "with plausible imputed", observed = "observed only")

#' The suffix of the model ids of a sample.
sample_suffix <- function(sample) {
  c("with imputed" = "", "with plausible imputed" = "_pls", "observed only" = "_obs")[[sample]]
}

#' Short code of a species, for model ids.
species_code <- function(species) {
  c("nau", "tel")[match(species, c(SPECIES_NAU, SPECIES_TEL))]
}

#' The records a revised model is fitted on, with the model variables.
#'
#' @param data     the analysis table of a specification with design
#'                 variables
#' @param species  the species, or NULL for both
#' @param sample   "with imputed", "with plausible imputed" (the imputed
#'                 absences near a known population, P4.3) or "observed only"
#' @param window   NULL for the records in the flight season
#'                 (IN_FLIGHT_WINDOW), or a number of days: the records
#'                 within that many days of the year's median instead
#'                 (IN_WINDOW_<days>), for the sensitivity analysis (P4.1)
#' @return the records of the sample in the season, with
#'         DOY_STD   DOY_DEV in units of config.yml `revised_models:
#'                   doy_unit_days`
#'         ERA       PROTOCOL_ERA as a factor, the older form first
#'         PLANT     host-plant abundance as a factor: single, abundant,
#'                   dominant
#'         LOG_AREA  log10 of the site area in ha (polygon sites)
#'         LOG_CONN and LOG_CONN_A<alpha>_B<b>  log10(connectivity + offset)
#'                   at the fixed pair and at every pair of the profile
revised_model_data <- function(data, species = NULL, sample = REVISED_SAMPLES[["with_imputed"]],
                               unit = CONFIG$revised_models$doy_unit_days, window = NULL) {
  sample <- match.arg(sample, unname(REVISED_SAMPLES))
  in_season <- if (is.null(window)) {
    data$IN_FLIGHT_WINDOW
  } else {
    !is.na(data$DOY_DEV) & abs(data$DOY_DEV) <= window
  }
  out <- data[in_season, , drop = FALSE]
  if (!is.null(species)) out <- dplyr::filter(out, DRUH == species)
  if (sample == REVISED_SAMPLES[["observed"]]) out <- dplyr::filter(out, !IMPUTED)
  if (sample == REVISED_SAMPLES[["plausible"]]) out <- dplyr::filter(out, !IMPUTED | IMPUTED_PLAUSIBLE)

  conn_columns <- grep("^CONN($|_A)", names(out), value = TRUE)
  out <- out %>%
    dplyr::mutate(
      DOY_STD = DOY_DEV / unit,
      ERA = factor(PROTOCOL_ERA, levels = sort(unique(data$PROTOCOL_ERA))),
      PLANT = factor(PLANT_QUANT, levels = 1:3, labels = c("single", "abundant", "dominant")),
      LOG_AREA = log10(SITE_AREA_HA)
    )
  for (col in conn_columns) {
    out[[paste0("LOG_", col)]] <- log10(out[[col]] + CONFIG$connectivity$log_offset)
  }
  out
}

#--------------------------------------------------#
## Formulas -----
#--------------------------------------------------#

#' The random-intercept part of every revised model.
revised_random_effects <- function(extra = NULL) {
  groups <- c(unlist(CONFIG$revised_models$random_effects), extra)
  paste(sprintf("(1 | %s)", groups), collapse = " + ")
}

#' The fixed terms placing a visit in the flight season.
#'
#' @param season  "activity": the log relative activity of the flight curve
#'                (P4.1); "quadratic": DOY_DEV and its square (P3.3)
season_terms <- function(season = CONFIG$revised_models$season_term) {
  switch(match.arg(season, c("activity", "quadratic")),
    activity = "LOG_ACTIVITY",
    quadratic = c("DOY_STD", "I(DOY_STD^2)")
  )
}

#' The season term not chosen in config.yml, for the comparison of the two.
other_season_term <- function(season = CONFIG$revised_models$season_term) {
  setdiff(c("activity", "quadratic"), season)
}

#' A revised model formula from its fixed terms.
#'
#' @param response  the response
#' @param terms     fixed terms beyond the detection covariates
#' @param era       include the field-form era (leave it out for data of one
#'                  era)
#' @param extra_re  additional random-intercept grouping factors
#' @param season    the season term, season_terms()
revised_formula <- function(response = "POSITIVE", terms = character(), era = TRUE, extra_re = NULL,
                            season = CONFIG$revised_models$season_term) {
  detection <- c(if (era) "ERA", season_terms(season))
  stats::as.formula(paste(
    response, "~",
    paste(c(terms, detection), collapse = " + "),
    "+", revised_random_effects(extra_re)
  ))
}

#' The management terms of the multivariable model (P3.4).
MANAGEMENT_TERMS <- c("TIMING * METHOD", "PLANT", "EVL_species")

#--------------------------------------------------#
## Occupancy models of one species (step 17) -----
#--------------------------------------------------#

#' The revised occupancy models of one species, for one sample.
#'
#'   baseline           the detection covariates and random effects only, on
#'                      every in-season record
#'   baseline_season    the same with the other season term (quadratic
#'                      against flight curve, P4.1)
#'   management_null    the same, on the records complete on the management
#'                      variables (the reference of the management model)
#'   management         mowing timing x method, host-plant abundance and
#'                      protection designated for the species (P3.4)
#'   management_conn    + connectivity (P3.5) as LOG_CONN, which step 17
#'                      sets to the alpha and b the profile likelihood picks
#'                      (P4.2; connectivity_profile_specs())
#'   management_site    + a persistent-site random intercept, at the
#'                      configured matching distance and the sensitivity
#'                      distances (P3.1)
#'   management_2021    2021-2024 polygon sites only, + grazing and site
#'                      area, which the earlier form did not record (P3.4,
#'                      item 20)
#'
#' @param data     revised_model_data() of the species and sample
#' @param species  the species
#' @param sample   the sample, for the ids and labels
revised_occupancy_specs <- function(data, species, sample) {
  code <- species_code(species)
  suffix <- sample_suffix(sample)
  id <- function(x) paste0(code, "_", x, suffix)
  spec <- function(name, label, group, formula, d) {
    list(id = id(name), label = label, group = group, formula = formula, data = d, sample = sample)
  }

  mgmt_vars <- c("TIMING", "METHOD", "PLANT", "EVL_species")
  d_mgmt <- data[stats::complete.cases(data[, mgmt_vars]), , drop = FALSE]
  d_2021 <- d_mgmt %>%
    dplyr::filter(YEAR >= CONFIG$design$protocol_change_year, !is.na(GRAZE), !is.na(LOG_AREA))

  site_distances <- unlist(CONFIG$sites$sensitivity_distances_m)

  c(
    list(
      spec(
        "baseline", "Detection covariates and random effects, every in-season record", "Baseline",
        revised_formula(), data
      ),
      spec(
        "baseline_season", sprintf("Baseline with the %s season term", other_season_term()), "Baseline",
        revised_formula(season = other_season_term()), data
      ),
      spec(
        "management_null", "Detection covariates only, records with the management recorded", "Management",
        revised_formula(), d_mgmt
      ),
      spec(
        "management", "Mowing timing x method, host plant, protection for the species", "Management",
        revised_formula(terms = MANAGEMENT_TERMS), d_mgmt
      ),
      spec(
        "management_conn", "Management model + connectivity", "Connectivity",
        revised_formula(terms = c(MANAGEMENT_TERMS, "LOG_CONN")), d_mgmt
      ),
      spec(
        "management_site", sprintf("Management model + persistent site (%g m)", CONFIG$sites$match_distance_m),
        "Persistent site",
        revised_formula(terms = MANAGEMENT_TERMS, extra_re = "SITE_ID"), d_mgmt
      )
    ),
    lapply(site_distances, function(dist) {
      spec(
        paste0("management_site", dist), sprintf("Management model + persistent site (%g m)", dist),
        "Persistent site",
        revised_formula(terms = MANAGEMENT_TERMS, extra_re = site_id_column(dist)), d_mgmt
      )
    }),
    list(
      spec(
        "management_2021", "2021-2024 polygon sites: management + grazing + site area", "2021-2024",
        revised_formula(terms = c(MANAGEMENT_TERMS, "GRAZE", "LOG_AREA"), era = FALSE), d_2021
      )
    )
  )
}

#' The connectivity models of the profile likelihood (P4.2).
#'
#' The management model with the connectivity at each alpha x b pair of
#' connectivity_grid(), on the records with the management recorded.
#'
#' @param data     revised_model_data() of the species and sample
#' @param species  the species
#' @param sample   the sample, for the ids and labels
#' @return specs for fit_tmb_set(), each with `alpha` and `b`
connectivity_profile_specs <- function(data, species, sample) {
  code <- species_code(species)
  suffix <- sample_suffix(sample)
  mgmt_vars <- c("TIMING", "METHOD", "PLANT", "EVL_species")
  d_mgmt <- data[stats::complete.cases(data[, mgmt_vars]), , drop = FALSE]
  pairs <- connectivity_grid()
  lapply(seq_len(nrow(pairs)), function(i) {
    a <- pairs$alpha[i]
    b <- pairs$b[i]
    list(
      id = sprintf("%s_conn_a%g_b%g%s", code, a, b, suffix),
      label = sprintf("Management model + connectivity, alpha = %g per km, b = %g", a, b),
      group = "Connectivity profile",
      formula = revised_formula(terms = c(MANAGEMENT_TERMS, paste0("LOG_", connectivity_column(a, b)))),
      data = d_mgmt, sample = sample, alpha = a, b = b
    )
  })
}

#' The profile likelihood of alpha and b from the fitted profile models.
#'
#' @param models  the fitted connectivity_profile_specs()
#' @param specs   the specs they were fitted from (for alpha and b)
#' @return one row per pair: alpha, b, the log-likelihood and AIC, the
#'         difference to the best pair, the odds ratio of connectivity, and
#'         `best`; NULL rows for failed fits are left out
connectivity_profile <- function(models, specs) {
  rows <- lapply(seq_along(specs), function(i) {
    m <- models[[specs[[i]]$id]]
    if (is.null(m$fit)) {
      return(NULL)
    }
    co <- summary(m$fit)$coefficients$cond
    term <- paste0("LOG_", connectivity_column(specs[[i]]$alpha, specs[[i]]$b))
    data.frame(
      model = m$id, sample = m$sample, alpha_per_km = specs[[i]]$alpha, area_exponent = specs[[i]]$b,
      mean_dispersal_m = 1000 / specs[[i]]$alpha,
      n_obs = m$n_input, logLik = as.numeric(stats::logLik(m$fit)), AIC = stats::AIC(m$fit),
      conn_odds_ratio = exp(co[term, 1]),
      conn_or_low = exp(co[term, 1] - stats::qnorm(0.975) * co[term, 2]),
      conn_or_high = exp(co[term, 1] + stats::qnorm(0.975) * co[term, 2]),
      converged = tmb_converged(m),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) {
    return(data.frame())
  }
  stopifnot(length(unique(out$n_obs)) == 1)
  out$delta_AIC <- out$AIC - min(out$AIC)
  out$best <- out$delta_AIC == 0
  out
}

#' The AIC comparisons of one species and sample: ids, caption, stem suffix.
revised_occupancy_comparisons <- function(species, sample) {
  code <- species_code(species)
  suffix <- sample_suffix(sample)
  id <- function(x) paste0(code, "_", x, suffix)
  dists <- unlist(CONFIG$sites$sensitivity_distances_m)
  list(
    list(
      ids = id(c("baseline", "baseline_season")),
      caption = sprintf(
        "Season term: %s (baseline) against %s (baseline_season)",
        CONFIG$revised_models$season_term, other_season_term()
      ),
      stem = "season"
    ),
    list(
      ids = id(c("management_null", "management", "management_conn")),
      caption = "Management and connectivity",
      stem = "management"
    ),
    list(
      ids = id(c("management", "management_site", paste0("management_site", dists))),
      caption = "Persistent-site random effect at three matching distances",
      stem = "site"
    )
  )
}

#--------------------------------------------------#
## Co-occurrence (step 17) -----
#--------------------------------------------------#

#' Records of one species at visits where the other species was recorded too.
#'
#' Only observed records count, on both sides: an imputed absence exists
#' because the other species was recorded and this one was not, so it cannot
#' serve to test whether the two occur together (REVIEW.md, item 26).
#'
#' @param data     revised_model_data(), both species, observed only
#' @param species  the species whose occupancy is modelled
#' @return the species' records with OTHER_POSITIVE, the other species'
#'         outcome at the same visit
cooccurrence_data <- function(data, species) {
  other <- data %>%
    dplyr::filter(DRUH != species, !IMPUTED) %>%
    dplyr::select(ID_LOKAL, DATUM_OD, OTHER_POSITIVE = POSITIVE)
  data %>%
    dplyr::filter(DRUH == species, !IMPUTED) %>%
    dplyr::inner_join(other, by = c("ID_LOKAL", "DATUM_OD"))
}

#--------------------------------------------------#
## Threat models (step 19) -----
#--------------------------------------------------#

#' Per-threat models of one species and sample (P3.7).
#'
#' One model per threat class: occupancy on the class, with the detection
#' covariates and the random effects, on the records with a threat list. A
#' class with fewer than config.yml `revised_models: threats:
#' min_records_per_outcome` positive or negative records among the records
#' listing it is not modelled.
#'
#' @return list(specs, too_rare): the specs and a table of the classes left
#'         out
revised_threat_specs <- function(data, species, sample, classes = names(CONFIG$strukt_pozn$threat_items)) {
  code <- species_code(species)
  suffix <- sample_suffix(sample)
  d <- data[!is.na(data$SUM_THREATS), , drop = FALSE]
  minimum <- CONFIG$revised_models$threats$min_records_per_outcome

  counts <- dplyr::bind_rows(lapply(classes, function(k) {
    tibble::tibble(
      class = k,
      with_threat_positive = sum(d[[k]] == 1 & d$POSITIVE == 1),
      with_threat_negative = sum(d[[k]] == 1 & d$POSITIVE == 0)
    )
  }))
  usable <- counts$class[counts$with_threat_positive >= minimum & counts$with_threat_negative >= minimum]

  specs <- lapply(usable, function(k) {
    list(
      id = paste0(code, "_threat_", k, suffix), label = k, group = "Threat",
      formula = revised_formula(terms = k), data = d, sample = sample
    )
  })
  list(
    specs = specs,
    counts = counts %>%
      dplyr::mutate(sample = sample, modelled = class %in% usable, .after = class)
  )
}

#--------------------------------------------------#
## Abundance models (step 20) -----
#--------------------------------------------------#

#' Zero-truncated negative-binomial models of the counts (P3.8).
#'
#' The count POCET is recorded on positive records only (at least one adult),
#' so the count given presence is modelled with a zero-truncated negative
#' binomial: management model, and for 2021-2024 polygon sites, + grazing
#' and site area. The imputed absences never carry a count.
#'
#' @param data     revised_model_data() of the species, observed only
#' @param species  the species
revised_abundance_specs <- function(data, species) {
  code <- species_code(species)
  d <- data %>%
    dplyr::filter(POSITIVE == 1, !is.na(POCET), POCET >= 1)
  mgmt_vars <- c("TIMING", "METHOD", "PLANT", "EVL_species")
  d_mgmt <- d[stats::complete.cases(d[, mgmt_vars]), , drop = FALSE]
  d_2021 <- d_mgmt %>%
    dplyr::filter(YEAR >= CONFIG$design$protocol_change_year, !is.na(GRAZE), !is.na(LOG_AREA))
  family <- glmmTMB::truncated_nbinom2()
  sample <- REVISED_SAMPLES[["observed"]]
  list(
    list(
      id = paste0(code, "_abundance_null"), label = "Detection covariates only", group = "Abundance",
      formula = revised_formula("POCET"), data = d_mgmt, family = family, sample = sample
    ),
    list(
      id = paste0(code, "_abundance"), label = "Management model", group = "Abundance",
      formula = revised_formula("POCET", MANAGEMENT_TERMS), data = d_mgmt, family = family, sample = sample
    ),
    list(
      id = paste0(code, "_abundance_2021"), label = "2021-2024 polygon sites: + grazing + site area",
      group = "Abundance",
      formula = revised_formula("POCET", c(MANAGEMENT_TERMS, "GRAZE", "LOG_AREA"), era = FALSE),
      data = d_2021, family = family, sample = sample
    )
  )
}

#----------------------------------------------------------#
# End model sets -----
#----------------------------------------------------------#

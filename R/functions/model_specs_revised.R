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
#     the visit in the flight season of its species and year, as a quadratic
#     (P3.3, item 20);
#   * is fitted on the records inside the flight season only;
#   * is fitted twice, with the imputed absences and on the observed records
#     only (P3.2).
#
# Each builder returns specs, the arguments of fit_tmb(); the steps fit them
# with fit_tmb_set().
#
#----------------------------------------------------------#

#--------------------------------------------------#
## Data -----
#--------------------------------------------------#

#' The two samples every revised model is fitted on.
REVISED_SAMPLES <- c(with_imputed = "with imputed", observed = "observed only")

#' Short code of a species, for model ids.
species_code <- function(species) {
  c("nau", "tel")[match(species, c(SPECIES_NAU, SPECIES_TEL))]
}

#' The records a revised model is fitted on, with the model variables.
#'
#' @param data     the analysis table of a specification with design
#'                 variables
#' @param species  the species, or NULL for both
#' @param sample   "with imputed" or "observed only"
#' @return the in-season records of the sample, with
#'         DOY_STD   DOY_DEV in units of config.yml `revised_models:
#'                   doy_unit_days`
#'         ERA       PROTOCOL_ERA as a factor, the older form first
#'         PLANT     host-plant abundance as a factor: single, abundant,
#'                   dominant
#'         LOG_AREA  log10 of the site area in ha (polygon sites)
#'         LOG_CONN and LOG_CONN_<alpha>  log10(connectivity + offset)
revised_model_data <- function(data, species = NULL, sample = REVISED_SAMPLES[["with_imputed"]],
                               unit = CONFIG$revised_models$doy_unit_days) {
  sample <- match.arg(sample, unname(REVISED_SAMPLES))
  out <- data %>%
    dplyr::filter(IN_FLIGHT_WINDOW)
  if (!is.null(species)) out <- dplyr::filter(out, DRUH == species)
  if (sample == REVISED_SAMPLES[["observed"]]) out <- dplyr::filter(out, !IMPUTED)

  conn_columns <- grep("^CONN($|_[0-9.]+$)", names(out), value = TRUE)
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

#' A revised model formula from its fixed terms.
#'
#' @param response  the response
#' @param terms     fixed terms beyond the detection covariates
#' @param era       include the field-form era (leave it out for data of one
#'                  era)
#' @param extra_re  additional random-intercept grouping factors
revised_formula <- function(response = "POSITIVE", terms = character(), era = TRUE, extra_re = NULL) {
  detection <- c(if (era) "ERA", "DOY_STD", "I(DOY_STD^2)")
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
#'   management_null    the same, on the records complete on the management
#'                      variables (the reference of the management model)
#'   management         mowing timing x method, host-plant abundance and
#'                      protection designated for the species (P3.4)
#'   management_conn    + connectivity (P3.5), at the configured alpha and
#'                      at the sensitivity values
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
  suffix <- if (sample == REVISED_SAMPLES[["observed"]]) "_obs" else ""
  id <- function(x) paste0(code, "_", x, suffix)
  spec <- function(name, label, group, formula, d) {
    list(id = id(name), label = label, group = group, formula = formula, data = d, sample = sample)
  }

  mgmt_vars <- c("TIMING", "METHOD", "PLANT", "EVL_species")
  d_mgmt <- data[stats::complete.cases(data[, mgmt_vars]), , drop = FALSE]
  d_2021 <- d_mgmt %>%
    dplyr::filter(YEAR >= CONFIG$design$protocol_change_year, !is.na(GRAZE), !is.na(LOG_AREA))

  alphas <- unlist(CONFIG$connectivity$alpha_sensitivity_per_km)
  site_distances <- unlist(CONFIG$sites$sensitivity_distances_m)

  c(
    list(
      spec(
        "baseline", "Detection covariates and random effects, every in-season record", "Baseline",
        revised_formula(), data
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
      )
    ),
    lapply(alphas, function(a) {
      spec(
        paste0("management_conn_a", a), sprintf("Management model + connectivity, alpha = %g per km", a),
        "Connectivity",
        revised_formula(terms = c(MANAGEMENT_TERMS, paste0("LOG_", connectivity_column(a)))), d_mgmt
      )
    }),
    list(
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

#' The AIC comparisons of one species and sample: ids, caption, stem suffix.
revised_occupancy_comparisons <- function(species, sample) {
  code <- species_code(species)
  suffix <- if (sample == REVISED_SAMPLES[["observed"]]) "_obs" else ""
  id <- function(x) paste0(code, "_", x, suffix)
  alphas <- unlist(CONFIG$connectivity$alpha_sensitivity_per_km)
  dists <- unlist(CONFIG$sites$sensitivity_distances_m)
  list(
    list(
      ids = id(c("management_null", "management", "management_conn", paste0("management_conn_a", alphas))),
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
  suffix <- if (sample == REVISED_SAMPLES[["observed"]]) "_obs" else ""
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

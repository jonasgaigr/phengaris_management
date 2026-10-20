#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: the main findings
#
#----------------------------------------------------------#
#
# Step 26 lists, for each species, the variables associated with the
# probability of recording it. Each variable is read from the model it is
# the subject of (FINDINGS_TERMS), in the three samples:
#
#   - the management terms, the host plant, the EVL and the survey design
#     from the management model of step 17;
#   - the connectivity from the same model with connectivity, the grazing
#     and the site area from the 2021-2024 polygon model (step 17);
#   - the habitat of the site and the landscape from the combined habitat
#     and landscape model of step 22, the edge zone from its 2021-2024
#     polygon model;
#   - a population known in 2012-2018 from the persistence model (step 24),
#     the other species from the co-occurrence model (step 17);
#   - the threat classes from their per-threat models (step 19).
#
# A variable is estimable in a sample when its interval is an estimate
# (is_estimable()) and its fit is not flagged for possible separation by the
# diagnostics of its step; it is significant when, besides, its p is below
# config.yml `synthesis: alpha`: the Wald p of the GLMM, or for a threat
# class the p adjusted within its family (Holm or Benjamini-Hochberg). The
# tables are read as the steps wrote them; nothing is refitted.
#
#----------------------------------------------------------#

#--------------------------------------------------#
## The variables -----
#--------------------------------------------------#

#' The models the variables are read from: the result table (%s is the
#' species key, "nausithous" or "teleius"), the model id without the species
#' code and the sample suffix, and a label.
FINDINGS_SOURCES <- list(
  management = list(table = "17_%s_coefficients", model = "_management", label = "management model (step 17)"),
  connectivity = list(table = "17_%s_coefficients", model = "_management_conn", label = "+ connectivity (step 17)"),
  polygons = list(table = "17_%s_coefficients", model = "_management_2021", label = "2021-2024 polygons (step 17)"),
  habitat = list(
    table = "22_%s_coefficients", model = "_habitat_landscape", label = "habitat and landscape (step 22)"
  ),
  edge = list(table = "22_%s_coefficients", model = "_local_2021", label = "habitat, 2021-2024 polygons (step 22)"),
  persistence = list(
    table = "24_%s_persistence_coefficients", model = "_persistence_all", label = "persistence (step 24)"
  ),
  cooccurrence = list(
    table = "17_cooccurrence_coefficients", model = "_cooccurrence", label = "co-occurrence (step 17)"
  )
)

#' The groups of the variables, in the order they are listed.
FINDINGS_GROUPS <- c(
  "management", "host plant and habitat", "site and protection", "landscape", "populations",
  "threats noted at the visit", "survey design"
)

#' The variables of the models: label, group and the source they are read
#' from (FINDINGS_SOURCES). The landscape variables of the combined model,
#' <VARIABLE>_<radius>, and the threat classes are labelled by
#' landscape_term_label() and threat_label().
FINDINGS_TERMS <- data.frame(
  term = c(
    "TIMING", "METHOD", "TIMING:METHOD", "GRAZE",
    "PLANTabundant", "PLANTdominant", "SANGUISORBA_10", "WET_10", "X_10", "EDGE_10",
    "LOG_AREA", "EVL_species",
    "LOG_CONN", "HIST_POSITIVE", "OTHER_POSITIVE",
    "ERA2021-2024", "DOY_STD", "I(DOY_STD^2)"
  ),
  label = c(
    unname(SYNTHESIS_TERMS[c("TIMING", "METHOD", "TIMING:METHOD")]), "grazing",
    unname(SYNTHESIS_TERMS[c("PLANTabundant", "PLANTdominant")]),
    "Sanguisorba meadows in the site (per 10 pp)", "wet meadows in the site (per 10 pp)",
    "X habitats in the site (per 10 pp)", "edge zone of the site (per 10 pp)",
    "site area (per tenfold)", unname(SYNTHESIS_TERMS["EVL_species"]),
    "connectivity to occupied sites (per tenfold)", "a record of the species at the site in 2012-2018",
    "the other species recorded at the visit",
    "field form of 2021-2024",
    sprintf("day of the visit (per %d days from the year's median)", CONFIG$revised_models$doy_unit_days),
    "day of the visit, squared"
  ),
  group = c(
    rep("management", 4), rep("host plant and habitat", 6), rep("site and protection", 2),
    rep("populations", 3), rep("survey design", 3)
  ),
  source = c(
    rep("management", 3), "polygons", rep("management", 2), rep("habitat", 3), "edge",
    "polygons", "management", "connectivity", "persistence", "cooccurrence", rep("management", 3)
  ),
  stringsAsFactors = FALSE
)

#' The diagnostics of the models of FINDINGS_SOURCES (%s is the species
#' key), whose flags say which fits may be separated.
FINDINGS_DIAGNOSTICS <- c(
  "17_%s_diagnostics", "22_%s_diagnostics", "24_%s_persistence_diagnostics", "17_cooccurrence_diagnostics"
)

#' The model families a variable is counted across, beside its own model
#' (summarise_findings()), for the variables outside step 25.
FINDINGS_FIT_MODELS <- c(
  "_management", "_management_conn", "_management_2021", "_local", "_local_2021", "_habitat_landscape"
)

#' Label of a landscape variable of the combined model, "FOREST_8000" ->
#' "forest within 8 km (per 10 pp)"; NA for any other term.
landscape_term_label <- function(term) {
  pattern <- "^([A-Z]+)_([0-9]+)$"
  variable <- ifelse(grepl(pattern, term), sub(pattern, "\\1", term), NA_character_)
  radius <- suppressWarnings(as.numeric(ifelse(grepl(pattern, term), sub(pattern, "\\2", term), NA)))
  ok <- variable %in% names(LANDSCAPE_LABELS) & radius %in% unlist(CONFIG$landscape$radii_m)
  out <- rep(NA_character_, length(term))
  out[ok] <- sprintf("%s within %s km (per 10 pp)", LANDSCAPE_LABELS[variable[ok]], format_tick(radius[ok] / 1000))
  out
}

# The unit at the end of a label, "(per 10 pp)".
FINDINGS_UNIT <- " \\((per [^)]*)\\)$"

#' A label without its unit, for running text: "site area (per tenfold)" ->
#' "site area".
plain_label <- function(label) sub(FINDINGS_UNIT, "", label)

#' "2.16 (1.48-3.15)".
format_ratio_ci <- function(ratio, low, high) sprintf("%.2f (%.2f-%.2f)", ratio, low, high)

#' A p-value for the text: "< 0.001", "0.012", with its adjustment.
#'
#' @param basis  "Wald", or the adjustment of a threat class ("holm", "BH")
format_p <- function(p, basis = "Wald") {
  basis <- rep_len(basis, length(p))
  out <- ifelse(is.na(p), "", ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p)))
  adjusted <- !is.na(p) & basis != "Wald"
  out[adjusted] <- paste0(out[adjusted], " (", ifelse(basis[adjusted] == "holm", "Holm", basis[adjusted]), ")")
  out
}

#--------------------------------------------------#
## Collect -----
#--------------------------------------------------#

#' Rows of one source from its coefficient table, in every sample.
.findings_from_coefficients <- function(tab, code, source) {
  if (is.null(tab)) {
    return(NULL)
  }
  spec <- FINDINGS_SOURCES[[source]]
  base <- sub("_(pls|obs)$", "", tab$model)
  terms <- FINDINGS_TERMS$term[FINDINGS_TERMS$source == source]
  wanted <- tab$term %in% terms | (source == "habitat" & !is.na(landscape_term_label(tab$term)))
  rows <- tab[base == paste0(code, spec$model) & wanted, , drop = FALSE]
  if (!nrow(rows)) {
    return(NULL)
  }
  data.frame(
    term = rows$term, source = spec$label, model = rows$model, sample = rows$sample,
    odds_ratio = rows$ratio, or_low = rows$ratio_low, or_high = rows$ratio_high,
    p_value = rows$p_value, p_basis = "Wald", stringsAsFactors = FALSE
  )
}

#' The fits of one species that the diagnostics of their step flag for
#' possible separation, as "<model> <sample>".
separated_fits <- function(species) {
  stems <- sprintf(FINDINGS_DIAGNOSTICS, species_key(species))
  diagnostics <- dplyr::bind_rows(lapply(stems, function(stem) {
    d <- .read_result_table(stem)
    if (is.null(d)) NULL else d[, c("model", "sample", "flags")]
  }))
  if (!nrow(diagnostics)) {
    return(character())
  }
  flagged <- grepl("possible separation", diagnostics$flags)
  paste(diagnostics$model[flagged], diagnostics$sample[flagged])
}

#' The odds ratios of every variable of one species, in the three samples.
#'
#' @param species  the species name
#' @return one row per variable and sample, the groups in the order of
#'         FINDINGS_GROUPS: term, label, group, source, model, sample,
#'         odds_ratio, or_low, or_high, p_value, p_basis ("Wald", "holm" or
#'         "BH"), separated (the fit is flagged for possible separation),
#'         estimable and significant
collect_findings <- function(species, alpha = CONFIG$synthesis$alpha) {
  code <- species_code(species)
  key <- species_key(species)
  out <- lapply(names(FINDINGS_SOURCES), function(source) {
    template <- FINDINGS_SOURCES[[source]]$table
    stem <- if (grepl("%s", template, fixed = TRUE)) sprintf(template, key) else template
    .findings_from_coefficients(.read_result_table(stem), code, source)
  })

  threats <- .read_result_table("19_threat_estimates")
  if (!is.null(threats)) {
    threats <- threats[threats$species == species, , drop = FALSE]
    out$threats <- data.frame(
      term = threats$threat, source = "per-threat models (step 19)", model = NA_character_, sample = threats$sample,
      parse_ratio_ci(threats$odds_ratio), p_value = threats$p_adjusted, p_basis = threats$adjustment,
      stringsAsFactors = FALSE
    )
  }
  out <- dplyr::bind_rows(out)
  if (!nrow(out)) stop("No result tables of steps 17-24 for ", species, call. = FALSE)

  known <- match(out$term, FINDINGS_TERMS$term)
  landscape <- landscape_term_label(out$term)
  is_threat <- out$source == "per-threat models (step 19)"
  out$label <- dplyr::case_when(
    is_threat ~ paste(threat_label(out$term), "noted"),
    !is.na(known) ~ FINDINGS_TERMS$label[known],
    !is.na(landscape) ~ landscape,
    TRUE ~ out$term
  )
  out$group <- dplyr::case_when(
    is_threat ~ "threats noted at the visit",
    !is.na(known) ~ FINDINGS_TERMS$group[known],
    !is.na(landscape) ~ "landscape",
    TRUE ~ "other"
  )
  out$separated <- paste(out$model, out$sample) %in% separated_fits(species)
  out$estimable <- is_estimable(out) & !out$separated
  out$significant <- out$estimable & !is.na(out$p_value) & out$p_value < alpha

  # Groups in their order; within a group the terms of FINDINGS_TERMS, the
  # land covers and the threat classes in the order of config.yml.
  land_rank <- match(sub("_[0-9]+$", "", out$term), names(LANDSCAPE_LABELS))
  land_rank[is.na(landscape)] <- NA
  rank <- dplyr::coalesce(
    as.numeric(known),
    100 + land_rank,
    200 + match(out$term, names(CONFIG$strukt_pozn$threat_article17))
  )
  out <- out[order(match(out$group, c(FINDINGS_GROUPS, "other")), rank, match(out$sample, REVISED_SAMPLES)), ]
  rownames(out) <- NULL
  out[, c(
    "term", "label", "group", "source", "model", "sample", "odds_ratio", "or_low", "or_high",
    "p_value", "p_basis", "separated", "estimable", "significant"
  )]
}

#' Every fit of the model families of FINDINGS_FIT_MODELS (steps 17 and 22)
#' that includes a variable, for the count of summarise_findings(); the fits
#' flagged for possible separation are left out.
#'
#' @return term, sample, model, odds_ratio, or_low, or_high
findings_fits <- function(species) {
  code <- species_code(species)
  key <- species_key(species)
  tabs <- dplyr::bind_rows(
    .read_result_table(sprintf("17_%s_coefficients", key)),
    .read_result_table(sprintf("22_%s_coefficients", key))
  )
  if (!nrow(tabs)) {
    return(NULL)
  }
  tabs <- tabs[sub("_(pls|obs)$", "", tabs$model) %in% paste0(code, FINDINGS_FIT_MODELS), , drop = FALSE]
  tabs <- tabs[!paste(tabs$model, tabs$sample) %in% separated_fits(species), , drop = FALSE]
  data.frame(
    term = tabs$term, sample = tabs$sample, model = tabs$model,
    odds_ratio = tabs$ratio, or_low = tabs$ratio_low, or_high = tabs$ratio_high,
    stringsAsFactors = FALSE
  )
}

#--------------------------------------------------#
## Summarise -----
#--------------------------------------------------#

#' One row per variable of one species: its estimate in the primary sample
#' (or in the only sample it is fitted on), in how many samples it is
#' significant, and in how many analyses its interval excludes 1.
#'
#' The analyses are those of step 25 for its key terms (SYNTHESIS_TERMS),
#' and the fits of findings_fits() for the others, in the same sample.
#'
#' @param findings    collect_findings() of one species
#' @param primary     primary_sample() of the species
#' @param robustness  summarise_robustness() of the species (step 25)
#' @param fits        findings_fits() of the species
summarise_findings <- function(findings, primary, robustness = NULL, fits = NULL) {
  out <- findings %>%
    dplyr::group_by(term) %>%
    dplyr::mutate(
      samples_significant = sum(significant),
      samples_estimable = sum(estimable),
      headline = if (primary %in% sample) {
        sample == primary
      } else {
        sample == sample[which.min(match(sample, REVISED_SAMPLES))]
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(headline) %>%
    dplyr::select(-headline)

  out$analyses <- NA_integer_
  out$analyses_excluding_1 <- NA_integer_
  out$analyses_basis <- NA_character_
  if (!is.null(fits) && nrow(fits)) {
    fits <- fits[is_estimable(fits), , drop = FALSE]
    fits$excludes_1 <- fits$or_low > 1 | fits$or_high < 1
    counts <- fits %>%
      dplyr::group_by(term, sample) %>%
      dplyr::summarise(n = dplyr::n(), k = sum(excludes_1), .groups = "drop")
    i <- match(paste(out$term, out$sample), paste(counts$term, counts$sample))
    out$analyses <- counts$n[i]
    out$analyses_excluding_1 <- counts$k[i]
    out$analyses_basis[!is.na(i)] <- "steps 17, 22"
  }
  if (!is.null(robustness) && nrow(robustness)) {
    i <- match(paste(out$term, out$sample), paste(robustness$term, robustness$sample))
    key <- !is.na(i)
    out$analyses[key] <- robustness$analyses[i[key]]
    out$analyses_excluding_1[key] <- robustness$ci_excluding_1[i[key]]
    out$analyses_basis[key] <- "step 25"
  }
  out
}

#' The summary of one species as the report shows it.
#'
#' @param summary  summarise_findings()
findings_table <- function(summary) {
  data.frame(
    group = summary$group,
    variable = summary$label,
    sample = summary$sample,
    odds_ratio_ci = format_ratio_ci(summary$odds_ratio, summary$or_low, summary$or_high),
    p = format_p(summary$p_value, summary$p_basis),
    recorded = ifelse(summary$odds_ratio > 1, "more often", "less often"),
    significant_in_samples = sprintf("%d of %d", summary$samples_significant, summary$samples_estimable),
    intervals_excluding_1 = ifelse(
      is.na(summary$analyses), "",
      sprintf("%d of %d (%s)", summary$analyses_excluding_1, summary$analyses, summary$analyses_basis)
    ),
    model = summary$source,
    stringsAsFactors = FALSE
  )
}

#' The significant variables of one species in words, for the summary:
#' those with which the species was recorded more often and less often,
#' each with its odds ratio. The survey design and the interactions are
#' left out; the interactions are described by findings_interaction().
#'
#' @param summary  summarise_findings()
#' @return a list of `more` and `less`, character vectors
findings_in_words <- function(summary) {
  sig <- summary[summary$significant & summary$group != "survey design" & !grepl(":", summary$term), ]
  words <- label_with_ratio(sig$label, sig$odds_ratio)
  list(more = words[sig$odds_ratio > 1], less = words[sig$odds_ratio < 1])
}

#' A label with its odds ratio, the unit moved inside the parentheses:
#' "connectivity to occupied sites (2.01 per tenfold)".
label_with_ratio <- function(label, ratio) {
  unit <- ifelse(grepl(FINDINGS_UNIT, label), paste0(" ", sub(paste0("^.*", FINDINGS_UNIT), "\\1", label)), "")
  sprintf("%s (%.2f%s)", plain_label(label), ratio, unit)
}

#' A significant interaction of two binary terms in words: the odds ratio
#' of each alone and of both together.
#'
#' @param summary  summarise_findings()
#' @return one sentence per significant interaction
findings_interaction <- function(summary) {
  sig <- summary[summary$significant & grepl(":", summary$term), ]
  vapply(seq_len(nrow(sig)), function(i) {
    parts <- strsplit(sig$term[i], ":", fixed = TRUE)[[1]]
    alone <- summary$odds_ratio[match(parts, summary$term)]
    sprintf(
      "%s and %s each alone %.2f and %.2f, both together %.2f (interaction %.2f)",
      summary$label[match(parts[1], summary$term)], summary$label[match(parts[2], summary$term)],
      alone[1], alone[2], prod(alone) * sig$odds_ratio[i], sig$odds_ratio[i]
    )
  }, character(1))
}

#' Each landscape variable at the radius step 22 chose for it, alone and in
#' the combined model.
#'
#' @param best      22_<species>_landscape_best (sample with the imputed
#'                  absences)
#' @param findings  collect_findings() of the species
#' @param primary   primary_sample() of the species
landscape_alone_table <- function(best, findings, primary) {
  if (is.null(best) || !nrow(best)) {
    return(NULL)
  }
  combined <- findings[findings$group == "landscape" & findings$sample == primary, , drop = FALSE]
  i <- match(paste0(best$variable, "_", best$radius_m), combined$term)
  data.frame(
    variable = unname(LANDSCAPE_LABELS[best$variable]),
    radius_km = best$radius_m / 1000,
    alone_odds_ratio_ci = format_ratio_ci(best$odds_ratio, best$or_low, best$or_high),
    alone_p = format_p(best$p_value),
    delta_AIC = best$delta_AIC_vs_management,
    combined_odds_ratio_ci = ifelse(
      is.na(i), "", format_ratio_ci(combined$odds_ratio[i], combined$or_low[i], combined$or_high[i])
    ),
    combined_p = ifelse(is.na(i), "", format_p(combined$p_value[i])),
    stringsAsFactors = FALSE
  )
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

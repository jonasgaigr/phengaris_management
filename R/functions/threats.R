#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: the threats recorded at the visits (P4.11)
#
#----------------------------------------------------------#
#
# Step 19 models occupancy against each threat class in turn (P3.7). The
# literature review (P4.11) added three things:
#
#   * the threat classes are mapped to the pressure codes of the Article 17
#     reporting (config.yml `strukt_pozn: threat_article17`), so that both
#     field-form lists, and other studies, speak the same language;
#   * one stacked model of all classes at once: each record x class is a row,
#     the response is whether the class was recorded, and each class has its
#     own intercept, its own difference between visits where the species was
#     found and where it was not, and its own era effect, with random
#     intercepts for the record (the visit), the grid square and the
#     observer. The class x occupancy terms say which threats go with the
#     species being found, every class estimated on the same records with the
#     visit-level dependence between threats accounted for;
#   * multiplicity: Holm within the threats with an expected direction, set
#     before looking (config.yml `revised_models: threats: confirmatory`),
#     Benjamini-Hochberg within the others.
#
#----------------------------------------------------------#

#' The records x threat classes, one row each, for the stacked model.
#'
#' @param data     revised_model_data() of one species and sample
#' @param classes  the threat classes to stack
#' @return the records with a threat list, one row per class: CLASS (a
#'         factor), THREAT (0/1), POSITIVE, ERA, NEWER_FORM (1 for the newer
#'         field form, so that each class gets one era term), ID_RECORD,
#'         SITMAP, OBSERVER
stack_threats <- function(data, classes) {
  d <- data[!is.na(data$SUM_THREATS), , drop = FALSE]
  newer <- as.numeric(as.integer(factor(d$ERA)) == 2)
  dplyr::bind_rows(lapply(classes, function(k) {
    tibble::tibble(
      CLASS = k, THREAT = d[[k]], POSITIVE = d$POSITIVE, ERA = d$ERA, NEWER_FORM = newer,
      ID_RECORD = d$ID_RECORD, SITMAP = d$SITMAP, OBSERVER = d$OBSERVER
    )
  })) %>%
    dplyr::mutate(CLASS = factor(CLASS, levels = classes))
}

#' Fit the stacked threat model.
#'
#' @param stacked  stack_threats()
#' @param id       model id
#' @param sample   the sample, for the tables
#' @return a fitted-model record of fit_tmb()
fit_stacked_threats <- function(stacked, id, sample) {
  era <- dplyr::n_distinct(stacked$NEWER_FORM) > 1
  formula <- stats::as.formula(paste(
    "THREAT ~ 0 + CLASS + CLASS:POSITIVE", if (era) "+ CLASS:NEWER_FORM",
    "+ (1 | ID_RECORD) + (1 | SITMAP) + (1 | OBSERVER)"
  ))
  fit_tmb(id, "All threat classes, stacked", formula, stacked, group = "Stacked threats", sample = sample)
}

#' p-values adjusted for multiplicity, by family.
#'
#' @param p             p-values
#' @param class         threat class of each p-value
#' @param confirmatory  the classes adjusted with Holm; the others with
#'                      Benjamini-Hochberg
#' @return list(adjusted = the adjusted p-values, method = "holm" or "BH")
adjust_threat_p <- function(p, class, confirmatory = unlist(CONFIG$revised_models$threats$confirmatory)) {
  is_confirmatory <- class %in% confirmatory
  adjusted <- rep(NA_real_, length(p))
  adjusted[is_confirmatory] <- stats::p.adjust(p[is_confirmatory], "holm")
  adjusted[!is_confirmatory] <- stats::p.adjust(p[!is_confirmatory], "BH")
  list(adjusted = adjusted, method = ifelse(is_confirmatory, "holm", "BH"))
}

#' The Article 17 pressure code of each threat class.
article17_code <- function(class, codes = CONFIG$strukt_pozn$threat_article17) {
  out <- unname(unlist(codes)[class])
  out[is.na(out)] <- ""
  out
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

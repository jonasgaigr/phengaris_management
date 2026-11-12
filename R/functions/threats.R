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

#' Set the threat classes to NA in an era whose form could not record them.
#'
#' The 2019-2020 threat list has no item for some classes of the current list
#' (e.g. abandonment), so in that era their 0 means "not on the form", not
#' "not seen" (as REVIEW.md, item 6, for the other variables). A class
#' recorded at fewer than `min_records` records of an era, over both species,
#' is taken to be off that era's form, and is NA there.
#'
#' @param data         records with the class flags, SUM_THREATS and the era
#' @param classes      the threat classes
#' @param era          the era column
#' @param min_records  config.yml `strukt_pozn: threat_min_records_per_era`
#' @return list(data = `data` with the masked cells set to NA, masked = a
#'         table of the class x era combinations masked and the records they
#'         held)
mask_unlisted_threats <- function(data, classes = names(CONFIG$strukt_pozn$threat_items), era = "PROTOCOL_ERA",
                                  min_records = CONFIG$strukt_pozn$threat_min_records_per_era) {
  listed <- !is.na(data$SUM_THREATS)
  masked <- list()
  for (k in classes) {
    for (e in sort(unique(data[[era]]))) {
      rows <- listed & data[[era]] == e
      recorded <- sum(data[[k]][rows] == 1, na.rm = TRUE)
      if (recorded < min_records) {
        masked[[length(masked) + 1]] <- tibble::tibble(
          class = k, era = e, records_with_threat = recorded, records_masked = sum(rows)
        )
        data[[k]][data[[era]] == e] <- NA
      }
    }
  }
  list(
    data = data,
    masked = if (length(masked)) dplyr::bind_rows(masked) else tibble::tibble(class = character(), era = character())
  )
}

#' Set the threat classes to NA in the years their record cannot be trusted.
#'
#' Beside the classes off the form of a whole era (mask_unlisted_threats()),
#' a class can be unusable in a single year of an era: in 2021 "absence či
#' nedostatek péče" is listed at 1 record of 627 with a threat list against
#' 23-29 % in 2022-2024, and "nevhodná pastva" at 34 % against 1-3 %, with
#' grazing recorded at 9 of those 212 records. Such a class is unknown in
#' that year, not absent and not present.
#'
#' @param data     records with the class flags, YEAR and SUM_THREATS
#' @param unknown  config.yml `strukt_pozn: threat_unknown_years`: the years
#'                 of each class
#' @return list(data = `data` with the masked cells set to NA, masked = a
#'         table of the class x year combinations masked, the records with
#'         a threat list they held and those listing the class)
mask_threat_years <- function(data, unknown = CONFIG$strukt_pozn$threat_unknown_years) {
  listed <- !is.na(data$SUM_THREATS)
  masked <- list()
  for (k in names(unknown)) {
    for (y in unlist(unknown[[k]])) {
      rows <- data$YEAR == y
      masked[[length(masked) + 1]] <- tibble::tibble(
        class = k, year = y,
        records_with_threat = sum(data[[k]][rows & listed] == 1, na.rm = TRUE),
        records_masked = sum(rows & listed & !is.na(data[[k]]))
      )
      data[[k]][rows] <- NA
    }
  }
  list(
    data = data,
    masked = if (length(masked)) dplyr::bind_rows(masked) else tibble::tibble(class = character(), year = integer())
  )
}

#' The records x threat classes, one row each, for the stacked model.
#'
#' A class that is NA (off the form of an era, mask_unlisted_threats()) has
#' no rows in that era.
#'
#' @param data     revised_model_data() of one species and sample
#' @param classes  the threat classes to stack
#' @return the records with a threat list, one row per class: CLASS (a
#'         factor), THREAT (0/1), POSITIVE, ERA, NEWER_FORM (1 for the newer
#'         field form), ID_RECORD, SITMAP, OBSERVER
stack_threats <- function(data, classes) {
  d <- data[!is.na(data$SUM_THREATS), , drop = FALSE]
  newer <- as.numeric(as.integer(factor(d$ERA)) == 2)
  dplyr::bind_rows(lapply(classes, function(k) {
    tibble::tibble(
      CLASS = k, THREAT = d[[k]], POSITIVE = d$POSITIVE, ERA = d$ERA, NEWER_FORM = newer,
      ID_RECORD = d$ID_RECORD, SITMAP = d$SITMAP, OBSERVER = d$OBSERVER
    )
  })) %>%
    dplyr::filter(!is.na(THREAT)) %>%
    dplyr::mutate(CLASS = factor(CLASS, levels = classes))
}

#' Fit the stacked threat model.
#'
#' Each class recorded in both eras gets its own era term (ERA_<class>, 1 for
#' a row of the class from the newer form); a class recorded in one era only
#' gets none.
#'
#' @param stacked  stack_threats()
#' @param id       model id
#' @param sample   the sample, for the tables
#' @return a fitted-model record of fit_tmb()
fit_stacked_threats <- function(stacked, id, sample) {
  both <- names(which(tapply(stacked$NEWER_FORM, stacked$CLASS, function(x) dplyr::n_distinct(x) > 1)))
  era_terms <- character()
  for (k in both) {
    column <- paste0("ERA_", k)
    stacked[[column]] <- as.numeric(stacked$CLASS == k) * stacked$NEWER_FORM
    era_terms <- c(era_terms, column)
  }
  formula <- stats::as.formula(paste(
    "THREAT ~ 0 + CLASS + CLASS:POSITIVE", if (length(era_terms)) paste("+", paste(era_terms, collapse = " + ")),
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

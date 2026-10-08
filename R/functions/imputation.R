#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: targeted records and imputed absences
#
#----------------------------------------------------------#
#
# The two species are surveyed on the same visits, so a targeted record of one
# species at a place inside the other species' reporting range is evidence that
# the other species was looked for and not found. These functions turn that
# evidence into explicit absence records.
#
# The reporting range matters twice. A species can only be imputed absent at a
# place inside its own reporting range: the candidate records of the other
# species are intersected with the range of the species being imputed, and a
# record outside it is never turned into an absence. And under the original
# rule the range cell (a 10 x 10 km square, row_n) is also the unit that
# decides whether the species was already recorded "here".
#
#----------------------------------------------------------#

#--------------------------------------------------#
## Targeted records inside a reporting range -----
#--------------------------------------------------#

#' Targeted records of one species inside its own reporting range.
#'
#' Intersects the records with the range cells, so a record whose site
#' touches two cells appears once per cell, with the cell in row_n. Records
#' outside the range are dropped.
#'
#' @param records  records with site geometry (sf), both species
#' @param range    reporting range cells of `species`, with row_n
#' @param species  the species whose records are kept
#' @param sources  ZDROJ values counted as targeted monitoring
#' @return a data frame without geometry
targeted_records_in_range <- function(records, range, species, sources) {
  sf::st_intersection(
    records %>%
      dplyr::filter(DRUH == species),
    range
  ) %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(
      ZDROJ %in% sources
    )
}

#--------------------------------------------------#
## The original imputation rule -----
#--------------------------------------------------#

#' Absences of one species imputed from the other species' records.
#'
#' The original rule: every targeted record of the *other* species that falls
#' inside this species' reporting range, in a range cell where this species
#' has no targeted record at all (in any year), becomes an absence of this
#' species. Every column of the source record is carried over.
#'
#' @param records        records with site geometry (sf), both species
#' @param range          reporting range cells of the species being imputed
#' @param other_range    reporting range cells of the other species, used
#'                       only for the original range_* flags
#' @param species        the species being imputed absent
#' @param other_species  the species whose records serve as evidence
#' @param covered_cells  row_n of the cells where `species` was recorded
#' @param sources        ZDROJ values counted as targeted monitoring
#' @param flag_self,flag_other  names of the range flag columns of `species`
#'                       and of the other species ("range_Pnau", "range_Ptel")
imputed_absences_legacy <- function(records, range, other_range, species,
                                    other_species, covered_cells, sources,
                                    flag_self, flag_other) {
  sf::st_intersection(
    records %>%
      dplyr::filter(DRUH == other_species, ZDROJ %in% sources),
    range
  ) %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(
      !row_n %in% covered_cells
    ) %>%
    dplyr::mutate(
      DRUH = species,
      NEGATIV = 1,
      IMPUTED = TRUE,
      !!flag_self := 1,
      # row_n here indexes `range`, not `other_range`, so this flag carries no
      # information; it is kept as the original computed it.
      !!flag_other := dplyr::case_when(row_n %in% other_range$row_n ~ 1, TRUE ~ 0),
      range_both = .data[[flag_self]] + .data[[flag_other]]
    ) %>%
    dplyr::select(
      ID_LOKAL, row_n, DRUH, NEGATIV, IMPUTED,
      range_Pnau, range_Ptel, range_both, dplyr::everything()
    )
}

#' Observed targeted records plus imputed absences, by the original rule.
#'
#' @param records           records with site geometry (sf), both species
#' @param range_nausithous  reporting range cells of P. nausithous, with row_n
#' @param range_teleius     reporting range cells of P. teleius, with row_n
#' @param sources           ZDROJ values counted as targeted monitoring
#' @return one row per targeted record x range cell, observed (IMPUTED =
#'         FALSE) followed by the imputed absences (IMPUTED = TRUE)
impute_absences_legacy <- function(records, range_nausithous, range_teleius,
                                   sources = target_mon_zdroj) {
  observed_nau <-
    targeted_records_in_range(records, range_nausithous, SPECIES_NAU, sources) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      range_Pnau = dplyr::case_when(
        row_n %in% range_nausithous$row_n ~ 1,
        TRUE ~ 0
      )
    )

  observed_tel <-
    targeted_records_in_range(records, range_teleius, SPECIES_TEL, sources) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      range_Ptel = dplyr::case_when(
        row_n %in% range_teleius$row_n ~ 1,
        TRUE ~ 0
      )
    )

  observed <-
    dplyr::bind_rows(observed_nau, observed_tel) %>%
    dplyr::mutate(
      range_both = sum(range_Pnau, range_Ptel, na.rm = TRUE)
    )

  imputed_nau <- imputed_absences_legacy(
    records, range_nausithous, range_teleius,
    species = SPECIES_NAU, other_species = SPECIES_TEL,
    covered_cells = observed$row_n[observed$DRUH == SPECIES_NAU],
    sources = sources, flag_self = "range_Pnau", flag_other = "range_Ptel"
  )

  imputed_tel <- imputed_absences_legacy(
    records, range_teleius, range_nausithous,
    species = SPECIES_TEL, other_species = SPECIES_NAU,
    covered_cells = observed$row_n[observed$DRUH == SPECIES_TEL],
    sources = sources, flag_self = "range_Ptel", flag_other = "range_Pnau"
  )

  observed %>%
    dplyr::mutate(
      IMPUTED = FALSE
    ) %>%
    dplyr::bind_rows(
      imputed_tel,
      imputed_nau
    )
}

#--------------------------------------------------#
## Summaries -----
#--------------------------------------------------#

#' Records by species, presence/absence and imputation status.
summarise_imputation <- function(data_with_imputed) {
  data_with_imputed %>%
    dplyr::group_by(
      DRUH,
      NEGATIV,
      IMPUTED
    ) %>%
    dplyr::reframe(
      number = dplyr::n()
    )
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

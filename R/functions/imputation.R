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
## One row per record -----
#--------------------------------------------------#

#' The range cell holding each site's representative point.
#'
#' @param records  records with site geometry (sf)
#' @param range    reporting range cells, with row_n
#' @return named vector: ID_LOKAL -> row_n, NA where the point lies in no cell
representative_cells <- function(records, range) {
  sites <- records %>%
    dplyr::distinct(ID_LOKAL, .keep_all = TRUE)
  points <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(sites)))
  hit <- sf::st_intersects(points, range)
  cell <- vapply(hit, function(i) if (length(i)) range$row_n[min(i)] else NA_integer_, integer(1))
  stats::setNames(cell, sites$ID_LOKAL)
}

#' Keep one row per record and species.
#'
#' The intersection with the reporting range repeats a record once per range
#' cell its site touches. This keeps the row of the cell that holds the site's
#' representative point, and the lowest row_n when that point lies in none of
#' the touched cells.
#'
#' @param x      rows with ID_NALEZ, DRUH, ID_LOKAL and row_n
#' @param cells  named list by species of representative_cells() results
deduplicate_range_records <- function(x, cells) {
  rep_cell <- rep(NA_integer_, nrow(x))
  for (species in names(cells)) {
    is_species <- x$DRUH == species
    rep_cell[is_species] <- cells[[species]][as.character(x$ID_LOKAL[is_species])]
  }
  x %>%
    dplyr::mutate(
      .order = dplyr::row_number(),
      .preferred = !is.na(rep_cell) & row_n == rep_cell
    ) %>%
    dplyr::group_by(ID_NALEZ, DRUH) %>%
    dplyr::arrange(dplyr::desc(.preferred), row_n, .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.order) %>%
    dplyr::select(-.order, -.preferred)
}

#' A unique identifier for every row: the record and the species.
#'
#' An imputed absence keeps the ID_NALEZ of the record of the other species it
#' was imputed from, so ID_NALEZ alone is not unique; ID_NALEZ with the
#' species is.
record_id <- function(id_nalez, druh) {
  code <- dplyr::case_when(
    druh == SPECIES_NAU ~ "nau",
    druh == SPECIES_TEL ~ "tel"
  )
  paste(format(id_nalez, scientific = FALSE, trim = TRUE), code, sep = "_")
}

#--------------------------------------------------#
## Imputed rows describe the imputed species -----
#--------------------------------------------------#

#' Empty the other species' observation fields on imputed absences.
#'
#' An imputed absence starts as a copy of a record of the other species, so it
#' carries that species' count, counting unit, behaviour and taxon note. Those
#' columns are set to NA, and CESKE_JMENO is set to the Czech name of the
#' imputed species, taken from the observed records.
#'
#' @param x         rows with IMPUTED and DRUH
#' @param columns   the species-specific columns, config.yml `imputation:
#'                  species_specific_columns`
#' @param observed  observed records, to look up each species' Czech name
blank_species_fields <- function(x, columns = CONFIG$imputation$species_specific_columns,
                                 observed = x[!x$IMPUTED, ]) {
  czech_names <- observed %>%
    dplyr::distinct(DRUH, CESKE_JMENO)
  czech_names <- stats::setNames(czech_names$CESKE_JMENO, czech_names$DRUH)

  for (col in intersect(columns, names(x))) {
    x[[col]][x$IMPUTED] <- NA
  }
  if ("CESKE_JMENO" %in% names(x)) {
    x$CESKE_JMENO[x$IMPUTED] <- unname(czech_names[x$DRUH[x$IMPUTED]])
  }
  x
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
#' @param species        the species being imputed absent
#' @param other_species  the species whose records serve as evidence
#' @param covered_cells  row_n of every cell where `species` was recorded
#' @param sources        ZDROJ values counted as targeted monitoring
imputed_absences_legacy <- function(records, range, species, other_species,
                                    covered_cells, sources) {
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
      IMPUTED = TRUE
    )
}

#' Observed targeted records plus imputed absences, by the original rule.
#'
#' Every range cell a record's site touches counts as surveyed for that
#' species; the output then keeps one row per record and species (see
#' deduplicate_range_records()).
#'
#' @param records           records with site geometry (sf), both species
#' @param range_nausithous  reporting range cells of P. nausithous, with row_n
#' @param range_teleius     reporting range cells of P. teleius, with row_n
#' @param sources           ZDROJ values counted as targeted monitoring
#' @return one row per record and species, observed (IMPUTED = FALSE)
#'         followed by the imputed absences (IMPUTED = TRUE), with the unique
#'         key ID_RECORD
impute_absences_legacy <- function(records, range_nausithous, range_teleius,
                                   sources = target_mon_zdroj) {
  observed <- dplyr::bind_rows(
    targeted_records_in_range(records, range_nausithous, SPECIES_NAU, sources),
    targeted_records_in_range(records, range_teleius, SPECIES_TEL, sources)
  )

  imputed_nau <- imputed_absences_legacy(
    records, range_nausithous,
    species = SPECIES_NAU, other_species = SPECIES_TEL,
    covered_cells = observed$row_n[observed$DRUH == SPECIES_NAU],
    sources = sources
  )

  imputed_tel <- imputed_absences_legacy(
    records, range_teleius,
    species = SPECIES_TEL, other_species = SPECIES_NAU,
    covered_cells = observed$row_n[observed$DRUH == SPECIES_TEL],
    sources = sources
  )

  cells <- stats::setNames(
    list(
      representative_cells(records, range_nausithous),
      representative_cells(records, range_teleius)
    ),
    c(SPECIES_NAU, SPECIES_TEL)
  )

  observed %>%
    dplyr::mutate(
      IMPUTED = FALSE
    ) %>%
    dplyr::bind_rows(
      imputed_tel,
      imputed_nau
    ) %>%
    deduplicate_range_records(cells) %>%
    blank_species_fields() %>%
    dplyr::mutate(
      ID_RECORD = record_id(ID_NALEZ, DRUH),
      .before = 1
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

#----------------------------------------------------------#
# impute_absences(): deduplication, the imputation rules, blanked fields -----
#----------------------------------------------------------#
#
# Two 1 km cells side by side (the reporting range of both species in this
# fixture, except where noted), and these sites:
#
#   11  point in cell 1
#   12  polygon straddling cells 1 and 2, most of it in cell 2
#   13  point in cell 2
#   14  point east of both cells, outside the range
#
#----------------------------------------------------------#

cells <- square_cells(n = 2)
range_nau <- cells
range_tel <- cells[1, ] # P. teleius' range covers cell 1 only

pt <- function(x, y) sf::st_point(c(x, y))
sites <- sf::st_sf(
  ID_LOKAL = c(11, 12, 13, 14),
  geometry = sf::st_sfc(
    pt(-699500, -1099500),
    sf::st_polygon(list(rbind(
      c(-699100, -1099600), c(-698200, -1099600), c(-698200, -1099400),
      c(-699100, -1099400), c(-699100, -1099600)
    ))),
    pt(-698500, -1099500),
    pt(-697500, -1099500),
    crs = 5514
  )
)

src <- "targeted"
make_records <- function(...) {
  r <- tibble::tibble(...)
  r$ZDROJ <- src
  r$CESKE_JMENO <- ifelse(r$DRUH == SPECIES_NAU, "modrásek bahenní", "modrásek očkovaný")
  r$POCET <- ifelse(r$NEGATIV == 0, 5, NA)
  r %>%
    dplyr::left_join(sites, by = "ID_LOKAL") %>%
    sf::st_as_sf()
}

records <- make_records(
  ID_NALEZ = c(1, 2, 3, 4, 5, 6),
  DRUH = c(SPECIES_NAU, SPECIES_NAU, SPECIES_TEL, SPECIES_NAU, SPECIES_TEL, SPECIES_TEL),
  ID_LOKAL = c(11, 12, 12, 13, 13, 14),
  DATUM_OD = c(20220701, 20220702, 20220702, 20220703, 20220704, 20220705),
  NEGATIV = c(0, 0, 1, 0, 0, 1)
)

test_that("a site touching two cells keeps one row, in the cell of its representative point", {
  out <- impute_absences(records, range_nau, range_tel, sources = src, rule = "none")
  expect_equal(sum(out$ID_NALEZ == 2), 1)
  # Most of polygon 12 lies in cell 2.
  expect_equal(out$row_n[out$ID_NALEZ == 2], 2L)
  expect_false(anyDuplicated(out$ID_RECORD) > 0)
})

test_that("records outside their species' own range are dropped, or kept on request", {
  # Record 5 (P. teleius at site 13, cell 2) and 6 (site 14) are outside the
  # P. teleius range.
  dropped <- impute_absences(records, range_nau, range_tel, sources = src, rule = "none")
  expect_setequal(dropped$ID_NALEZ[dropped$DRUH == SPECIES_TEL], 3)
  kept <- impute_absences(records, range_nau, range_tel,
    sources = src, rule = "none",
    keep_outside_range = TRUE
  )
  expect_setequal(kept$ID_NALEZ[kept$DRUH == SPECIES_TEL], c(3, 5, 6))
  expect_true(all(is.na(kept$row_n[kept$ID_NALEZ %in% c(5, 6)])))
})

test_that("the visit rule imputes per visit, only inside the imputed species' range", {
  out <- impute_absences(records, range_nau, range_tel, sources = src, rule = "visit")
  imputed <- out[out$IMPUTED, ]
  # P. teleius absent at the P. nausithous visit 1 (site 11, cell 1); not at
  # visit 2 (site 12), where P. teleius has its own record, an explicit
  # negative; not at visit 4 (site 13, outside the P. teleius range).
  expect_setequal(imputed$ID_NALEZ[imputed$DRUH == SPECIES_TEL], 1)
  # P. nausithous absent at the P. teleius visit at site 13 (record 5);
  # site 14 lies outside every range.
  expect_setequal(imputed$ID_NALEZ[imputed$DRUH == SPECIES_NAU], 5)
  expect_true(all(imputed$NEGATIV == 1))
})

test_that("the original rule imputes by range cell, across dates", {
  out <- impute_absences(records, range_nau, range_tel, sources = src, rule = "legacy_cell")
  imputed <- out[out$IMPUTED, ]
  # P. teleius was recorded in cell 1 (record 3, via site 12), so no
  # P. teleius absence is imputed there, whatever the date.
  expect_false(any(imputed$DRUH == SPECIES_TEL))
  # P. nausithous was recorded in both cells, so none is imputed either.
  expect_false(any(imputed$DRUH == SPECIES_NAU))
})

test_that("imputed absences carry no count of the other species and their own Czech name", {
  out <- impute_absences(records, range_nau, range_tel, sources = src, rule = "visit")
  imputed <- out[out$IMPUTED, ]
  expect_true(all(is.na(imputed$POCET)))
  expect_equal(unique(imputed$CESKE_JMENO[imputed$DRUH == SPECIES_TEL]), "modrásek očkovaný")
  expect_equal(unique(imputed$CESKE_JMENO[imputed$DRUH == SPECIES_NAU]), "modrásek bahenní")
  # The record they came from keeps its count.
  expect_equal(out$POCET[!out$IMPUTED & out$ID_NALEZ == 1], 5)
})

test_that("ID_RECORD is unique although imputed rows reuse ID_NALEZ", {
  out <- impute_absences(records, range_nau, range_tel, sources = src, rule = "visit")
  expect_true(any(duplicated(out$ID_NALEZ)))
  expect_false(any(duplicated(out$ID_RECORD)))
  expect_true("1_tel" %in% out$ID_RECORD)
})

test_that("records from other sources are ignored", {
  other <- records
  other$ZDROJ[other$ID_NALEZ == 1] <- "opportunistic"
  out <- impute_absences(other, range_nau, range_tel, sources = src, rule = "visit")
  expect_false(1 %in% out$ID_NALEZ)
})

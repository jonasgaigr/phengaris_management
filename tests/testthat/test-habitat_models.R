#----------------------------------------------------------#
# Habitat models: the mowing-timing check and the habitat proxies -----
# (P4.5, P4.6)
#----------------------------------------------------------#

test_that("freshly mown visits and the no-cut window are recognised", {
  expect_equal(
    fresh_mown(c("Čerstvě pokoseno", "pokoseno ještě v den monitoringu", "louka nesečená", NA), "čerstvě po|v den monitoringu"),
    c(TRUE, TRUE, FALSE, FALSE)
  )
  expect_equal(
    in_no_cut_window(c(20220630, 20220701, 20220815, 20220901, 20220902), c("07-01", "09-01")),
    c(FALSE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_equal(tenths(0.35), 3.5)
})

test_that("the configured pattern finds the notes of freshly mown sites", {
  notes <- c(
    "pomer: 1:4, Pokoseno ještě v den monitoringu!", "Čerstvě pokoseno",
    "louka s ovocným sadem, nevhodná seč oproti minulému roku, v době kontroly čerstvě posečeno",
    "část plochy s krvavci sečena (v době návštěvy čerstvě pokoseno)",
    "plocha poměrně čerstvě posečená, ale několik krvavců zde kvetlo",
    "pokoseno pozde", "intenzivně a nevhodně sečená louka"
  )
  expect_equal(fresh_mown(notes), c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("the edge share is the part of a polygon within the edge zone, and NA for KVES areas", {
  square <- function(side) {
    sf::st_polygon(list(rbind(c(0, 0), c(side, 0), c(side, side), c(0, side), c(0, 0))))
  }
  areas <- sf::st_sf(
    AREA_SOURCE = c("site", "site", "KVES"),
    geometry = sf::st_sfc(square(100), square(40), square(100), crs = 5514)
  )
  out <- edge_share(areas, width = 25)
  # 100 m square: the core is 50 x 50 m, so 75 % lies in the edge zone; a
  # 40 m square is all edge.
  expect_equal(out[1:2], c(0.75, 1), tolerance = 1e-6)
  expect_true(is.na(out[3]))
})

test_that("the wet-meadow share is summarised with the other habitat shares", {
  segments <- tibble::tibble(
    ID_LOKAL = 1, BIOTOP = c("T1.1", "T1.5", "X5"), FSB = "T", HABITAT = NA_character_,
    AREA_real = c(5000, 3000, 2000)
  )
  out <- select_habitat_segment(
    tibble::tibble(ID_LOKAL = 1), segments, rule = "area_weighted",
    sanguisorba_codes = c("T1.1", "T1.5"), wet_codes = c("T1.5")
  )
  expect_equal(out$WET_SHARE, 0.3)
  expect_equal(out$SANGUISORBA_SHARE, 0.8)
  expect_equal(out$X_SHARE, 0.2)
})

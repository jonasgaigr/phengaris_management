#----------------------------------------------------------#
# The populations known around a site (P4.3, P4.8) -----
#----------------------------------------------------------#

test_that("the nearest distance is in km and Inf without targets", {
  expect_equal(nearest_distance_km(c(0, 3000), c(0, 4000), c(0, 1000), c(1000, 0)), c(1, sqrt(3^2 + 3^2)))
  expect_equal(nearest_distance_km(1, 1, numeric(), numeric()), Inf)
})

x0 <- -700000
y0 <- -1100000
square <- function(x, y, w = 100) {
  sf::st_polygon(list(rbind(c(x, y), c(x + w, y), c(x + w, y + w), c(x, y + w), c(x, y))))
}

test_that("site_history_table() finds the nearest positive record and the earlier populations", {
  sites <- sf::st_sf(
    ID_LOKAL = c(1, 2),
    geometry = sf::st_sfc(square(x0, y0), square(x0 + 5000, y0), crs = 5514)
  )
  site_design <- tibble::tibble(ID_LOKAL = c(1, 2), SITE_X = c(x0 + 50, x0 + 5050), SITE_Y = y0 + 50)
  records <- tibble::tibble(
    ID_LOKAL = c(1, 2), DRUH = SPECIES_NAU, NEGATIV = c(1, 0), DATUM_OD = 20220720,
    X = c(x0 + 50, x0 + 5050), Y = y0 + 50
  )
  # 2012-2018: P. nausithous found 30 m from site 1 (in 2015 and 2017) and
  # 1 km from site 2 only in a negative record; P. teleius nowhere.
  sites_history <- sf::st_sf(
    ID_LOKAL = c(11, 12),
    geometry = sf::st_sfc(square(x0 + 130, y0), square(x0 + 6100, y0), crs = 5514)
  )
  records_history <- tibble::tibble(
    ID_LOKAL = c(11, 11, 12), DRUH = SPECIES_NAU, NEGATIV = c(0, 0, 1),
    DATUM_OD = c(20150720, 20170720, 20160720), X = c(x0 + 180, x0 + 180, x0 + 6150), Y = y0 + 50
  )
  out <- site_history_table(
    site_design, sites, records, records_history, sites_history,
    surveyed = c(1, 2), distance_m = 50
  )
  nau <- out[out$DRUH == SPECIES_NAU, ]
  # Site 1: the nearest positive is the 2012-2018 one 130 m away (its own
  # 2022 record is negative); site 2: its own 2022 positive.
  expect_equal(nau$NEAREST_POSITIVE_KM, c(0.13, 0))
  expect_equal(nau$HIST_POSITIVE, c(1, 0))
  expect_equal(nau$HIST_LAST_YEAR, c(2017L, NA))
  tel <- out[out$DRUH == SPECIES_TEL, ]
  expect_equal(tel$NEAREST_POSITIVE_KM, c(Inf, Inf))
  expect_equal(tel$HIST_POSITIVE, c(0, 0))
})

test_that("the plausible sample keeps the observed records and the plausible imputed absences", {
  d <- tibble::tibble(
    DRUH = SPECIES_TEL, IN_FLIGHT_WINDOW = TRUE, DOY_DEV = 0,
    IMPUTED = c(FALSE, TRUE, TRUE), IMPUTED_PLAUSIBLE = c(FALSE, TRUE, FALSE),
    PROTOCOL_ERA = "2021-2024", PLANT_QUANT = 1, SITE_AREA_HA = 1
  )
  expect_equal(nrow(revised_model_data(d, sample = "with imputed")), 3)
  expect_equal(revised_model_data(d, sample = "with plausible imputed")$IMPUTED, c(FALSE, TRUE))
  expect_equal(nrow(revised_model_data(d, sample = "observed only")), 1)
  expect_equal(unname(vapply(REVISED_SAMPLES, sample_suffix, character(1))), c("", "_pls", "_obs"))
})

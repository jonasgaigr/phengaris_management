#----------------------------------------------------------#
# The flight curve (P4.1) -----
#----------------------------------------------------------#

test_that("the curve data take targeted, exactly dated visits with a count", {
  records <- tibble::tibble(
    DRUH = SPECIES_NAU, ID_LOKAL = c(1, 1, 2, 3, 4), SITMAP = 5000,
    DATUM_OD = c(20220725, 20220730, 20220801, 20221101, 20220725),
    DATUM_DO = c(20220725, 20220730, 20220805, 20221101, 20220725),
    NEGATIV = c(0, 1, 0, 1, 0), POCET = c(3, NA, 2, NA, NA),
    ZDROJ = c("t", "t", "t", "t", "o")
  )
  fw <- tibble::tibble(DRUH = SPECIES_NAU, YEAR = 2022L, median_doy = day_of_year(20220725))
  sd <- tibble::tibble(ID_LOKAL = 1:4, ELEVATION = c(300, 300, 400, 500))
  out <- flight_curve_data(records, fw, sd, sources = "t", max_deviation = 45)
  # Dropped: a date range (site 2), a visit 99 days away (site 3) and an
  # opportunistic record (site 4).
  expect_equal(out$COUNT, c(3, 0))
  expect_equal(out$DOY_DEV, c(0, 5))
  expect_equal(out$ELEVATION, c(300, 300))
})

# Visits of one species with a Gaussian flight curve peaking 5 days after
# the median, sd 8 days, at 60 sites in 3 years.
simulated_visits <- function(n = 900, seed = 1) {
  set.seed(seed)
  d <- tibble::tibble(
    DRUH = SPECIES_NAU,
    YEAR = sample(2019:2021, n, replace = TRUE),
    SITMAP = sample(1:60, n, replace = TRUE),
    ID_LOKAL = seq_len(n),
    DOY_DEV = sample(-25:35, n, replace = TRUE),
    ELEVATION = sample(seq(200, 600, by = 10), n, replace = TRUE)
  )
  mu <- exp(1.5 - (d$DOY_DEV - 5)^2 / (2 * 8^2))
  d$COUNT <- stats::rnbinom(n, mu = mu, size = 2)
  d
}

test_that("the fitted curve peaks where the simulated one does and is relative to its peak", {
  curve <- fit_flight_curve(simulated_visits(), k = 5, elevation = "never")
  expect_false(curve$elevation)
  expect_equal(curve$doy_range, c(-25, 35))
  tab <- flight_curve_table(curve, SPECIES_NAU, doy = -45:45)
  expect_equal(max(tab$RELATIVE_ACTIVITY), 1)
  expect_true(abs(tab$DOY_DEV[which.max(tab$RELATIVE_ACTIVITY)] - 5) <= 3)
  expect_true(all(is.na(tab$ELEVATION)))
  expect_equal(sum(tab$IN_DATA_RANGE), 61)
  bounds <- flight_season_bounds(tab, threshold = 0.05)
  # A Gaussian with sd 8 falls to 5 % of its peak 2.45 sd (20 days) away.
  expect_true(abs(bounds$first_doy_dev - (5 - 20)) <= 5)
  expect_true(abs(bounds$last_doy_dev - (5 + 20)) <= 5)
})

test_that("the elevation variant is compared by AIC and kept only when it is better", {
  d <- simulated_visits()
  curve <- fit_flight_curve(d, k = 5, elevation = "when_better")
  expect_equal(nrow(curve$comparison), 2)
  expect_equal(sum(curve$comparison$chosen), 1)
  expect_equal(curve$elevation, curve$comparison$variant[curve$comparison$chosen] == "elevation")
  forced <- fit_flight_curve(d, k = 5, elevation = "always")
  expect_true(forced$elevation)
  tab <- flight_curve_table(forced, SPECIES_NAU, doy = -10:10, elevation_step = 100)
  expect_equal(sort(unique(tab$ELEVATION)), seq(200, 600, by = 100))
  # Relative to the peak at each elevation.
  expect_true(all(tapply(tab$RELATIVE_ACTIVITY, tab$ELEVATION, max) <= 1))
  # Without any elevation the variant cannot be fitted.
  d$ELEVATION <- NA_real_
  expect_false(fit_flight_curve(d, k = 5, elevation = "always")$elevation)
})

test_that("each record takes the activity of its species, day and nearest elevation", {
  curve <- tibble::tibble(
    DRUH = c(rep(SPECIES_NAU, 4), rep(SPECIES_TEL, 2)),
    ELEVATION = c(200, 200, 225, 225, NA, NA),
    DOY_DEV = c(0L, 1L, 0L, 1L, 0L, 1L),
    RELATIVE_ACTIVITY = c(1, 0.5, 0.9, 0.4, 1, 0.2)
  )
  out <- relative_activity(
    druh = c(SPECIES_NAU, SPECIES_NAU, SPECIES_NAU, SPECIES_TEL, SPECIES_TEL, SPECIES_NAU),
    doy_dev = c(0, 1, 1, 1, 7, 0),
    elevation = c(205, 220, 900, 300, 300, NA),
    curve = curve
  )
  # 900 m takes the highest tabulated elevation, NA the median one; a day
  # outside the grid gives NA.
  expect_equal(out, c(1, 0.4, 0.4, 0.2, NA, 1))
})

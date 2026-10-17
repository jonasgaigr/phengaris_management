#----------------------------------------------------------#
# Spatial structure: spatial+ (P4.12) -----
#----------------------------------------------------------#

test_that("spatial+ removes the smooth spatial part of a covariate before refitting", {
  set.seed(4)
  n <- 800
  d <- tibble::tibble(
    SITE_X = stats::runif(n, 0, 1e5), SITE_Y = stats::runif(n, 0, 1e5),
    SITMAP = sample(1:20, n, replace = TRUE), YEAR = sample(2019:2021, n, replace = TRUE)
  )
  d$TIMING <- stats::rbinom(n, 1, stats::plogis(d$SITE_X / 2e4 - 2.5))
  d$POSITIVE <- stats::rbinom(n, 1, stats::plogis(-0.5 + 0.8 * d$TIMING))
  m <- fit_tmb("sim", "sim", POSITIVE ~ TIMING + (1 | SITMAP) + (1 | YEAR), d)
  plus <- fit_spatial_plus(m, "TIMING", k = 20)
  expect_equal(plus$column_map, c(TIMING_sp = "TIMING"))
  expect_true(abs(mean(plus$data$TIMING_sp)) < 0.05)
  comparison <- spatial_plus_comparison(m, plus, flag_se = 1)
  expect_equal(comparison$term, "TIMING")
  expect_true(all(c("estimate", "estimate_spatialplus", "shift_in_se", "flag") %in% names(comparison)))
})

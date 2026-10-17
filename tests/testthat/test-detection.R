#----------------------------------------------------------#
# Detection sensitivity (P4.4) -----
#----------------------------------------------------------#

test_that("the known-detection link caps the probability at p", {
  fam <- known_detection_family(0.7)
  expect_equal(fam$linkinv(c(-Inf, 0, Inf)), c(0, 0.35, 0.7))
  expect_equal(fam$linkfun(0.35), 0)
  expect_equal(fam$mu.eta(0), 0.7 * 0.25)
  expect_equal(known_detection_family(1)$linkinv(1.2), stats::plogis(1.2))
})

# Occupancy psi = plogis(-0.5 + 1 * x), detection p = 0.6, one visit each.
simulated_single_visits <- function(n = 3000, p = 0.6, seed = 2) {
  set.seed(seed)
  d <- tibble::tibble(
    x = stats::rnorm(n), YEAR = sample(2019:2022, n, replace = TRUE),
    SITMAP = sample(1:30, n, replace = TRUE), OBSERVER = "a"
  )
  z <- stats::rbinom(n, 1, stats::plogis(-0.5 + d$x))
  d$POSITIVE <- stats::rbinom(n, 1, z * p)
  d
}

test_that("a known-detection refit at the true p recovers the occupancy slope", {
  d <- simulated_single_visits()
  m <- list(id = "sim", sample = "s", formula_object = POSITIVE ~ x + (1 | SITMAP), data = d)
  naive <- fit_known_detection(m, 1)
  true_p <- fit_known_detection(m, 0.6)
  slope <- function(tab) tab$estimate[tab$term == "x"]
  # The naive slope is attenuated; the refit at the true p is not.
  expect_lt(slope(naive), 0.9)
  expect_true(abs(slope(true_p) - 1) < 0.2)
  expect_equal(unique(true_p$status), "ok")
  expect_true(all(c("factor(YEAR)2020", "x") %in% true_p$term))
  # A p below the share of positives cannot be fitted.
  expect_match(unique(fit_known_detection(m, 0.1)$status), "unbounded|not converged|singular")
})

test_that("the occupancy arrays put the visits of a site-year side by side", {
  d <- tibble::tibble(
    SITE_ID = c("A", "A", "B", "C", "C", "C"), YEAR = 2021, ID_LOKAL = 1:6,
    DATUM_OD = c(20210720, 20210701, 20210710, 20210705, 20210712, 20210719),
    POSITIVE = c(1, 0, 1, 0, 0, 1), TIMING = c(1, 1, 0, 1, 1, 0),
    ERA = factor("2021-2024", levels = c("2019-2020", "2021-2024")), DOY_STD = c(1, -1, 0, -0.5, 0.2, 0.9),
    SITMAP = c(1, 1, 2, 3, 3, 3), OBSERVER = c("a", "b", "a", "a", "c", "c")
  )
  a <- occupancy_arrays(d, "TIMING", c("ERA", "DOY_STD"), occ_re = "SITMAP", det_re = "OBSERVER")
  expect_equal(dim(a$y), c(3, 3))
  # Visits in date order: site A was surveyed on 1 July (0) before 20 July (1).
  expect_equal(a$y[1, ], c(0, 1, NA))
  expect_equal(a$det.covs$DOY_STD[1, 1:2], c(-1, 1))
  expect_equal(a$occ.covs$TIMING, c(1, 0, 1))
  expect_equal(a$design$units_visited_twice_or_more, 2)
  expect_equal(a$design$units_with_mixed_outcomes, 2)
  # Site C's visits disagree on TIMING.
  expect_equal(a$design$units_with_conflicting_covariates, 1)
  expect_equal(deparse(a$occ_formula), "~TIMING + (1 | SITMAP)")
  expect_equal(a$det_columns, c("ERA2021_2024", "DOY_STD"))
})

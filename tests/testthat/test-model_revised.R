#----------------------------------------------------------#
# Revised model helpers: fitting, diagnostics, predictions -----
#----------------------------------------------------------#

set.seed(1)
n <- 400
sim <- data.frame(
  TIMING = stats::rbinom(n, 1, 0.5),
  METHOD = stats::rbinom(n, 1, 0.5),
  z = stats::rnorm(n),
  YEAR = sample(2019:2024, n, TRUE),
  SITMAP = sample(1:20, n, TRUE)
)
sim$z_copy <- sim$z + stats::rnorm(n, sd = 0.05)
sim$POSITIVE <- stats::rbinom(n, 1, stats::plogis(-0.5 + 0.8 * sim$TIMING + 0.5 * sim$z))
sim$SEPARATOR <- as.numeric(sim$POSITIVE == 1 & sim$TIMING == 1)

test_that("fit_tmb() fits on complete cases and records failures instead of stopping", {
  d <- sim
  d$z[1:10] <- NA
  m <- fit_tmb("ok", "ok", POSITIVE ~ TIMING + z + (1 | SITMAP), d)
  expect_false(is.null(m$fit))
  expect_equal(m$n_input, n - 10)
  expect_true(tmb_converged(m))

  bad <- fit_tmb("bad", "bad", POSITIVE ~ not_a_column + (1 | SITMAP), d)
  expect_null(bad$fit)
  expect_false(is.na(bad$error))
})

test_that("the coefficient table gives odds ratios with Wald CIs", {
  m <- fit_tmb("m", "m", POSITIVE ~ TIMING + (1 | SITMAP), sim)
  co <- tmb_coef_table(list(m))
  row <- co[co$term == "TIMING", ]
  expect_equal(row$ratio, exp(row$estimate))
  expect_equal(row$ratio_low, exp(row$estimate - stats::qnorm(0.975) * row$std_error))
})

test_that("the GVIF finds collinear terms and ignores interactions", {
  m <- fit_tmb("m", "m", POSITIVE ~ TIMING * METHOD + z + z_copy + (1 | SITMAP), sim)
  g <- gvif_table(m)
  expect_setequal(g$term, c("TIMING", "METHOD", "z", "z_copy"))
  expect_gt(g$GVIF_adjusted[g$term == "z"], 5)
  expect_lt(g$GVIF_adjusted[g$term == "TIMING"], 1.2)
})

test_that("separation is detected and the bias-reduced refit stays finite", {
  m <- fit_tmb("sep", "sep", POSITIVE ~ TIMING + SEPARATOR + (1 | YEAR), sim)
  check <- separation_check(m)
  expect_true(check$separated)
  expect_match(check$terms, "SEPARATOR")
  refit <- bias_reduced_refit(m)
  expect_true(all(is.finite(refit$estimate)))
  expect_true(any(grepl("factor\\(YEAR\\)", refit$term)))
  expect_match(tmb_diagnostics(m)$flags, "possible separation")
})

test_that("marginal predictions of a binomial model stay within 0-1", {
  m <- fit_tmb("m", "m", POSITIVE ~ TIMING * METHOD + z + (1 | SITMAP), sim)
  p <- marginal_predictions(m, by = c("TIMING", "METHOD"))
  expect_equal(nrow(p), 4)
  expect_true(all(p$conf_low >= 0 & p$conf_high <= 1))
  expect_true(all(p$conf_low <= p$estimate & p$estimate <= p$conf_high))
})

test_that("an AIC table refuses models fitted on different records", {
  a <- fit_tmb("a", "a", POSITIVE ~ TIMING + (1 | SITMAP), sim)
  b <- fit_tmb("b", "b", POSITIVE ~ TIMING + (1 | SITMAP), sim[-1, ])
  c <- fit_tmb("c", "c", POSITIVE ~ TIMING + z + (1 | SITMAP), sim)
  tab <- aic_table_tmb(list(a = a, c = c), c("a", "c"))
  expect_equal(tab$delta_AIC[1], 0)
  expect_error(aic_table_tmb(list(a = a, b = b), c("a", "b")))
})

test_that("revised formulas carry the detection covariates and the random effects", {
  f <- revised_formula(terms = c("TIMING * METHOD", "PLANT"), extra_re = "SITE_ID")
  labels <- attr(stats::terms(reformulas::nobars(f)), "term.labels")
  expect_true(all(c("ERA", "DOY_STD", "I(DOY_STD^2)", "TIMING:METHOD") %in% labels))
  bars <- vapply(reformulas::findbars(f), function(b) deparse(b[[3]]), character(1))
  expect_equal(bars, c(unlist(CONFIG$revised_models$random_effects), "SITE_ID"))
  expect_false("ERA" %in% attr(stats::terms(reformulas::nobars(revised_formula(era = FALSE))), "term.labels"))
})

test_that("co-occurrence uses observed records of both species only", {
  d <- tibble::tibble(
    DRUH = c(SPECIES_NAU, SPECIES_TEL, SPECIES_NAU, SPECIES_TEL, SPECIES_NAU),
    ID_LOKAL = c(1, 1, 2, 2, 3), DATUM_OD = 20220720,
    POSITIVE = c(1, 0, 1, 0, 0), IMPUTED = c(FALSE, FALSE, FALSE, TRUE, FALSE)
  )
  out <- cooccurrence_data(d, SPECIES_NAU)
  # Site 2's P. teleius record is imputed, site 3 has no P. teleius record.
  expect_equal(out$ID_LOKAL, 1)
  expect_equal(out$OTHER_POSITIVE, 0)
})

test_that("the spatial test and the smooth alternative run on a glmmTMB and an mgcv fit", {
  d <- sim
  d$SITE_ID <- sprintf("S%03d", sample(1:150, n, TRUE))
  centre <- stats::setNames(stats::runif(150, 0, 50000), sprintf("S%03d", 1:150))
  d$SITE_X <- -700000 + centre[d$SITE_ID]
  d$SITE_Y <- -1100000 + rev(centre)[d$SITE_ID]
  m <- fit_tmb("m", "m", POSITIVE ~ TIMING + z + (1 | SITMAP) + (1 | YEAR), d)
  grid <- spatial_autocorrelation_test(m)
  expect_equal(grid$sites, dplyr::n_distinct(d$SITE_ID))
  expect_true(grid$p_value >= 0 && grid$p_value <= 1)
  smooth <- fit_spatial_smooth(m, k = 10)
  expect_s3_class(smooth, "gam")
  expect_false(is.na(spatial_autocorrelation_test(m, fit = smooth)$p_value))
  expect_true("TIMING" %in% gam_coef_table(smooth, "m", NA)$term)
})

test_that("count predictions are expected counts given presence, with positive CIs", {
  d <- sim
  d$POCET <- 1 + stats::rnbinom(n, mu = exp(0.5 + 0.7 * d$TIMING), size = 1.2)
  m <- fit_tmb("count", "count", POCET ~ TIMING + (1 | SITMAP), d, family = glmmTMB::truncated_nbinom2())
  p <- marginal_predictions(m, by = "TIMING")
  expect_true(all(p$conf_low > 1))
  expect_gt(p$estimate[p$TIMING == 1], p$estimate[p$TIMING == 0])
})

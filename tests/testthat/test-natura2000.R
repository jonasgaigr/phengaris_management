#----------------------------------------------------------#
# Natura 2000: matching (P4.8) -----
#----------------------------------------------------------#

test_that("matching balances the covariates of a designation that depends on them", {
  set.seed(3)
  n <- 600
  d <- tibble::tibble(cov = stats::rnorm(n), ERA = factor(sample(c("a", "b"), n, replace = TRUE)))
  d$EVL_species <- stats::rbinom(n, 1, stats::plogis(-1.5 + 1.5 * d$cov))
  m <- match_designation(d, "cov", exact = "ERA", method = "full")
  bal <- balance_table(m)
  row <- bal[bal$covariate == "cov", ]
  expect_gt(abs(row$smd_before), 0.5)
  expect_lt(abs(row$smd_after), 0.1)
  expect_equal(sum(matched_sizes(m)$treated[matched_sizes(m)$sample == "All"]), sum(d$EVL_species))
})

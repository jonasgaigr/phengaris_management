#----------------------------------------------------------#
# Synthesis: the robustness of the key estimates (step 25) -----
#----------------------------------------------------------#

test_that("parse_ratio_ci reads formatted odds ratios back into numbers", {
  parsed <- parse_ratio_ci(c("2.16 (1.48-3.15)", "0.78 (NaN-NaN)", "1.00 (0.00-Inf)", "", NA))
  expect_equal(parsed$odds_ratio, c(2.16, 0.78, 1, NA, NA))
  expect_equal(parsed$or_low, c(1.48, NaN, 0, NA, NA))
  expect_equal(parsed$or_high, c(3.15, NaN, Inf, NA, NA))
})

test_that("summarise_robustness leaves separated fits out of the range and the direction", {
  robustness <- data.frame(
    analysis = c("a", "b", "c", "a", "b"),
    sample = "s",
    term = c("TIMING", "TIMING", "TIMING", "METHOD", "METHOD"),
    term_label = c("t", "t", "t", "m", "m"),
    odds_ratio = c(2, 1.5, 1e20, 0.8, 1e-9),
    or_low = c(1.2, 0.9, 1e-5, 0.6, 1e-30),
    or_high = c(3.3, 2.5, Inf, 1.1, 1e10)
  )
  expect_equal(is_estimable(robustness, 1e4), c(TRUE, TRUE, FALSE, TRUE, FALSE))

  s <- summarise_robustness(robustness, max_interval_ratio = 1e4)
  timing <- s[s$term == "TIMING", ]
  expect_equal(timing$analyses, 2)
  expect_equal(timing$not_estimable, 1)
  expect_equal(c(timing$min_odds_ratio, timing$max_odds_ratio), c(1.5, 2))
  expect_equal(timing$ci_excluding_1, 1)
  expect_equal(timing$direction, "always above 1")
  expect_equal(s$direction[s$term == "METHOD"], "always below 1")

  all_separated <- robustness[c(3, 5), ]
  expect_equal(unique(summarise_robustness(all_separated, 1e4)$direction), "not estimable")
})

#----------------------------------------------------------#
# Threats: Article 17 codes, stacking, multiplicity (P4.11) -----
#----------------------------------------------------------#

test_that("threat p-values are adjusted by family", {
  p <- c(0.01, 0.02, 0.03, 0.04)
  adj <- adjust_threat_p(p, c("A", "B", "C", "D"), confirmatory = c("A", "B"))
  expect_equal(adj$adjusted[1:2], stats::p.adjust(p[1:2], "holm"))
  expect_equal(adj$adjusted[3:4], stats::p.adjust(p[3:4], "BH"))
  expect_equal(adj$method, c("holm", "holm", "BH", "BH"))
  expect_equal(article17_code(c("X", "Y", "Z"), codes = list(X = "A06", Y = "L02")), c("A06", "L02", ""))
})

test_that("threats are stacked one row per record and class", {
  d <- tibble::tibble(
    ID_RECORD = c("r1", "r2", "r3"), SUM_THREATS = c(1, 0, NA), POSITIVE = c(1, 0, 1),
    ERA = factor(c("a", "b", "a")), SITMAP = 1, OBSERVER = "o",
    Abandonment = c(1, 0, NA), HarmfulMow = c(0, 0, NA)
  )
  s <- stack_threats(d, c("Abandonment", "HarmfulMow"))
  expect_equal(nrow(s), 4)
  expect_equal(levels(s$CLASS), c("Abandonment", "HarmfulMow"))
  expect_equal(s$THREAT, c(1, 0, 0, 0))
  expect_equal(s$NEWER_FORM, c(0, 1, 0, 1))
})

test_that("a class off the threat list of an era is NA there, not 0", {
  d <- tibble::tibble(
    PROTOCOL_ERA = rep(c("2019-2020", "2021-2024"), each = 6),
    SUM_THREATS = c(rep(1, 5), NA, rep(1, 6)),
    Abandonment = c(rep(0, 5), NA, 1, 1, 1, 1, 1, 0),
    HarmfulMow = c(1, 1, 1, 1, 1, NA, 1, 0, 1, 1, 1, 1)
  )
  out <- mask_unlisted_threats(d, classes = c("Abandonment", "HarmfulMow"), min_records = 5)
  expect_true(all(is.na(out$data$Abandonment[1:6])))
  expect_equal(out$data$Abandonment[7:12], c(1, 1, 1, 1, 1, 0))
  expect_equal(out$data$HarmfulMow, d$HarmfulMow)
  expect_equal(out$masked$class, "Abandonment")
  expect_equal(out$masked$records_masked, 5)
})

test_that("a class unknown in a year is NA in that year only", {
  d <- tibble::tibble(
    YEAR = c(2020, 2021, 2021, 2021, 2022),
    SUM_THREATS = c(1, 1, 2, NA, 1),
    HarmfulGrazing = c(0, 1, 1, NA, 0),
    HarmfulMow = c(1, 0, 1, NA, 1)
  )
  out <- mask_threat_years(d, list(HarmfulGrazing = 2021))
  expect_equal(out$data$HarmfulGrazing, c(0, NA, NA, NA, 0))
  expect_equal(out$data$HarmfulMow, d$HarmfulMow)
  expect_equal(out$masked$class, "HarmfulGrazing")
  expect_equal(out$masked$records_with_threat, 2)
  expect_equal(out$masked$records_masked, 2)
})

test_that("without a list of unknown years nothing is masked", {
  d <- tibble::tibble(YEAR = 2021, SUM_THREATS = 1, HarmfulGrazing = 1)
  out <- mask_threat_years(d, NULL)
  expect_equal(out$data, d)
  expect_equal(nrow(out$masked), 0)
})

test_that("the classes unknown in a year in config.yml are threat classes", {
  expect_true(all(names(CONFIG$strukt_pozn$threat_unknown_years) %in% names(CONFIG$strukt_pozn$threat_items)))
})

test_that("the stacked model gives an era term only to the classes on both forms", {
  set.seed(5)
  n <- 300
  d <- tibble::tibble(
    ID_RECORD = paste0("r", seq_len(n)), SUM_THREATS = 1, POSITIVE = stats::rbinom(n, 1, 0.4),
    ERA = factor(sample(c("a", "b"), n, replace = TRUE)), SITMAP = sample(1:10, n, replace = TRUE),
    OBSERVER = sample(c("o1", "o2", "o3"), n, replace = TRUE)
  )
  d$HarmfulMow <- stats::rbinom(n, 1, 0.3)
  d$Abandonment <- ifelse(d$ERA == "a", NA, stats::rbinom(n, 1, 0.3))
  s <- stack_threats(d, c("HarmfulMow", "Abandonment"))
  expect_equal(nrow(s), n + sum(d$ERA == "b"))
  m <- fit_stacked_threats(s, "stacked", "with imputed")
  terms <- rownames(summary(m$fit)$coefficients$cond)
  expect_true("ERA_HarmfulMow" %in% terms)
  expect_false("ERA_Abandonment" %in% terms)
  expect_true(all(c("CLASSHarmfulMow:POSITIVE", "CLASSAbandonment:POSITIVE") %in% terms))
})

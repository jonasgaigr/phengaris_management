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

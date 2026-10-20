#----------------------------------------------------------#
# The main findings (step 26) -----
#----------------------------------------------------------#

test_that("primary_sample reads the sample of each species from config.yml", {
  expect_equal(species_key(SPECIES_TEL), "teleius")
  expect_equal(primary_sample(SPECIES_NAU), REVISED_SAMPLES[[CONFIG$synthesis$primary_sample$nausithous]])
  expect_equal(primary_sample(SPECIES_TEL), REVISED_SAMPLES[[CONFIG$synthesis$primary_sample$teleius]])
  expect_error(species_key("Phengaris arion"), "Unknown species")
})

test_that("landscape_term_label labels the landscape variables only", {
  expect_equal(
    landscape_term_label(c("FOREST_8000", "GRASSLAND_1000", "SANGUISORBA_10", "TIMING", "FOREST_7")),
    c("forest within 8 km (per 10 pp)", "grassland within 1 km (per 10 pp)", NA, NA, NA)
  )
})

test_that("format_p writes small p-values and the adjustment", {
  expect_equal(
    format_p(c(0.0001, 0.0123, 0.03, NA), c("Wald", "Wald", "holm", "BH")),
    c("< 0.001", "0.012", "0.030 (Holm)", "")
  )
  expect_equal(format_p(c(0.2, 0.5)), c("0.200", "0.500"))
})

test_that("label_with_ratio moves the unit inside the parentheses", {
  expect_equal(
    label_with_ratio(c("site area (per tenfold)", "host plant abundant"), c(0.41, 2.93)),
    c("site area (0.41 per tenfold)", "host plant abundant (2.93)")
  )
  expect_equal(plain_label("forest within 8 km (per 10 pp)"), "forest within 8 km")
})

#' Rows as collect_findings() returns them.
findings_rows <- function() {
  rows <- data.frame(
    term = rep(c("TIMING", "METHOD", "TIMING:METHOD", "LOG_CONN"), each = 3),
    label = rep(c("mowing timed appropriately", "patches left uncut", "timing x patches", "connectivity"), each = 3),
    group = rep(c("management", "management", "management", "populations"), each = 3),
    source = "s", model = "m",
    sample = rep(unname(REVISED_SAMPLES), 4),
    odds_ratio = c(2, 2.2, 1e9, 2, 1.5, 1, 0.5, 0.6, 1, 2, 2, 2),
    or_low = c(1.4, 1.5, 1e-3, 1.3, 0.9, 0.5, 0.3, 0.35, 0.5, 1.7, 1.7, 1.7),
    or_high = c(3, 3.2, 1e21, 3.5, 2.5, 2, 0.8, 0.95, 2, 2.3, 2.3, 2.3),
    p_value = c(0.001, 0.001, 0.01, 0.002, 0.1, 0.9, 0.006, 0.03, 0.9, 1e-10, 1e-10, 1e-10),
    p_basis = "Wald", separated = FALSE,
    stringsAsFactors = FALSE
  )
  rows$estimable <- is_estimable(rows)
  rows$significant <- rows$estimable & rows$p_value < 0.05
  rows
}

test_that("summarise_findings reads the primary sample and counts samples and analyses", {
  findings <- findings_rows()
  robustness <- data.frame(
    sample = REVISED_SAMPLES[["plausible"]], term = c("TIMING", "METHOD"), analyses = 10L, ci_excluding_1 = c(10L, 6L)
  )
  fits <- data.frame(
    term = "LOG_CONN", sample = REVISED_SAMPLES[["plausible"]], model = c("a", "b"),
    odds_ratio = c(2, 1.2), or_low = c(1.5, 0.8), or_high = c(2.5, 1.8)
  )
  s <- summarise_findings(findings, REVISED_SAMPLES[["plausible"]], robustness, fits)
  expect_equal(nrow(s), 4)
  expect_true(all(s$sample == REVISED_SAMPLES[["plausible"]]))
  # TIMING is separated in the observed sample: 2 of 2 estimable samples.
  timing <- s[s$term == "TIMING", ]
  expect_equal(c(timing$samples_significant, timing$samples_estimable), c(2, 2))
  expect_equal(c(timing$analyses_excluding_1, timing$analyses), c(10, 10))
  expect_equal(timing$analyses_basis, "step 25")
  method <- s[s$term == "METHOD", ]
  expect_false(method$significant)
  expect_equal(method$samples_significant, 1)
  conn <- s[s$term == "LOG_CONN", ]
  expect_equal(c(conn$analyses_excluding_1, conn$analyses), c(1, 2))
  expect_equal(conn$analyses_basis, "steps 17, 22")

  words <- findings_in_words(s)
  expect_equal(words$more, c("mowing timed appropriately (2.20)", "connectivity (2.00)"))
  expect_length(words$less, 0)
  # Both together: 2.2 x 1.5 x 0.6.
  expect_match(findings_interaction(s), "both together 1.98 \\(interaction 0.60\\)")
})

test_that("summarise_findings falls back on the only sample a variable is fitted on", {
  findings <- findings_rows()
  findings <- findings[findings$sample == REVISED_SAMPLES[["observed"]] & findings$term == "LOG_CONN", ]
  s <- summarise_findings(findings, REVISED_SAMPLES[["with_imputed"]])
  expect_equal(s$sample, REVISED_SAMPLES[["observed"]])
  expect_true(is.na(s$analyses))
  expect_equal(findings_table(s)$intervals_excluding_1, "")
})

test_that("plot_findings builds and leaves out the fits that are not estimable", {
  findings <- findings_rows()
  expect_equal(nrow(findings_shown(findings)), 11)
  expect_s3_class(ggplot2::ggplot_build(plot_findings(findings, "P. teleius")), "ggplot_built")
})

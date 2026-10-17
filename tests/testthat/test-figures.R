#----------------------------------------------------------#
# Figures of the revised analysis and the summaries they plot -----
#----------------------------------------------------------#

test_that("format_tick writes plain tick labels", {
  expect_equal(format_tick(c(0.3, 1, 10, 0.01, 2.5, NA)), c("0.3", "1", "10", "0.01", "2.5", NA))
})

test_that("log_breaks thins the breaks as the range widens", {
  expect_equal(log_breaks(c(0.5, 3)), c(0.5, 1, 2))
  expect_equal(log_breaks(c(0.1, 10)), c(0.1, 0.3, 1, 3, 10))
  expect_equal(log_breaks(c(0.01, 100)), c(0.01, 0.1, 1, 10, 100))
})

test_that("threat_label adds the Article 17 code and keeps unknown classes", {
  expect_equal(threat_label("FertilizerUse"), "fertiliser use (A19, A20)")
  expect_equal(threat_label("Unlisted"), "Unlisted")
})

test_that("model_term reads the posterior's term names back", {
  expect_equal(
    model_term(c("TIMING_METHOD", "TIMING", "PLANTabundant", "other")),
    c("TIMING:METHOD", "TIMING", "PLANTabundant", "other")
  )
})

test_that("record_outcome separates observed and imputed negatives", {
  outcome <- record_outcome(
    positive = c(1, 0, 0, 0, 0),
    imputed = c(FALSE, FALSE, TRUE, TRUE, TRUE),
    plausible = c(FALSE, FALSE, TRUE, FALSE, NA)
  )
  expect_equal(as.character(outcome), RECORD_OUTCOMES[c(1, 2, 3, 4, 4)])
  expect_equal(levels(outcome), RECORD_OUTCOMES)
})

test_that("jeffreys_interval is bounded by 0 and 1 and brackets the share", {
  ci <- jeffreys_interval(c(0, 5, 10), c(10, 10, 10))
  expect_equal(ci$low[1], 0)
  expect_equal(ci$high[3], 1)
  expect_true(ci$low[2] < 0.5 && ci$high[2] > 0.5)
})

#' Records as revised_model_data() expects them.
revised_records <- function() {
  data.frame(
    DRUH = rep(c(SPECIES_NAU, SPECIES_TEL), each = 6),
    YEAR = 2021,
    POSITIVE = c(1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0),
    IMPUTED = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE),
    IMPUTED_PLAUSIBLE = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    IN_FLIGHT_WINDOW = c(rep(TRUE, 11), FALSE),
    DOY_DEV = c(-3, 0, 4, 4, 9, 12, -1, 2, 2, 8, 8, 60),
    PROTOCOL_ERA = "2021-2024",
    TIMING = c(1, 1, 0, 0, NA, 1, 1, 0, 0, 1, NA, 1),
    METHOD = c(1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0),
    PLANT_QUANT = c(3, 2, 1, 1, 1, 2, 2, 1, 1, 1, 3, 1),
    EVL_species = c(1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0),
    SITE_AREA_HA = 1
  )
}

test_that("summarise_records_by_outcome counts the in-season records", {
  tab <- summarise_records_by_outcome(revised_records())
  expect_equal(sum(tab$records), 11)
  tel <- tab[tab$DRUH == SPECIES_TEL, ]
  expect_equal(tel$records[tel$outcome == "imputed negative, not plausible"], 2)
})

test_that("summarise_share_positive follows the samples of the models", {
  share <- summarise_share_positive(revised_records())
  timing <- share[share$DRUH == SPECIES_NAU & share$variable == "TIMING", ]
  # With imputed: appropriate 3 records, 3 positive; harmful 2 records, 0 positive.
  with_imputed <- timing[timing$sample == REVISED_SAMPLES[["with_imputed"]], ]
  expect_equal(with_imputed$records[with_imputed$value == "appropriate"], 3)
  expect_equal(with_imputed$share_positive[with_imputed$value == "harmful"], 0)
  # Observed only drops the imputed absence with harmful timing.
  observed <- timing[timing$sample == REVISED_SAMPLES[["observed"]], ]
  expect_equal(observed$records[observed$value == "harmful"], 1)
})

test_that("the plot functions build from tables of their steps' layout", {
  robustness <- data.frame(
    analysis = rep(c("management model (step 17)", "+ connectivity"), each = 2),
    sample = REVISED_SAMPLES[["with_imputed"]],
    term = c("TIMING", "EVL_species"),
    odds_ratio = c(2, 1.2, 1.8, 1e20), or_low = c(1.2, 0.7, 1.1, 1e-5), or_high = c(3.3, 2.1, 2.9, Inf)
  )
  expect_s3_class(ggplot2::ggplot_build(plot_robustness(robustness)), "ggplot_built")

  predictions <- data.frame(
    model = "nau_management", sample = REVISED_SAMPLES[["with_imputed"]],
    TIMING = c(0, 0, 1, 1), METHOD = c(0, 1, 0, 1),
    estimate = c(0.3, 0.45, 0.45, 0.46), conf_low = 0.2, conf_high = 0.6
  )
  other <- data.frame(
    model = "nau_management", sample = REVISED_SAMPLES[["with_imputed"]],
    PLANT = c("single", "abundant", "dominant", NA, NA), EVL_species = c(NA, NA, NA, 0, 1),
    estimate = 0.4, conf_low = 0.3, conf_high = 0.5
  )
  expect_s3_class(ggplot2::ggplot_build(plot_predictions(predictions, other, "P. nausithous")), "ggplot_built")

  threats <- data.frame(
    species = SPECIES_NAU, sample = unname(REVISED_SAMPLES),
    term = "FertilizerUse", ratio = 0.35, ratio_low = 0.17, ratio_high = 0.74, p_adjusted = c(0.01, 0.2, NA)
  )
  expect_s3_class(ggplot2::ggplot_build(plot_threat_estimates(threats)), "ggplot_built")

  curve <- data.frame(
    DRUH = SPECIES_TEL, ELEVATION = NA, DOY_DEV = -45:45,
    RELATIVE_ACTIVITY = stats::dnorm(-45:45, 0, 15) / stats::dnorm(0, 0, 15),
    IN_DATA_RANGE = abs(-45:45) <= 25
  )
  expect_s3_class(ggplot2::ggplot_build(plot_flight_curve(curve, 0.05)), "ggplot_built")
})

#----------------------------------------------------------#
# Survey-design variables of the revised analysis -----
#----------------------------------------------------------#

test_that("the day of year comes from the YYYYMMDD number", {
  expect_equal(day_of_year(c(20240101, 20241231, 20230725)), c(1L, 366L, 206L))
})

test_that("the protocol era splits at the year of the form change", {
  expect_equal(protocol_era(c(2019, 2020, 2021, 2024), change_year = 2021), c(
    "2019-2020", "2019-2020", "2021-2024", "2021-2024"
  ))
})

test_that("observer spellings are merged", {
  x <- c(
    "Jiří Švarc", "Švarc Jiří", "Jiří  Švarc", "jiri.svarc",
    "Petr Mückstein RNDr.", "Mückstein Petr", "Tájek Přemysl, Tájková Pavla",
    "Valda Slávek", "Slavomír Valda", NA
  )
  out <- normalise_observer(x, aliases = list("slavek valda" = "slavomir valda"))
  expect_equal(unique(out[1:4]), "jiri svarc")
  expect_equal(out[5], out[6])
  expect_equal(out[7], "premysl tajek")
  expect_equal(out[8], out[9])
  expect_true(is.na(out[10]))
})

records <- tibble::tibble(
  DRUH = c(SPECIES_NAU, SPECIES_NAU, SPECIES_NAU, SPECIES_NAU, SPECIES_TEL),
  DATUM_OD = c(20220720, 20220730, 20220801, 20220801, 20220715),
  DATUM_DO = c(20220720, 20220730, 20220805, 20220801, 20220715),
  NEGATIV = c(0, 0, 0, 1, 0),
  ZDROJ = c("a", "b", "a", "a", "a")
)

test_that("the flight season uses positive records with an exact date", {
  fw <- flight_window_reference(records, years = 2022)
  nau <- fw[fw$DRUH == SPECIES_NAU, ]
  # 20220801-05 has no exact date and 20220801 is negative.
  expect_equal(nau$positive_records, 2)
  expect_equal(nau$median_doy, stats::median(day_of_year(c(20220720, 20220730))))
  expect_equal(flight_window_reference(records, sources = "a", years = 2022)$positive_records, c(1, 1))
})

test_that("collapse_to_visits keeps one record per species, site and date, positive first", {
  d <- tibble::tibble(
    DRUH = SPECIES_NAU, ID_LOKAL = c(1, 1, 1, 2), DATUM_OD = c(20220720, 20220720, 20220721, 20220720),
    POSITIVE = c(0, 1, 0, 0), IMPUTED = FALSE, ID_NALEZ = c(10, 11, 12, 13)
  )
  out <- collapse_to_visits(d)
  expect_equal(out$ID_NALEZ, c(11, 12, 13))
})

test_that("the host-plant filter drops what its rule says", {
  d <- tibble::tibble(PLANT_QUAL = c(1, 0, NA))
  expect_equal(nrow(filter_host_plant(d, "absent_or_unknown")), 1)
  expect_equal(filter_host_plant(d, "absent")$PLANT_QUAL, c(1, NA))
  expect_equal(nrow(filter_host_plant(d, "none")), 3)
})

test_that("add_design_variables places each visit in its season and flags protection for its own species", {
  data <- tibble::tibble(
    ID_LOKAL = c(1, 2), DRUH = c(SPECIES_NAU, SPECIES_TEL), YEAR = 2022L,
    DATUM_OD = c(20220725, 20221020), AUTOR = c("Švarc Jiří", "jiri.svarc")
  )
  fw <- tibble::tibble(DRUH = c(SPECIES_NAU, SPECIES_TEL), YEAR = 2022L, median_doy = c(206, 200))
  sd <- tibble::tibble(ID_LOKAL = c(1, 2), SITE_ID = c("S1", "S2"))
  conn <- tibble::tibble(ID_LOKAL = c(1, 2), YEAR = 2022L, DRUH = c(SPECIES_NAU, SPECIES_TEL), CONN = c(0.5, 0))
  pa <- tibble::tibble(ID_LOKAL = c(1, 2), PA_TYPE = "EVL_target", PA_SPECIES = c(SPECIES_TEL, SPECIES_TEL))
  fixed <- list(cutoff = "fixed", max_abs_deviation_days = 45, min_relative_activity = 0.05, sensitivity_days = 28)
  out <- add_design_variables(data, fw, sd, conn, pa, window = fixed)
  expect_equal(out$DOY_DEV, c(0, day_of_year(20221020) - 200))
  expect_equal(out$IN_FLIGHT_WINDOW, c(TRUE, FALSE))
  expect_equal(out$IN_WINDOW_28, c(TRUE, FALSE))
  expect_true(all(is.na(out$FLIGHT_ACTIVITY)))
  # Site 1 lies in an EVL designated for P. teleius only.
  expect_equal(out$EVL_species, c(0, 1))
  expect_equal(out$OBSERVER[1], out$OBSERVER[2])
  expect_equal(out$CONN, c(0.5, 0))

  # The flight curve cuts the season where the relative activity is low.
  curve <- tibble::tibble(
    DRUH = rep(c(SPECIES_NAU, SPECIES_TEL), each = 3), ELEVATION = NA_real_,
    DOY_DEV = rep(c(-1L, 0L, 1L), 2), RELATIVE_ACTIVITY = c(0.5, 1, 0.5, 0.01, 0.02, 0.04)
  )
  data$DATUM_OD <- c(20220725, 20220720)
  by_curve <- add_design_variables(
    data, fw, sd, conn, pa,
    flight_curve = curve, window = utils::modifyList(fixed, list(cutoff = "flight_curve"))
  )
  expect_equal(by_curve$FLIGHT_ACTIVITY, c(1, 0.04))
  expect_equal(by_curve$LOG_ACTIVITY, log(c(1, 0.04)))
  expect_equal(by_curve$IN_FLIGHT_WINDOW, c(TRUE, FALSE))
  expect_error(add_design_variables(data, fw, sd, conn, pa, window = list(cutoff = "flight_curve")), "no flight curve")

  # An imputed absence is plausible near a positive record of its species.
  data$IMPUTED <- c(FALSE, TRUE)
  history <- tibble::tibble(
    ID_LOKAL = c(1, 2), DRUH = c(SPECIES_NAU, SPECIES_TEL), NEAREST_POSITIVE_KM = c(0, 2.5),
    HIST_POSITIVE = c(1, 0), HIST_LAST_YEAR = c(2016L, NA)
  )
  with_history <- add_design_variables(data, fw, sd, conn, pa, site_history = history, window = fixed, plausible_km = 3)
  expect_equal(with_history$IMPUTED_PLAUSIBLE, c(FALSE, TRUE))
  stricter <- add_design_variables(data, fw, sd, conn, pa, site_history = history, window = fixed, plausible_km = 2)
  expect_equal(stricter$IMPUTED_PLAUSIBLE, c(FALSE, FALSE))
  expect_equal(with_history$HIST_POSITIVE, c(1, 0))
})

test_that("the flight season is cut by the curve and by the outer window", {
  window <- list(cutoff = "flight_curve", max_abs_deviation_days = 45, min_relative_activity = 0.05)
  expect_equal(
    in_flight_season(c(0, 10, 50, NA, 5), c(1, 0.04, 0.5, 1, NA), window),
    c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  window$cutoff <- "fixed"
  expect_equal(in_flight_season(c(0, 10, 50, NA), c(1, 0.04, 0.5, 1), window), c(TRUE, TRUE, FALSE, FALSE))
})

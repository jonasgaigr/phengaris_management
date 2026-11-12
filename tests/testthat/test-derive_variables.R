#----------------------------------------------------------#
# derive_analysis_variables(): occupancy, management, threats, habitat -----
#----------------------------------------------------------#

records <- tibble::tibble(
  ID_NALEZ = c(1, 2, 3, 4, 5),
  DRUH = c(SPECIES_NAU, SPECIES_NAU, SPECIES_TEL, SPECIES_TEL, SPECIES_NAU),
  ID_LOKAL = c(11, 12, 13, 14, 15),
  NEGATIV = c(0, 1, 0, 1, 0),
  DATUM_OD = c(20200710, 20220710, 20220712, 20230715, 20240720),
  SITMAP = c(6852, 6852, 5360, 5360, 6852),
  POP_BIOT = c(
    "TTP s pravidelným managementem, příkop",
    "příkop u komunikace",
    "zarůstající louka bez managementu",
    "okraj vodní plochy",
    NA
  ),
  STRUKT_POZN = c(
    # 2020: no grazing entry on the form yet
    note(
      "sta_pritomnostrostlin: hojně", "Seč vhodně načasovaná: ano", "Seč celoplošná: ne",
      "Negativní vlivy na lokalitě: sukcese", "Perspektiva lokality: dobrá"
    ),
    "Pastva: ne, vliv: žádný vliv, Perspektiva lokality: dobrá, Přítomnost rostlin: jednotlivě",
    note(
      "Pastva: extenzivní", "Seč vhodně načasovaná: ne", "vliv: nevhodná seč (vysoká intenzita, termín apod.)",
      "sukcese", "Přítomnost rostlin: dominantně"
    ),
    "Perspektiva lokality: neznámá, Přítomnost rostlin: žádné",
    NA
  )
)

protected <- tibble::tibble(
  ID_LOKAL = c(11, 13, 13, 12),
  DRUH = c(SPECIES_NAU, SPECIES_TEL, SPECIES_TEL, SPECIES_NAU),
  PA_TYPE = c("EVL_any", "EVL_any", "EVL_target", "MZCHU"),
  PA_SPECIES = c(NA, NA, SPECIES_TEL, NA)
)
both_present <- tibble::tibble(ID_NALEZ = 1)

d <- derive_analysis_variables(records, protected, both_present)

test_that("occupancy, year and protection are derived", {
  expect_equal(d$POSITIVE, c(1, 0, 1, 0, 1))
  expect_equal(d$YEAR, c(2020L, 2022L, 2022L, 2023L, 2024L))
  expect_equal(d$EVL, c(1, 0, 1, 0, 0))
  expect_equal(d$EVL_target, c(0, 0, 1, 0, 0))
  expect_equal(d$MZCHU, c(0, 1, 0, 0, 0))
  expect_equal(d$PROTECT, c(1, 1, 1, 0, 0))
  expect_equal(d$SPEC_NUM, c(1, 0, 0, 0, 0))
})

test_that("host plant abundance comes from either host-plant key", {
  expect_equal(d$PLANT_QUANT, c(2, 1, 3, 0, NA))
  expect_equal(d$PLANT_QUAL, c(1, 1, 1, 0, NA))
})

test_that("mowing timing and method come from their own keys", {
  expect_equal(d$TIMING, c(1, NA, 0, NA, NA))
  expect_equal(d$METHOD, c(1, NA, NA, NA, NA))
  expect_equal(d$MOW, c(1, 0, 1, 0, 0))
  # Unknown method or timing leaves OVERALL unknown.
  expect_equal(d$OVERALL, c(1, NA, NA, NA, NA))
})

test_that("grazing is NA before the form had a grazing entry, and 'ne' is not grazed", {
  # 2020: NA; 'Pastva: ne' -> 0; 'Pastva: extenzivní' -> 1; no entry from 2021 on -> 0.
  expect_equal(d$GRAZE, c(NA, 0, 1, 0, 0))
  expect_equal(d$GRAZE_MET, c(NA, NA, 1, NA, NA))
})

test_that("threats are NA without a threat list, and 'žádný vliv' is None, not a threat", {
  expect_equal(d$Encroachment, c(1, 0, 1, NA, NA))
  expect_equal(d$HarmfulMow, c(0, 0, 1, NA, NA))
  expect_equal(d$None, c(0, 1, 0, NA, NA))
  expect_equal(d$SUM_THREATS, c(1, 0, 2, NA, NA))
})

test_that("habitat flags match both ditch categories and stay NA without a description", {
  expect_equal(d$TTP, c(1, 0, 0, 0, NA))
  expect_equal(d$PRIKOP, c(1, 1, 0, 0, NA))
  expect_equal(d$ZARUST, c(0, 0, 1, 0, NA))
  # 'okraj vodní plochy' is a habitat type of its own, so HET_INN is 1.
  expect_equal(d$VODA, c(0, 0, 0, 1, NA))
  expect_equal(d$PASTVINA, c(0, 0, 0, 0, NA))
  expect_equal(d$HET_INN, c(2, 1, 1, 1, NA))
})

test_that("only records with the host plant present are kept", {
  kept <- keep_host_plant_present(d)
  expect_equal(kept$ID_NALEZ, c(1, 2, 3))
})

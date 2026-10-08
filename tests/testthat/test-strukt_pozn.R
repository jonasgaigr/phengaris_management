#----------------------------------------------------------#
# parse_strukt_pozn() and threat_class_flags() -----
#----------------------------------------------------------#

# Real STRUKT_POZN strings, with the edge cases found in the exports.
notes <- c(
  # the usual form, 2021 onwards
  standard = "Seč vhodně načasovaná: ano, Seč celoplošná: ne, vliv: eutrofizace či okyselování, Perspektiva lokality: dobrá, Přítomnost rostlin: hojně",
  # 2019-2020: the host plant comes first, under another key
  old_plant_key = "sta_pritomnostrostlin: jednotlivě, Seč vhodně načasovaná: ne, Seč celoplošná: ano, Negativní vlivy na lokalitě: sečení, sukcese všeobecně, Perspektiva lokality: špatná",
  # threat items with commas inside and outside parentheses
  commas = "vliv: nevhodná seč (vysoká intenzita, termín apod.), hromadění organického materiálu, Perspektiva lokality: neznámá, Přítomnost rostlin: žádné",
  # free text under 'jiný vliv', itself containing commas
  free_text = "Negativní vlivy na lokalitě: sukcese, jiný vliv: extenzivne udrzovany okraj, do budoucna ohrozen, Perspektiva lokality: dobrá, Přítomnost rostlin: dominantně",
  # grazing, including the explicit 'no'
  grazing_no = "Pastva: ne, vliv: žádný vliv, Perspektiva lokality: dobrá, Přítomnost rostlin: hojně",
  grazing = "Pastva: extenzivní, vliv: žádný vliv, Perspektiva lokality: neznámá, Přítomnost rostlin: jednotlivě",
  # no known key at all
  no_key = "lokalita pod dráty vysokého napětí",
  missing = NA
)

test_that("each value runs from its key to the next key, commas and all", {
  p <- parse_strukt_pozn(notes)
  expect_equal(nrow(p), length(notes))
  expect_equal(p$mow_timing[1], "ano")
  expect_equal(p$mow_whole_area[1], "ne")
  expect_equal(p$threats[1], "eutrofizace či okyselování")
  expect_equal(p$prospects[1], "dobrá")
  expect_equal(p$host_plant[1], "hojně")
  expect_equal(p$threats[3], "nevhodná seč (vysoká intenzita, termín apod.), hromadění organického materiálu")
})

test_that("both host-plant keys and both threat lists feed one column", {
  p <- parse_strukt_pozn(notes)
  expect_equal(p$host_plant[2], "jednotlivě")
  expect_equal(p$threats[2], "sečení, sukcese všeobecně")
})

test_that("free text under 'jiný vliv' is kept apart from the threat list", {
  p <- parse_strukt_pozn(notes)
  expect_equal(p$threats[4], "sukcese")
  expect_equal(p$other_threat[4], "extenzivne udrzovany okraj, do budoucna ohrozen")
  expect_equal(p$host_plant[4], "dominantně")
})

test_that("a key is only recognised at the start or after ', '", {
  # 'vliv' must not match inside 'jiný vliv' or 'Negativní vlivy na lokalitě'.
  p <- parse_strukt_pozn(notes)
  expect_false(grepl("jiný", p$threats[4]))
  expect_true(is.na(p$other_threat[1]))
})

test_that("text without any known key, and missing notes, are handled", {
  p <- parse_strukt_pozn(notes)
  expect_equal(p$unparsed[7], "lokalita pod dráty vysokého napětí")
  expect_true(all(is.na(p[8, ])))
})

test_that("threat classes are flagged from the threat list only", {
  p <- parse_strukt_pozn(notes)
  classes <- list(
    HarmfulMow = "nevhodná seč",
    Encroachment = "sukcese",
    Eutrophization = "eutrofizace či okyselování"
  )
  flags <- threat_class_flags(p$threats, classes, missing = NA)
  expect_equal(flags$HarmfulMow, c(0, 0, 1, 0, 0, 0, NA, NA))
  expect_equal(flags$Encroachment, c(0, 1, 0, 1, 0, 0, NA, NA))
  expect_equal(flags$Eutrophization, c(1, 0, 0, 0, 0, 0, NA, NA))
  # The original coding: no threat list counts as 0.
  expect_equal(threat_class_flags(p$threats, classes)$HarmfulMow[7:8], c(0, 0))
})

test_that("the legacy threat phrases in config.yml match the newer threat list", {
  p <- parse_strukt_pozn(notes)
  flags <- threat_class_flags(p$threats, CONFIG$strukt_pozn$threat_classes)
  expect_equal(flags$BiomassAccumulation[3], 1)
  expect_equal(flags$HarmfulMow[3], 1)
})

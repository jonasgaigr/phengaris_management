#----------------------------------------------------------#
# parse_strukt_pozn() and threat_class_flags() -----
#----------------------------------------------------------#

# Real STRUKT_POZN strings, with the edge cases found in the exports.
notes <- c(
  # the usual form, 2021 onwards
  standard = note(
    "Seč vhodně načasovaná: ano", "Seč celoplošná: ne", "vliv: eutrofizace či okyselování",
    "Perspektiva lokality: dobrá", "Přítomnost rostlin: hojně"
  ),
  # 2019-2020: the host plant comes first, under another key
  old_plant_key = note(
    "sta_pritomnostrostlin: jednotlivě", "Seč vhodně načasovaná: ne", "Seč celoplošná: ano",
    "Negativní vlivy na lokalitě: sečení", "sukcese všeobecně", "Perspektiva lokality: špatná"
  ),
  # threat items with commas inside and outside parentheses
  commas = note(
    "vliv: nevhodná seč (vysoká intenzita, termín apod.)", "hromadění organického materiálu",
    "Perspektiva lokality: neznámá", "Přítomnost rostlin: žádné"
  ),
  # free text under 'jiný vliv', itself containing commas
  free_text = note(
    "Negativní vlivy na lokalitě: sukcese", "jiný vliv: extenzivne udrzovany okraj", "do budoucna ohrozen",
    "Perspektiva lokality: dobrá", "Přítomnost rostlin: dominantně"
  ),
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

#----------------------------------------------------------#
# Escaping and whole-item threat matching (harmonised vocabulary) -----
#----------------------------------------------------------#

test_that(".regex_escape() escapes every special character", {
  x <- "a (b.c) [d] $e ^ \\ | {f}+*?"
  expect_true(grepl(paste0("^", .regex_escape(x), "$"), x, perl = TRUE))
  expect_false(grepl(paste0("^", .regex_escape(x), "$"), paste0(x, "x"), perl = TRUE))
})

test_that("a threat item matches as a whole item only", {
  threats <- c(
    "nevhodná seč (vysoká intenzita, termín apod.),expanzní druhy (mimo invazních)",
    "dopady chovu dobytka mimo pastvu (např. sešlap, eutrofizace apod.)",
    "sukcese všeobecně, vývoj biocenózy",
    "sukcese: Absence zivne rostliny, eutrofizace",
    NA
  )
  expect_equal(
    has_threat_item(threats, "nevhodná seč (vysoká intenzita, termín apod.)"),
    c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  # "eutrofizace" inside the description of another item does not count.
  expect_equal(has_threat_item(threats, "eutrofizace"), c(FALSE, FALSE, FALSE, TRUE, FALSE))
  # Nor does "sukcese" as the start of "sukcese všeobecně"; a comment after
  # ":" is allowed.
  expect_equal(has_threat_item(threats, "sukcese"), c(FALSE, FALSE, FALSE, TRUE, FALSE))
})

test_that("the harmonised vocabulary maps both threat lists to the classes", {
  items <- list(
    Encroachment = c("sukcese", "sukcese všeobecně"),
    Eutrophization = c("eutrofizace či okyselování", "eutrofizace")
  )
  flags <- threat_item_flags(
    c("sukcese všeobecně, vývoj biocenózy", "eutrofizace či okyselování", "sečení", NA),
    items
  )
  expect_equal(flags$Encroachment, c(1, 0, 0, NA))
  expect_equal(flags$Eutrophization, c(0, 1, 0, NA))
})

test_that("the coverage report finds the text no item covers", {
  cov <- threat_vocabulary_coverage(
    c("sukcese, něco jiného", "sukcese: poznámka"),
    items = list(Encroachment = "sukcese"), unmapped = character(), no_threat = "žádný vliv"
  )
  expect_equal(cov$items$records[cov$items$item == "sukcese"], 2)
  expect_equal(cov$unrecognised$text, "něco jiného")
})

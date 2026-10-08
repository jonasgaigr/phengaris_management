#----------------------------------------------------------#
# protected_area_membership() -----
#----------------------------------------------------------#

cells <- square_cells(n = 3)
evl <- cells
evl$SITECODE <- c("CZ001", "CZ002", "CZ003")
mzchu <- cells[3, ]

subjects <- tibble::tibble(
  site_code = c("CZ001", "CZ002", "CZ002"),
  nazev_lat = c("Maculinea nausithous", "Maculinea teleius", "Maculinea nausithous")
)

pt <- function(x) sf::st_point(c(x, -1099500))
sites <- sf::st_sf(
  ID_LOKAL = c(1, 2, 3, 4),
  DRUH = c(SPECIES_NAU, SPECIES_TEL, SPECIES_NAU, SPECIES_TEL),
  geometry = sf::st_sfc(pt(-699500), pt(-698500), pt(-697500), pt(-650000), crs = 5514)
)

pa <- suppressWarnings(protected_area_membership(sites, evl, mzchu, subjects))

test_that("every site inside an EVL is listed as EVL_any", {
  expect_setequal(pa$ID_LOKAL[pa$PA_TYPE == "EVL_any"], c(1, 2, 3))
})

test_that("EVL_target records which species the EVL was designated for", {
  target <- pa[pa$PA_TYPE == "EVL_target", ]
  # CZ001 for P. nausithous; CZ002 for both species.
  expect_setequal(target$ID_LOKAL[target$PA_SPECIES == SPECIES_NAU], c(1, 2))
  expect_setequal(target$ID_LOKAL[target$PA_SPECIES == SPECIES_TEL], 2)
  expect_false(3 %in% target$ID_LOKAL)
})

test_that("MZCHU membership and sites outside every area", {
  expect_setequal(pa$ID_LOKAL[pa$PA_TYPE == "MZCHU"], 3)
  expect_false(4 %in% pa$ID_LOKAL)
})

test_that("the species-blind EVL_target flag of the legacy analysis", {
  records <- tibble::tibble(
    ID_NALEZ = 1:2, DRUH = c(SPECIES_TEL, SPECIES_TEL), ID_LOKAL = c(1, 2),
    NEGATIV = 0, DATUM_OD = 20220701, SITMAP = 1, POP_BIOT = NA, STRUKT_POZN = NA
  )
  d <- derive_analysis_variables(records, pa, tibble::tibble(ID_NALEZ = numeric()))
  # Site 1 lies in an EVL designated for P. nausithous only, but the legacy
  # flag counts it for a P. teleius record too.
  expect_equal(d$EVL_target, c(1, 1))
})

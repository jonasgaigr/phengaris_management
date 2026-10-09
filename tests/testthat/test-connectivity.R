#----------------------------------------------------------#
# Site design and connectivity -----
#----------------------------------------------------------#
#
# Sites (S-JTSK, metres):
#
#   1  polygon 100 x 100 m (1 ha) at the origin
#   2  point 30 m east of polygon 1 (same persistent site at 50 m)
#   3  point 2 km east
#   4  polygon 200 x 100 m (2 ha), 5 km east
#
#----------------------------------------------------------#

x0 <- -700000
y0 <- -1100000
square <- function(x, y, w, h) {
  sf::st_polygon(list(rbind(c(x, y), c(x + w, y), c(x + w, y + h), c(x, y + h), c(x, y))))
}
sites <- sf::st_sf(
  ID_LOKAL = 1:4,
  geometry = sf::st_sfc(
    square(x0, y0, 100, 100),
    sf::st_point(c(x0 + 130, y0 + 50)),
    sf::st_point(c(x0 + 2000, y0 + 50)),
    square(x0 + 5000, y0, 200, 100),
    crs = 5514
  )
)

test_that("site_design_table() gives persistent sites, areas and positions", {
  sd <- site_design_table(sites, distance_m = 50, sensitivity = c(0, 5000))
  expect_equal(sd$GEOMETRY_TYPE, c("polygon", "point", "point", "polygon"))
  expect_equal(sd$SITE_AREA_HA, c(1, NA, NA, 2))
  # 50 m merges sites 1 and 2 only; 0 m merges nothing.
  expect_equal(sd$SITE_ID[1], sd$SITE_ID[2])
  expect_equal(dplyr::n_distinct(sd$SITE_ID), 3)
  expect_equal(dplyr::n_distinct(sd$SITE_ID_0M), 4)
  expect_equal(dplyr::n_distinct(sd$SITE_ID_5000M), 1)
})

test_that("within_distance() gives the same pairs as st_is_within_distance()", {
  expect_equal(
    lapply(within_distance(sites, 2000), sort),
    lapply(sf::st_is_within_distance(sites, sites, dist = 2000), sort)
  )
})

test_that("connectivity_index() follows S = sum exp(-alpha d) A^b, without the site itself", {
  focal <- tibble::tibble(SITE_X = 0, SITE_Y = 0, SITE_ID = "A")
  sources <- tibble::tibble(
    SITE_X = c(0, 1000, 3000, 20000), SITE_Y = 0,
    SITE_ID = c("A", "B", "C", "D"), AREA_HA = c(5, 4, 1, 9)
  )
  s <- connectivity_index(focal, sources, alpha = 1, b = 0.5, max_km = 15)
  # Site A is the focal site itself and D lies beyond 15 km.
  expect_equal(s, exp(-1) * 2 + exp(-3) * 1)
  expect_equal(connectivity_index(focal, sources[0, ], 1, 0.5, 15), 0)
})

test_that("connectivity_table() counts occupied sites of the same species, this year and last", {
  sd <- site_design_table(sites, distance_m = 50, sensitivity = numeric())
  records <- tibble::tibble(
    ID_LOKAL = c(1, 3, 4, 4, 3),
    DRUH = c(SPECIES_NAU, SPECIES_NAU, SPECIES_NAU, SPECIES_TEL, SPECIES_NAU),
    DATUM_OD = c(20220720, 20220720, 20210720, 20220720, 20190720),
    NEGATIV = c(1, 0, 0, 0, 0),
    ZDROJ = c("t", "o", "o", "o", "o")
  )
  settings <- list(
    alpha_per_km = 1, alpha_sensitivity_per_km = list(), area_exponent = 0.5,
    sources = "all", max_distance_km = 15
  )
  conn <- connectivity_table(records, sd, focal_sources = "t", settings = settings)
  nau <- conn[conn$DRUH == SPECIES_NAU, ]
  # Site 1 (2022): site 3 occupied in 2022 (a point: the median occupied
  # polygon area, 2 ha) and site 4 occupied in 2021; site 3's 2019 record is
  # too old.
  d3 <- sqrt((sd$SITE_X[3] - sd$SITE_X[1])^2 + (sd$SITE_Y[3] - sd$SITE_Y[1])^2) / 1000
  d4 <- sqrt((sd$SITE_X[4] - sd$SITE_X[1])^2 + (sd$SITE_Y[4] - sd$SITE_Y[1])^2) / 1000
  expect_equal(nau$CONN, exp(-d3) * sqrt(2) + exp(-d4) * sqrt(2))
  expect_equal(nau$CONN_SOURCES, 2)
  # P. teleius: only site 4 in 2022.
  expect_equal(conn$CONN_SOURCES[conn$DRUH == SPECIES_TEL], 1)
})

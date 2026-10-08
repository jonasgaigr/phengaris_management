#----------------------------------------------------------#
# match_sites(): persistent sites across years -----
#----------------------------------------------------------#

pt <- function(x, y = -1100000) sf::st_point(c(x, y))
sq <- function(x0, size = 20) {
  sf::st_polygon(list(rbind(
    c(x0, -1100000), c(x0 + size, -1100000), c(x0 + size, -1099980),
    c(x0, -1099980), c(x0, -1100000)
  )))
}

# 1 and 2: overlapping polygons (the same meadow mapped twice).
# 3: a point 30 m east of polygon 2.
# 4: a point 30 m east of 3 (a chain: 4 joins 2 only through 3).
# 5: far away.
sites <- sf::st_sf(
  ID_LOKAL = 1:5,
  geometry = sf::st_sfc(sq(-700000), sq(-699990), pt(-699940), pt(-699910), pt(-690000), crs = 5514)
)

test_that("distance 0 merges only geometries that touch or overlap", {
  m <- match_sites(sites, distance_m = 0)
  expect_equal(m$ID_LOKAL, 1:5)
  expect_equal(m$SITE_ID[1], m$SITE_ID[2])
  expect_equal(length(unique(m$SITE_ID)), 4)
})

test_that("a distance threshold merges nearby geometries, chains included", {
  m <- match_sites(sites, distance_m = 35)
  expect_equal(length(unique(m$SITE_ID[1:4])), 1)
  expect_false(m$SITE_ID[5] %in% m$SITE_ID[1:4])
})

test_that("connected_components() follows chains of neighbours", {
  expect_equal(connected_components(list(2, c(1, 3), 2, integer(0))), c(1, 1, 1, 2))
})

test_that("a negative distance is refused", {
  expect_error(match_sites(sites, distance_m = -1))
})

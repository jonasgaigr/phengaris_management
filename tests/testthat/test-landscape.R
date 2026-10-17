#----------------------------------------------------------#
# The landscape around the sites (P4.7) -----
#----------------------------------------------------------#

test_that("land-cover groups follow the code prefixes", {
  groups <- list(GRASSLAND = c("B311", "C11"), ARABLE = "B111", FOREST = "D1")
  source(file.path(project_root, "R/tools/build_landscape_layers.R"), local = TRUE)
  expect_equal(landcover_group(c("C113", "B311", "B111", "D124", "D212", "A111", NA), groups), c(1L, 1L, 2L, 3L, 0L, 0L, NA))
})

test_that("shares in circles come from the fine raster for small radii and the coarse one for large", {
  # A 2 km x 2 km square of 25 m cells: the western half group 1, the
  # eastern half group 2; VMB natural 100 % in the western half.
  template <- terra::rast(xmin = 0, xmax = 2000, ymin = 0, ymax = 2000, resolution = 25, crs = "EPSG:5514")
  x <- terra::xFromCell(template, seq_len(terra::ncell(template)))
  landcover <- terra::setValues(template, ifelse(x < 1000, 1L, 2L))
  vmb <- c(terra::setValues(template, ifelse(x < 1000, 100, 0)), terra::setValues(template, 0))
  binary <- terra::segregate(landcover, classes = 1:2, other = 0)
  coarse <- terra::aggregate(c(binary * 100, vmb), fact = 4, fun = "mean")
  points <- tibble::tibble(ID_LOKAL = 1:2, SITE_X = c(500, 1000), SITE_Y = 1000)
  out <- landscape_shares(
    points, landcover, vmb, coarse,
    radii = c(200, 600), groups = list(GRASSLAND = "C", ARABLE = "B"), fine_max = 500
  )
  expect_equal(names(out), c(
    "ID_LOKAL", "GRASSLAND_200", "GRASSLAND_600", "ARABLE_200", "ARABLE_600",
    "NATURAL_200", "NATURAL_600", "SANGUISORBA_200", "SANGUISORBA_600"
  ))
  # Point 1 lies 500 m inside the western half: all group 1 within 200 m.
  expect_equal(out$GRASSLAND_200[1], 1)
  expect_equal(out$NATURAL_200[1], 1)
  # Point 2 lies on the boundary: about half and half at either radius.
  expect_true(abs(out$GRASSLAND_200[2] - 0.5) < 0.1)
  expect_true(abs(out$ARABLE_600[2] - 0.5) < 0.1)
  expect_equal(out$GRASSLAND_600 + out$ARABLE_600, c(1, 1), tolerance = 1e-6)
})

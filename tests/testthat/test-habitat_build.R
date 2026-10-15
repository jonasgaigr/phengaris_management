#----------------------------------------------------------#
# Building the local habitat layers from the kraj archives -----
#----------------------------------------------------------#
#
# Three VMB segments in two kraje, A and B, 100 m wide:
#
#   segment 1  natural mosaic, cut by the kraj line into a piece in A and a
#              piece in B; its habitat rows are repeated in both tables
#   segment 2  in A, a mosaic only 40 % natural
#   segment 3  in B, unmapped ("-")
#
#----------------------------------------------------------#

cells <- square_cells(x0 = -700000, size = 100, n = 4)

pieces <- sf::st_sf(
  SEGMENT_ID = c(1, 1, 2, 3),
  FSB = c("T", "T", "moz.", "-"),
  BIOTOP_SEZ = c("T1.1 (70), X7 (30)", "T1.1 (70), X7 (30)", "X12A (60), T1.9 (40)", "-1 (100)"),
  ROK_AKT = c(2022, 2022, 2019, 0),
  DATUM = as.Date(c("2022-07-01", "2022-07-01", "2019-06-01", NA)),
  NUTS3 = c("A", "B", "A", "B"),
  geometry = sf::st_geometry(cells)
)

habitat_rows <- function(kraj) {
  rows <- tibble::tibble(
    SEGMENT_ID = c(1, 1, 2, 2, 3),
    B_ID = c(1L, 2L, 1L, 2L, 1L),
    BIOTOP = c("T1.1", "X7", "X12A", "T1.9", "-1"),
    STEJ_PR = c(70L, 30L, 60L, 40L, 100L),
    REPRE = c("B", NA, NA, "C", NA),
    ZACH = c("B", NA, NA, "C", NA),
    KVALITA = c(2L, 4L, 4L, 3L, NA),
    DG = NA_character_,
    RB = NA_character_
  )
  # Each kraj table holds the rows of the segments it has a piece of.
  rows[rows$SEGMENT_ID %in% pieces$SEGMENT_ID[pieces$NUTS3 == kraj], ]
}
biotopes <- dplyr::bind_rows(habitat_rows("A"), habitat_rows("B"))

test_that("segments cut by a kraj line are put back together, their habitat rows kept once", {
  built <- build_habitat_segments(pieces, biotopes)
  layer <- built$layer
  expect_equal(built$segments, 3)
  expect_equal(built$split, 1)
  expect_equal(built$share_mismatch, 0)
  # One row per segment x mapped habitat.
  expect_equal(nrow(layer), 5)
  expect_equal(anyDuplicated(sf::st_drop_geometry(layer)[c("SEGMENT_ID", "B_ID")]), 0)
  seg1 <- layer[layer$SEGMENT_ID == 1, ]
  expect_equal(unique(seg1$SHAPE_Area), 20000)
  expect_equal(unique(seg1$NUTS3), "A,B")
})

test_that("FSB_EVAL marks a mapped segment less than half natural as X", {
  layer <- sf::st_drop_geometry(build_habitat_segments(pieces, biotopes)$layer) %>%
    dplyr::distinct(SEGMENT_ID, FSB, FSB_EVAL, NATURAL_PCT)
  expect_equal(layer$NATURAL_PCT, c(70, 40, 0))
  expect_equal(layer$FSB_EVAL, c("T", "X", "-"))
})

test_that("the built layer satisfies the habitat input contract", {
  layer <- build_habitat_segments(pieces, biotopes)$layer
  expect_true(all(CONFIG$habitat$required_columns %in% names(layer)))
  # The Natura 2000 habitat code is not published.
  expect_true(all(is.na(layer$HABITAT)))
  path <- tempfile(fileext = ".gpkg")
  sf::st_write(layer, path, quiet = TRUE)
  expect_s3_class(read_habitat_layer(path), "sf")
})

test_that("habitat rows that differ between two kraje stop the build", {
  conflicting <- biotopes
  conflicting$STEJ_PR[nrow(habitat_rows("A")) + 1] <- 60L
  expect_error(build_habitat_segments(pieces, conflicting), "differ between the kraje")
})

test_that("habitat shares not adding up to 100 are counted", {
  short <- biotopes[!(biotopes$SEGMENT_ID == 2 & biotopes$B_ID == 2), ]
  expect_equal(build_habitat_segments(pieces, short)$share_mismatch, 1)
})

test_that("invalid polygons are repaired and counted", {
  bowtie <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(10, 10), c(10, 0), c(0, 10), c(0, 0)))),
      sf::st_polygon(list(rbind(c(20, 0), c(30, 0), c(30, 10), c(20, 10), c(20, 0)))),
      crs = 5514
    )
  )
  repaired <- make_valid_polygons(bowtie)
  expect_equal(repaired$repaired, 1)
  expect_true(all(sf::st_is_valid(repaired$layer)))
  expect_equal(as.character(unique(sf::st_geometry_type(repaired$layer))), "MULTIPOLYGON")
})

test_that("a layer is cut to the area around the sites, attributes and order kept", {
  # Two disjoint areas: the first two cells, and the right half of the fourth.
  area <- sf::st_sfc(
    sf::st_union(sf::st_geometry(cells)[1:2])[[1]],
    sf::st_polygon(list(rbind(
      c(-699650, -1100000), c(-699600, -1100000), c(-699600, -1099900),
      c(-699650, -1099900), c(-699650, -1100000)
    ))),
    crs = 5514
  )
  # Feature 1 lies inside the first area, feature 2 spans both areas and the
  # gap between them, feature 3 lies outside either.
  layer <- sf::st_sf(
    id = c(1, 2, 3),
    geometry = sf::st_sfc(
      sf::st_geometry(cells)[[1]],
      sf::st_union(sf::st_geometry(cells)[2:4])[[1]],
      sf::st_polygon(list(rbind(
        c(-700000, -1099800), c(-699900, -1099800), c(-699900, -1099700),
        c(-700000, -1099700), c(-700000, -1099800)
      ))),
      crs = 5514
    )
  )
  out <- clip_to_area(layer, area)
  expect_equal(out$id, c(1, 2))
  expect_equal(units::drop_units(sf::st_area(out)), c(10000, 10000 + 5000))
  # A feature inside the area is left as it is.
  expect_true(sf::st_equals(sf::st_geometry(out)[1], sf::st_geometry(layer)[1], sparse = FALSE)[1, 1])
})

test_that("KVES polygons are identified by their kraj and feature id", {
  kves <- sf::st_sf(
    kves_fid = c(1, 1), KOD = c("C111", "B311"), KATEGORIE = c("Aluviální a vlhké louky", "x"),
    NUTS3 = c("A", "B"), geometry = sf::st_geometry(cells[1:2, ])
  )
  out <- build_kves_layer(kves)
  expect_equal(out$layer$KVES_ID, c("A:1", "B:1"))
  expect_named(out$layer, c("KVES_ID", "KOD", "KATEGORIE", "NUTS3", "geometry"))
  expect_length(out$uncoded, 0)
})

test_that("a KVES version without codes takes them from another by the category name", {
  older <- sf::st_sf(
    kves_fid = c(1, 2), KATEGORIE = c("Aluviální a vlhké louky ", "Městské zelené plochy"),
    NUTS3 = c("A", "A"), geometry = sf::st_geometry(cells[1:2, ])
  )
  codes <- data.frame(KATEGORIE = c("Aluviální a vlhké louky", "Městská zeleň"), KOD = c("C111", "A611"))
  out <- build_kves_layer(older, codes)
  # The name is trimmed before the lookup; a renamed category gets no code.
  expect_equal(out$layer$KOD, c("C111", NA))
  expect_equal(out$uncoded, "Městské zelené plochy")
  expect_true(all(is.na(build_kves_layer(older)$layer$KOD)))
  two_codes <- rbind(codes, data.frame(KATEGORIE = "Městská zeleň", KOD = "A612"))
  expect_error(build_kves_layer(older, two_codes), "more than one code")
})

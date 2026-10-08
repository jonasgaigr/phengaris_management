#----------------------------------------------------------#
# Habitat layer: input contract, intersection, segment rules -----
#----------------------------------------------------------#
#
# NOT VALIDATED ON REAL DATA: these synthetic fixtures are the only data the
# habitat contract and the area-weighted rule have been run on.
#
#----------------------------------------------------------#

# Three habitat segments side by side, 100 m wide: T1.1 (natural), X7
# (non-natural) and T1.9 (natural).
segments <- square_cells(x0 = -700000, size = 100, n = 3)
segments$SEGMENT_ID <- c(1, 2, 3)
segments$BIOTOP <- c("T1.1", "X7", "T1.9")
segments$BIOTOP_SEZ <- c("T1.1 (100)", "X7 (100)", "T1.9 (60), T1.1 (40)")
segments$FSB <- c("T", "X", "T")
segments$FSB_EVAL <- c("T", "X", "T")
segments$HABITAT <- c("6510", NA, "6410")
segments$STEJ_PR <- c(100, 100, 60)
segments$REPRE <- c("A", NA, "B")
segments$ZACH <- c("A", NA, "B")

# Site 1: polygon covering 60 m of segment 1 and 20 m of segment 2.
# Site 2: point inside segment 3.
sites <- sf::st_sf(
  ID_LOKAL = c(1, 2),
  geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(-699960, -1100000), c(-699880, -1100000), c(-699880, -1099900),
      c(-699960, -1099900), c(-699960, -1100000)
    ))),
    sf::st_point(c(-699750, -1099950)),
    crs = 5514
  )
)

overlaps <- function(...) {
  suppressWarnings(intersect_sites_with_habitats(sites, segments, ...)) %>%
    sf::st_drop_geometry()
}

test_that("the intersection gives the overlap area, and buffers point sites on request", {
  plain <- overlaps(point_buffer_m = NULL, line_buffer_m = NULL)
  expect_equal(sort(plain$AREA_real[plain$ID_LOKAL == 1]), c(2000, 6000))
  expect_equal(plain$AREA_real[plain$ID_LOKAL == 2], 0)
  buffered <- overlaps(point_buffer_m = 10, line_buffer_m = NULL)
  expect_gt(buffered$AREA_real[buffered$ID_LOKAL == 2], 300)
})

records <- tibble::tibble(ID_RECORD = c("a", "b", "c"), ID_LOKAL = c(1, 2, 1))

test_that("the legacy rule keeps the least natural, smallest overlap per record", {
  segs <- overlaps(point_buffer_m = NULL, line_buffer_m = NULL)
  out <- select_habitat_segment(records, segs, rule = "legacy")
  expect_equal(out$ID_RECORD, records$ID_RECORD)
  # Site 1: X7 (non-natural) wins over the larger, natural T1.1.
  expect_equal(out$BIOTOP[out$ID_RECORD == "a"], "X7")
  expect_equal(out$AREA_SITE[out$ID_RECORD == "a"], 2000)
  expect_equal(out$NATURAL[out$ID_RECORD == "a"], 0)
})

test_that("the area-weighted rule summarises every overlap of a site", {
  segs <- overlaps(point_buffer_m = NULL, line_buffer_m = NULL)
  out <- select_habitat_segment(records, segs,
    rule = "area_weighted",
    sanguisorba_codes = "T1.1", share_column = NULL
  )
  a <- out[out$ID_RECORD == "a", ]
  expect_equal(a$AREA_SITE, 8000)
  expect_equal(a$NATURAL_SHARE, 0.75)
  expect_equal(a$SANGUISORBA_AREA, 6000)
  expect_equal(a$BIOTOP, "T1.1")
  expect_equal(a$NATURAL, 1)
  # The same site gives the same summary for every record.
  expect_equal(out$AREA_SITE[out$ID_RECORD == "c"], 8000)
})

test_that("an empty Sanguisorba code list gives NA, never 0", {
  segs <- overlaps(point_buffer_m = NULL, line_buffer_m = NULL)
  out <- select_habitat_segment(records, segs,
    rule = "area_weighted",
    sanguisorba_codes = character(0), share_column = NULL
  )
  expect_true(all(is.na(out$SANGUISORBA_AREA)))
})

test_that("a share column weights the overlaps of a mosaic segment", {
  segs <- overlaps(point_buffer_m = 10, line_buffer_m = NULL)
  plain <- select_habitat_segment(records, segs, rule = "area_weighted", share_column = NULL)
  weighted <- select_habitat_segment(records, segs, rule = "area_weighted", share_column = "STEJ_PR")
  b_plain <- plain$AREA_SITE[plain$ID_RECORD == "b"]
  expect_equal(weighted$AREA_SITE[weighted$ID_RECORD == "b"], b_plain * 0.6)
})

test_that("the habitat layer is checked against the input contract", {
  path <- tempfile(fileext = ".gpkg")
  sf::st_write(segments, path, quiet = TRUE)
  expect_s3_class(read_habitat_layer(path), "sf")

  missing_col <- tempfile(fileext = ".gpkg")
  sf::st_write(segments[, setdiff(names(segments), "FSB")], missing_col, quiet = TRUE)
  expect_error(read_habitat_layer(missing_col), "FSB")

  wrong_crs <- tempfile(fileext = ".gpkg")
  sf::st_write(sf::st_transform(segments, 4326), wrong_crs, quiet = TRUE)
  expect_error(read_habitat_layer(wrong_crs), "EPSG")

  expect_error(read_habitat_layer(tempfile(fileext = ".gpkg")), "not found")
})

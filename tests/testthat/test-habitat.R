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

test_that("a site without area takes the habitat it lies in only when that is a single one", {
  segs <- overlaps(point_buffer_m = NULL, line_buffer_m = NULL)
  out <- select_habitat_segment(records, segs, rule = "area_weighted", share_column = NULL)
  b <- out[out$ID_RECORD == "b", ]
  expect_equal(b$AREA_SITE, 0)
  expect_true(is.na(b$NATURAL_SHARE))
  expect_equal(b$BIOTOP, "T1.9")

  # The same point in a mosaic of two habitats: no dominant habitat.
  mosaic <- dplyr::bind_rows(segs, dplyr::mutate(segs[segs$ID_LOKAL == 2, ], BIOTOP = "T1.1"))
  out <- select_habitat_segment(records, mosaic, rule = "area_weighted", share_column = NULL)
  expect_true(is.na(out$BIOTOP[out$ID_RECORD == "b"]))
  expect_true(is.na(out$NATURAL[out$ID_RECORD == "b"]))
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

test_that("the Natura 2000 habitat type comes from the habitat catalogue where the layer has none", {
  catalogue <- tibble::tibble(BIOTOP = c("T1.1", "T1.9", "T1.5", "X7"), HABITAT = c("6510", "6410", NA, NA))
  # Derived from the habitat code; none for a habitat that is no Natura 2000 type.
  derived <- natura_habitat_of(c("T1.1", "T1.9", "T1.5", "X7", NA), catalogue = catalogue)
  expect_equal(derived, c("6510", "6410", NA, NA, NA))
  # A code the layer already has is kept, the priority form of 6210 too.
  expect_equal(natura_habitat_of(c("T1.1", "T3.4D"), c(NA, "6210p"), catalogue = catalogue), c("6510", "6210p"))
  # Without the catalogue the codes stay as they are.
  expect_equal(natura_habitat_of(c("T1.1", "T1.9"), c(NA, "6410"), catalogue = NULL), c(NA, "6410"))
})

test_that("the local copy of the habitat catalogue has one row per habitat code", {
  catalogue <- read_habitat_catalogue(file.path(project_root, "Data/Input/reference/habitat_catalogue.csv"))
  expect_false(is.null(catalogue))
  expect_false(anyDuplicated(catalogue$BIOTOP) > 0)
  expect_equal(catalogue$HABITAT[match(c("T1.1", "T1.9", "T1.5"), catalogue$BIOTOP)], c("6510", "6410", NA))
})

#----------------------------------------------------------#
# The intersection a specification uses -----
#----------------------------------------------------------#

test_that("the current intersection falls back to the frozen one when it is missing", {
  current <- tempfile(fileext = ".csv")
  frozen <- tempfile(fileext = ".csv")
  writeLines("x", frozen)
  expect_equal(choose_habitat_overlaps("current", current, frozen), list(path = frozen, source = "frozen"))
  writeLines("x", current)
  expect_equal(choose_habitat_overlaps("current", current, frozen), list(path = current, source = "current"))
  expect_equal(choose_habitat_overlaps("frozen", current, frozen)$source, "frozen")
  file.remove(frozen)
  expect_equal(choose_habitat_overlaps("frozen", current, frozen)$source, "none")
})

#----------------------------------------------------------#
# KVES ecosystem classes -----
#----------------------------------------------------------#

# The same three cells as KVES polygons: wet meadow, arable land, wet meadow.
kves <- segments["geometry"]
kves$KVES_ID <- c("A:1", "A:2", "A:3")
kves$KOD <- c("C111", "B111", "C111")
kves$KATEGORIE <- c("Aluviální a vlhké louky", "Orná půda", "Aluviální a vlhké louky")

kves_overlaps <- function(...) {
  suppressWarnings(intersect_sites_with_habitats(sites, kves, ...)) %>%
    sf::st_drop_geometry()
}

test_that("each site gets the KVES class covering most of it", {
  out <- summarise_kves(records, kves_overlaps(point_buffer_m = NULL, line_buffer_m = NULL))
  expect_equal(out$ID_RECORD, records$ID_RECORD)
  # Site 1: 6000 m2 of wet meadow against 2000 m2 of arable land.
  expect_equal(out$KVES_KOD[out$ID_RECORD == "a"], "C111")
  expect_equal(out$KVES_KOD[out$ID_RECORD == "c"], "C111")
  # Site 2, a point, lies in the wet meadow of cell 3.
  expect_equal(out$KVES_KOD[out$ID_RECORD == "b"], "C111")
})

test_that("a point on the boundary between two KVES classes gets no class", {
  on_boundary <- sites
  sf::st_geometry(on_boundary)[2] <- sf::st_point(c(-699900, -1099950))
  overlaps <- suppressWarnings(intersect_sites_with_habitats(on_boundary, kves, point_buffer_m = NULL)) %>%
    sf::st_drop_geometry()
  out <- summarise_kves(records, overlaps)
  expect_true(is.na(out$KVES_KOD[out$ID_RECORD == "b"]))
  expect_true(is.na(out$KVES_KATEGORIE[out$ID_RECORD == "b"]))
})

test_that("the KVES layer is checked for its columns and CRS", {
  path <- tempfile(fileext = ".gpkg")
  sf::st_write(kves, path, quiet = TRUE)
  expect_s3_class(read_kves_layer(path), "sf")

  missing_col <- tempfile(fileext = ".gpkg")
  sf::st_write(kves[, setdiff(names(kves), "KOD")], missing_col, quiet = TRUE)
  expect_error(read_kves_layer(missing_col), "KOD")

  wrong_crs <- tempfile(fileext = ".gpkg")
  sf::st_write(sf::st_transform(kves, 4326), wrong_crs, quiet = TRUE)
  expect_error(read_kves_layer(wrong_crs), "EPSG")
})

test_that("a category of an earlier KVES version takes its 2025 code from the crosswalk", {
  crosswalk <- list("Hospodářské louky" = "B311", "Bučiny" = "D124", "Hospodářské lesy smíšené" = "D11")
  # Only a missing code is filled; a category without an entry stays NA.
  expect_equal(
    kves_code_of(c("Hospodářské louky", "Bučiny ", "Orná půda", "Neznámá"), c(NA, NA, "B111", NA), crosswalk),
    c("B311", "D124", "B111", NA)
  )
  # A category 2025 splits takes the code its parts share, which the
  # land-cover groups read by prefix.
  split_code <- kves_code_of("Hospodářské lesy smíšené", crosswalk = crosswalk)
  expect_equal(split_code, "D11")
  expect_true(startsWith(split_code, CONFIG$landscape$groups$FOREST))
  expect_equal(kves_code_of(c("Bučiny", "Orná půda"), c(NA, "B111"), crosswalk = NULL), c(NA, "B111"))
})

test_that("the crosswalk of config.yml gives every renamed or split category a code", {
  crosswalk <- CONFIG$habitat$kves_category_codes
  expect_length(crosswalk, 16)
  expect_true(all(grepl("^[A-G][0-9]{2,3}$", unlist(crosswalk))))
})

#----------------------------------------------------------#
# Shares of natural, X and Sanguisorba habitats -----
#----------------------------------------------------------#

test_that("the area-weighted rule gives the shares of natural, X and Sanguisorba habitats", {
  segs <- overlaps(point_buffer_m = NULL, line_buffer_m = NULL)
  out <- select_habitat_segment(records, segs, rule = "area_weighted", sanguisorba_codes = "T1.1")
  a <- out[out$ID_RECORD == "a", ]
  # Site 1: 6000 m2 of T1.1, 2000 m2 of X7.
  expect_equal(a$NATURAL_SHARE, 0.75)
  expect_equal(a$X_SHARE, 0.25)
  expect_equal(a$SANGUISORBA_SHARE, 0.75)
  # A site without area has no shares.
  b <- out[out$ID_RECORD == "b", ]
  expect_true(is.na(b$X_SHARE))
  expect_true(is.na(b$SANGUISORBA_SHARE))
  no_codes <- select_habitat_segment(records, segs, rule = "area_weighted", sanguisorba_codes = character(0))
  expect_true(all(is.na(no_codes$SANGUISORBA_SHARE)))
})

#----------------------------------------------------------#
# Site areas and KVES versions -----
#----------------------------------------------------------#

test_that("each record year takes the KVES version of config.yml", {
  expect_equal(kves_version_of(c(2019, 2022, 2024)), c("2021", "2022", "2025"))
  expect_error(kves_version_of(2018), "No KVES version for the year")
})

test_that("a point site is described over the KVES polygon it lies in, cut to the margin", {
  far <- sf::st_sf(ID_LOKAL = 3, geometry = sf::st_sfc(sf::st_point(c(-690000, -1099950)), crs = 5514))
  on_boundary <- sf::st_sf(ID_LOKAL = 4, geometry = sf::st_sfc(sf::st_point(c(-699900, -1099950)), crs = 5514))
  all_sites <- rbind(sites, far, on_boundary)

  out <- site_areas(all_sites, kves, margin_m = 200)
  expect_equal(out$AREA_SOURCE, c("site", "KVES", "none", "KVES"))
  area <- units::drop_units(sf::st_area(out))
  # The polygon site keeps its own area; the point in cell 3 gets the cell.
  expect_equal(area[1], 8000)
  expect_equal(area[2], 10000)
  # A point outside KVES stays as it is.
  expect_equal(area[3], 0)
  # A point on the boundary of two polygons gets both.
  expect_equal(area[4], 20000)

  # Cut to 30 m around the point: a disc inside the cell.
  cut <- site_areas(all_sites, kves, margin_m = 30)
  expect_equal(units::drop_units(sf::st_area(cut))[2], pi * 30^2, tolerance = 0.01)

  expect_equal(site_areas(all_sites, NULL)$AREA_SOURCE, c("site", "none", "none", "none"))
})

test_that("records are joined to the site area of their KVES version", {
  versioned_overlaps <- function(areas, layer, version) {
    suppressWarnings(intersect_sites_with_habitats(areas, layer)) %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(KVES_VERSION = version)
  }
  areas <- site_areas(sites, kves, margin_m = 200)
  segs <- dplyr::bind_rows(
    versioned_overlaps(areas, segments, "2021"),
    # The same site in a later version, over a smaller area.
    versioned_overlaps(site_areas(sites, kves, margin_m = 30), segments, "2025")
  )
  versioned <- tibble::tibble(ID_RECORD = c("b1", "b2"), ID_LOKAL = c(2, 2), KVES_VERSION = c("2021", "2025"))
  out <- select_habitat_segment(versioned, segs, rule = "area_weighted", by = c("ID_LOKAL", "KVES_VERSION"))
  expect_equal(out$AREA_SITE[1], 10000)
  expect_equal(out$AREA_SITE[2], pi * 30^2, tolerance = 0.01)
  # Now the point has an area, it gets a dominant habitat.
  expect_equal(out$BIOTOP, c("T1.9", "T1.9"))

  k <- summarise_kves(versioned, versioned_overlaps(areas, kves, "2021"), by = c("ID_LOKAL", "KVES_VERSION"))
  expect_equal(k$KVES_KOD, c("C111", NA))
})

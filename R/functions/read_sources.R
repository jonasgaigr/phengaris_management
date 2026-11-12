#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: reading the source data and the intermediates
#
#----------------------------------------------------------#
#
# One reader per kind of file, so that every step reads a file the same way:
#
#   read_occurrence_csv()     an AOPK occurrence export
#   read_lokal()              one site-geometry shapefile
#   read_range()              one Article 17 reporting-range layer
#   read_records()            the parsed records written by step 01
#   read_sites()              the site geometries written by step 01
#   read_records_history(), read_sites_history()   the same for 2012-2018
#   read_records_with_sites() the two joined, as an sf object
#
#----------------------------------------------------------#

#--------------------------------------------------#
## Occurrence exports -----
#--------------------------------------------------#

#' Column types of the occurrence exports.
#'
#' Every type is the one readr used to guess, so that nothing downstream sees a
#' different column type, with one exception: the free-text columns are text,
#' because guessing them as logical lost three non-empty values.
OCCURRENCE_COL_TYPES <- readr::cols(
  PORADI = readr::col_double(),
  ID_NALEZ = readr::col_double(),
  DATUM_OD = readr::col_double(),
  DATUM_DO = readr::col_double(),
  CAS = readr::col_time(),
  ID_LOKAL = readr::col_double(),
  SITMAP = readr::col_double(),
  X = readr::col_double(),
  Y = readr::col_double(),
  NEGATIV = readr::col_double(),
  POCET = readr::col_double(),
  ODHAD = readr::col_double(),
  POKRYVN = readr::col_logical(),
  PATROVITOST = readr::col_logical(),
  FENOFAZE = readr::col_logical(),
  KODHN = readr::col_logical(),
  KOMPLETNI_SEZNAM = readr::col_double(),
  VEROH = readr::col_double(),
  CXLOKAL_Z = readr::col_double(),
  CXPRESNOST = readr::col_double(),
  DATI_INSERT = readr::col_double(),
  DATI_UPDATE = readr::col_double(),
  UPDATOR = readr::col_double(),
  .default = readr::col_character()
)

#' Read one occurrence export.
#'
#' The exports come out of the AOPK recording system in Windows-1250. They are
#' semicolon-separated but, unlike the usual Czech CSV dialect, use "." as the
#' decimal mark (X = -759673.12). read_csv2() must not be used: it treats "."
#' as a grouping mark, which multiplied every coordinate by 1, 10 or 100
#' depending on how many decimals it happened to have.
#'
#' The files have CRLF line endings, and most rows leave out their two empty
#' trailing fields. The lines are read first, which drops the line endings
#' cleanly; readr then reports the short rows as parsing problems and fills
#' the missing trailing fields with NA, which is what they are.
read_occurrence_csv <- function(file) {
  lines <- readr::read_lines(
    file,
    locale = readr::locale(encoding = "Windows-1250"),
    progress = FALSE
  )
  suppressWarnings(readr::read_delim(
    I(paste(lines, collapse = "\n")),
    delim = ";",
    col_types = OCCURRENCE_COL_TYPES,
    locale = readr::locale(decimal_mark = "."),
    progress = FALSE
  ))
}

#' Drop the records excluded in config.yml.
#'
#' A record whose validation in the source database says it cannot be used
#' (config.yml `records: excluded`) is left out when the exports are read, so
#' that no later step sees it: not the analysis table, and not the positive
#' records that give the flight season, the connectivity and the
#' plausibility of an imputed absence.
#'
#' @param data      records with ID_NALEZ
#' @param excluded  config.yml `records: excluded`: a list of entries with
#'                  `id` (ID_NALEZ) and `reason`
#' @return list(data = the records kept, dropped = the ID_NALEZ dropped)
drop_excluded_records <- function(data, excluded = CONFIG$records$excluded) {
  ids <- vapply(excluded, function(e) as.numeric(e$id), numeric(1))
  drop <- data$ID_NALEZ %in% ids
  list(data = data[!drop, , drop = FALSE], dropped = data$ID_NALEZ[drop])
}

#--------------------------------------------------#
## Site geometries and reporting ranges -----
#--------------------------------------------------#

#' Read one site-geometry layer.
#'
#' Sites are recorded as points, polygons or lines in three separate
#' shapefiles. Their coordinates are S-JTSK, but the .prj files that come with
#' them declare ETRS89 / LAEA (or nothing), so S-JTSK is assigned rather than
#' transformed into.
#'
#' @param dir   folder holding the w03_nd_lokalizace_* shapefiles
#' @param kind  "b" points, "p" polygons, "l" lines
read_lokal <- function(dir, kind) {
  sf::st_read(
    file.path(dir, paste0("w03_nd_lokalizace_", kind, ".shp")),
    quiet = TRUE
  ) %>%
    sf::st_set_crs(CRS_SJTSK) %>%
    sf::st_make_valid()
}

#' Read one reporting range polygon layer.
#'
#' row_n identifies the range polygon (a 10 x 10 km cell) and is carried
#' through the intersections in step 02, where it becomes the unit of "this
#' place was surveyed".
read_range <- function(file) {
  sf::st_read(file, quiet = TRUE) %>%
    sf::st_transform(CRS_SJTSK) %>%
    sf::st_make_valid() %>%
    dplyr::mutate(
      row_n = dplyr::row_number()
    )
}

#--------------------------------------------------#
## Intermediates written by step 01 -----
#--------------------------------------------------#

#' Read the parsed occurrence records written by step 01.
read_records <- function() {
  readr::read_csv(
    processed_path("records.csv", "common"),
    col_types = OCCURRENCE_COL_TYPES,
    progress = FALSE
  )
}

#' Read the site geometries written by step 01.
read_sites <- function() {
  sf::st_read(processed_path("sites.gpkg", "common"), quiet = TRUE)
}

#' Read the 2012-2018 records written by step 01.
read_records_history <- function() {
  readr::read_csv(
    processed_path("records_2012_2018.csv", "common"),
    col_types = OCCURRENCE_COL_TYPES,
    progress = FALSE
  )
}

#' Read the 2012-2018 site geometries written by step 01.
read_sites_history <- function() {
  sf::st_read(processed_path("sites_2012_2018.gpkg", "common"), quiet = TRUE)
}

#' The occurrence records with their site geometry, as an sf object.
read_records_with_sites <- function() {
  read_records() %>%
    dplyr::left_join(
      read_sites(),
      by = "ID_LOKAL"
    ) %>%
    sf::st_as_sf() %>%
    sf::st_make_valid()
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

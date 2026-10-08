#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#          Step 01 - Load source data
#
#
#----------------------------------------------------------#
#
# Reads everything the cascade starts from:
#
#   * the border of Czechia and the national mapping grid (RCzechia, WFS),
#   * protected areas: Natura 2000 sites (EVL) and small-scale reserves (MZCHU),
#   * the Natura 2000 site-subject table, used to tell which sites were
#     designated for which Phengaris species,
#   * the reporting range polygons for each species,
#   * the occurrence records and their site geometries.
#
# Writes: Data/Processed/lokal_new.gpkg
#
# Needs a working internet connection for the WFS and RCzechia layers.
#
#----------------------------------------------------------#

message("Step 01: loading source data")

#--------------------------------------------------#
## Options -----
#--------------------------------------------------#

# The 2012-2018 records are read for completeness but no later step uses them:
# the analysis covers the 2019-2024 monitoring rounds. Set to FALSE to skip
# eight file reads and shorten the cascade.
LOAD_HISTORICAL <- TRUE

#----------------------------------------------------------#
# Remote reference layers -----
#----------------------------------------------------------#
#--------------------------------------------------#
## Border of Czechia -----
#--------------------------------------------------#

czechia_border <-
  RCzechia::republika(
    resolution = "high"
  ) %>%
  sf::st_transform(
    .,
    sf::st_crs("+init=epsg:5514")
  )

#--------------------------------------------------#
## Protected areas and the mapping grid -----
#--------------------------------------------------#

# AOPK open data WFS endpoint.
wfs_endpoint <- "http://gis.nature.cz/arcgis/services/Aplikace/Opendata/MapServer/WFSServer?"

#' Build a WFS GetFeature URL for one AOPK open-data layer.
wfs_url <- function(layer_name) {
  paste0(
    wfs_endpoint,
    "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name
  )
}

evl <-
  sf::st_read(wfs_url("Opendata:Evropsky_vyznamne_lokality")) %>%
  sf::st_transform(
    .,
    sf::st_crs("+init=epsg:5514")
  )

mzchu <-
  sf::st_read(wfs_url("Opendata:Maloplosna_zvlaste_chranena_uzemi__MZCHU_")) %>%
  sf::st_transform(
    .,
    sf::st_crs("+init=epsg:5514")
  )

sitmap <-
  sf::st_read(wfs_url("Opendata:Mapovaci_sit_-_deleni_1.radu")) %>%
  sf::st_transform(
    .,
    sf::st_crs("+init=epsg:5514")
  ) %>%
  sf::st_crop(
    .,
    czechia_border
  )

#--------------------------------------------------#
## Natura 2000 sites and their target features -----
#--------------------------------------------------#

# Puts sites_subjects into the session; step 03 uses it to find the sites for
# which each Phengaris species is a designated target feature.
rn2kcz::load_n2k_sites()

#----------------------------------------------------------#
# Reporting range layers -----
#----------------------------------------------------------#

#' Read one reporting range polygon layer.
#'
#' row_n identifies the range polygon and is carried through the intersections
#' in step 02, where it becomes the unit of "this place was surveyed".
read_range <- function(file) {
  sf::st_read(file) %>%
    sf::st_transform(CRS_SJTSK) %>%
    sf::st_make_valid() %>%
    dplyr::mutate(
      row_n = dplyr::row_number()
    )
}

range_nausithous <- read_range(CONFIG$paths$ranges$nausithous)
range_teleius    <- read_range(CONFIG$paths$ranges$teleius)

#----------------------------------------------------------#
# Occurrence records and site geometries -----
#----------------------------------------------------------#

#' Column types of the occurrence exports.
#'
#' Every type is the one readr used to guess, so that nothing downstream sees a
#' different column type, with one exception: GAR_POZN is text, because it
#' used to be guessed as logical and its one non-empty value was lost.
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
#' trailing fields. readr then keeps the carriage return on the last field of
#' those short rows ("20251209\r"), which made DATI_INSERT unparseable. The
#' lines are therefore read first, which drops the line endings cleanly.
read_occurrence_csv <- function(file) {
  lines <- readr::read_lines(
    file,
    locale = readr::locale(encoding = "Windows-1250"),
    progress = FALSE
  )
  readr::read_delim(
    I(paste(lines, collapse = "\n")),
    delim = ";",
    col_types = OCCURRENCE_COL_TYPES,
    locale = readr::locale(decimal_mark = "."),
    progress = FALSE
  )
}

#' Read one site-geometry layer.
#'
#' Sites are recorded as points, polygons or lines in three separate
#' shapefiles. The shapefiles carry no CRS, so S-JTSK is assigned rather than
#' transformed into.
#'
#' @param dir   folder holding the w03_nd_lokalizace_* shapefiles
#' @param kind  "b" points, "p" polygons, "l" lines
#' @param cast  geometry type to cast to, or NULL to leave the geometry alone
read_lokal <- function(dir, kind, cast = NULL) {
  x <- sf::st_read(
    file.path(dir, paste0("w03_nd_lokalizace_", kind, ".shp"))
  ) %>%
    sf::st_set_crs(CRS_SJTSK)

  if (!is.null(cast)) {
    x <- x %>%
      sf::st_transform(CRS_SJTSK) %>%
      sf::st_cast(cast)
  }

  sf::st_make_valid(x)
}

#--------------------------------------------------#
## Species data 2019 - 2024 (the analysed period) -----
#--------------------------------------------------#

data_new <-
  dplyr::bind_rows(
    read_occurrence_csv(CONFIG$paths$occurrence$nausithous),
    read_occurrence_csv(CONFIG$paths$occurrence$teleius)
  )

dir_nau_new <- CONFIG$paths$site_layers$nausithous
dir_tel_new <- CONFIG$paths$site_layers$teleius

lokal_new <-
  dplyr::bind_rows(
    read_lokal(dir_nau_new, "b"),
    read_lokal(dir_nau_new, "p"),
    read_lokal(dir_nau_new, "l"),
    read_lokal(dir_tel_new, "b"),
    read_lokal(dir_tel_new, "p"),
    read_lokal(dir_tel_new, "l")
  ) %>%
  dplyr::rename(
    ID_LOKAL = idx_nd_lok
  ) %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  dplyr::distinct()

phengaris_lokal_new <-
  data_new %>%
  dplyr::left_join(
    .,
    lokal_new,
    by = "ID_LOKAL"
  ) %>%
  sf::st_as_sf() %>%
  sf::st_make_valid()

sf::st_write(
  lokal_new,
  file.path(PATHS$processed, "lokal_new.gpkg"),
  delete_dsn = TRUE
)

#--------------------------------------------------#
## Species data 2012 - 2018 (historical, unused) -----
#--------------------------------------------------#

if (isTRUE(LOAD_HISTORICAL)) {

  data_old <-
    dplyr::bind_rows(
      read_occurrence_csv("Data/Input/Phengaris_nausithous_2012_2018.csv"),
      read_occurrence_csv("Data/Input/Phengaris_teleius_2012_2018.csv")
    )

  dir_nau_old <- "Data/Input/Phengaris_nausithous_2012_2018"
  dir_tel_old <- "Data/Input/Phengaris_teleius_2012_2018"

  # Unlike the 2019-2024 layers, these are cast to a single geometry type.
  lokal_old <-
    dplyr::bind_rows(
      read_lokal(dir_nau_old, "b", cast = "POINT"),
      read_lokal(dir_nau_old, "p", cast = "POLYGON"),
      read_lokal(dir_nau_old, "l", cast = "LINESTRING"),
      read_lokal(dir_tel_old, "b", cast = "POINT"),
      read_lokal(dir_tel_old, "p", cast = "POLYGON"),
      read_lokal(dir_tel_old, "l", cast = "LINESTRING")
    ) %>%
    dplyr::rename(
      ID_LOKAL = idx_nd_lok
    ) %>%
    sf::st_as_sf() %>%
    sf::st_make_valid()

  phengaris_lokal_old <-
    data_old %>%
    dplyr::left_join(
      .,
      lokal_old
    ) %>%
    sf::st_as_sf() %>%
    sf::st_make_valid()

}

message("Step 01 done: ", nrow(data_new), " records, ",
        nrow(lokal_new), " site geometries")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

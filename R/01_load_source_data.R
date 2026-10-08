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
# Reads the occurrence exports and the site geometries the cascade starts
# from, and writes them in the form every later step reads:
#
#   records.csv  the 2019-2024 occurrence records of both species, parsed
#                with explicit column types (see read_occurrence_csv())
#   sites.gpkg   the site geometries (points, polygons and lines), one row
#                per ID_LOKAL, in S-JTSK
#
# Reads:  the occurrence exports and site shapefiles named in config.yml
# Writes: Data/Processed/common/records.csv
#         Data/Processed/common/sites.gpkg
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("01")

#----------------------------------------------------------#
# Occurrence records -----
#----------------------------------------------------------#

data_new <-
  dplyr::bind_rows(
    read_occurrence_csv(CONFIG$paths$occurrence$nausithous),
    read_occurrence_csv(CONFIG$paths$occurrence$teleius)
  )

#----------------------------------------------------------#
# Site geometries -----
#----------------------------------------------------------#

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

#----------------------------------------------------------#
# Validate -----
#----------------------------------------------------------#

# Every data row of the exports has been read: the files hold one record per
# line after the header.
n_lines <- sum(vapply(
  CONFIG$paths$occurrence,
  function(f) length(readr::read_lines(f, progress = FALSE)) - 1L,
  integer(1)
))

validate_table(
  data_new, "records",
  columns = c(
    ID_NALEZ = "numeric", DRUH = "character", ID_LOKAL = "numeric",
    DATUM_OD = "numeric", X = "numeric", Y = "numeric", NEGATIV = "numeric",
    ZDROJ = "character", STRUKT_POZN = "character", POP_BIOT = "character",
    AUTOR = "character"
  ),
  key = "ID_NALEZ",
  not_na = c("ID_NALEZ", "DRUH", "ID_LOKAL", "DATUM_OD", "X", "Y", "NEGATIV"),
  values = list(DRUH = c(SPECIES_NAU, SPECIES_TEL), NEGATIV = c(0, 1)),
  xy = c("X", "Y"),
  n_rows = n_lines
)

validate_table(
  sf::st_drop_geometry(lokal_new), "sites",
  columns = c(ID_LOKAL = "numeric"),
  key = "ID_LOKAL",
  not_na = "ID_LOKAL"
)
checkmate::assert_subset(
  unique(data_new$ID_LOKAL), lokal_new$ID_LOKAL,
  .var.name = "ID_LOKAL of the records (every record needs a site geometry)"
)
checkmate::assert_true(
  sf::st_crs(lokal_new) == sf::st_crs(CRS_SJTSK),
  .var.name = "CRS of the site geometries is S-JTSK"
)

#----------------------------------------------------------#
# Export -----
#----------------------------------------------------------#

readr::write_csv(
  data_new,
  processed_path("records.csv", "common")
)

sf::st_write(
  lokal_new,
  processed_path("sites.gpkg", "common"),
  delete_dsn = TRUE,
  quiet = TRUE
)

message("  ", nrow(data_new), " records, ", nrow(lokal_new), " site geometries")

step_end("01")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

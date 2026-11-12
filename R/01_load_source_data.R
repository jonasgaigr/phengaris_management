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
#   records_2012_2018.csv, sites_2012_2018.gpkg
#                the same for the earlier exports, which say where the
#                species were known before the monitoring analysed here
#                (P4.3, P4.8; step 16)
#
# Reads:  the occurrence exports and site shapefiles named in config.yml
# Writes: Data/Processed/common/records.csv, sites.gpkg,
#         records_2012_2018.csv, sites_2012_2018.gpkg
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

# The records the validation of the source database marks as unusable
# (config.yml `records: excluded`) go no further.
n_read <- nrow(data_new)
excluded <- drop_excluded_records(data_new)
data_new <- excluded$data

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
# The earlier exports -----
#----------------------------------------------------------#

# Read the same way. They hold a few records from before 2012, which are kept.
data_history <-
  dplyr::bind_rows(
    read_occurrence_csv(CONFIG$paths$occurrence_history$nausithous),
    read_occurrence_csv(CONFIG$paths$occurrence_history$teleius)
  )
lokal_history <-
  dplyr::bind_rows(lapply(unlist(CONFIG$paths$site_layers_history), function(dir) {
    dplyr::bind_rows(read_lokal(dir, "b"), read_lokal(dir, "p"), read_lokal(dir, "l"))
  })) %>%
  dplyr::rename(ID_LOKAL = idx_nd_lok) %>%
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
  n_rows = n_lines - length(excluded$dropped)
)
checkmate::assert_true(n_read == n_lines, .var.name = "every line of the exports was read")

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

n_lines_history <- sum(vapply(
  CONFIG$paths$occurrence_history,
  function(f) length(readr::read_lines(f, progress = FALSE)) - 1L,
  integer(1)
))
validate_table(
  data_history, "records_2012_2018",
  columns = c(
    ID_NALEZ = "numeric", DRUH = "character", ID_LOKAL = "numeric", DATUM_OD = "numeric", NEGATIV = "numeric"
  ),
  key = "ID_NALEZ",
  not_na = c("ID_NALEZ", "DRUH", "ID_LOKAL", "DATUM_OD", "X", "Y", "NEGATIV"),
  values = list(DRUH = c(SPECIES_NAU, SPECIES_TEL), NEGATIV = c(0, 1)),
  xy = c("X", "Y"),
  n_rows = n_lines_history
)
validate_table(
  sf::st_drop_geometry(lokal_history), "sites_2012_2018",
  key = "ID_LOKAL", not_na = "ID_LOKAL"
)
checkmate::assert_subset(
  unique(data_history$ID_LOKAL), lokal_history$ID_LOKAL,
  .var.name = "ID_LOKAL of the 2012-2018 records (every record needs a site geometry)"
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

readr::write_csv(data_history, processed_path("records_2012_2018.csv", "common"))
sf::st_write(
  lokal_history,
  processed_path("sites_2012_2018.gpkg", "common"),
  delete_dsn = TRUE,
  quiet = TRUE
)

message("  ", nrow(data_new), " records, ", nrow(lokal_new), " site geometries")
if (length(excluded$dropped)) {
  message(
    "  ", length(excluded$dropped), " record(s) excluded in config.yml: ",
    paste(excluded$dropped, collapse = ", ")
  )
}
message("  2012-2018: ", nrow(data_history), " records, ", nrow(lokal_history), " site geometries")

step_end("01")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: paths, table and figure output
#
#----------------------------------------------------------#

#--------------------------------------------------#
## Project paths -----
#--------------------------------------------------#

# Built from config.yml, which R/00_setup.R reads into CONFIG before this
# file is sourced. The folders a step writes into depend on the step and the
# specification being run; processed_path() and output_dir() in pipeline.R
# give those.
PATHS <- list(
  input      = CONFIG$paths$input,
  processed  = CONFIG$paths$processed,
  temp       = CONFIG$paths$temp,
  outputs    = CONFIG$paths$outputs
)

#--------------------------------------------------#
## Tables -----
#--------------------------------------------------#

#' Write a result table to Outputs/<scope>/Tables as CSV.
#'
#' Windows-1250 is kept as the output encoding because the tables carry Czech
#' site, observer and habitat names and are opened in Excel on Czech Windows.
#'
#' @param x     data frame to write
#' @param stem  file name without extension, e.g. "07_positivity_by_year"
#' @return the path written, invisibly
write_table_csv <- function(x, stem) {
  x <- as.data.frame(x)
  path <- file.path(output_dir("Tables"), paste0(stem, ".csv"))
  utils::write.csv(
    x,
    file = path,
    fileEncoding = "Windows-1250",
    row.names = FALSE
  )
  invisible(path)
}

#--------------------------------------------------#
## Figures -----
#--------------------------------------------------#

#' Write a figure to Outputs/<scope>/Figures as PNG.
#'
#' @param plot  a ggplot object
#' @param stem  file name without extension
#' @param width,height,dpi  passed to ggplot2::ggsave
#' @return the path written, invisibly
#' bg is set explicitly because theme_void(), used by the maps, leaves the plot
#' background transparent. A transparent PNG renders on whatever the viewer
#' happens to use, black in some readers, which is not what a manuscript figure
#' should do.
write_figure_png <- function(plot, stem, width = 8, height = 6, dpi = 300) {
  path <- file.path(output_dir("Figures"), paste0(stem, ".png"))
  ggplot2::ggsave(
    filename = path,
    plot     = plot,
    width    = width,
    height   = height,
    dpi      = dpi,
    bg       = "white"
  )
  invisible(path)
}

#--------------------------------------------------#
## Reference layers -----
#--------------------------------------------------#

#' Path of one local reference-layer copy, failing clearly when it is missing.
reference_path <- function(file) {
  path <- file.path(CONFIG$paths$reference, file)
  if (!file.exists(path)) {
    stop(
      "Reference layer ", path, " is missing. Run ",
      "R/tools/refresh_reference_layers.R to fetch the reference layers.",
      call. = FALSE
    )
  }
  path
}

#' Read one vector reference layer from its zipped GeoPackage copy.
#'
#' The layers are snapshots of remote services (see
#' R/tools/refresh_reference_layers.R); the cascade never fetches them live.
#'
#' @param name  layer name, e.g. "evl" for Data/Input/reference/evl.gpkg.zip
read_reference_layer <- function(name) {
  path <- normalizePath(reference_path(paste0(name, ".gpkg.zip")), winslash = "/")
  sf::st_read(paste0("/vsizip/", path, "/", name, ".gpkg"), quiet = TRUE)
}

#' Read the Natura 2000 site-subject table from its local copy.
read_n2k_site_subjects <- function() {
  readr::read_csv(
    reference_path("n2k_site_subjects.csv"),
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )
}

#--------------------------------------------------#
## Reading the analysis tables -----
#--------------------------------------------------#

#' Read the cleaned occurrence table (output of step 04).
read_data_clean <- function() {
  readr::read_csv(
    processed_path("data_clean.csv"),
    show_col_types = FALSE
  )
}

#' Read the analysis table with habitat attributes (output of step 06).
#'
#' Falls back to the cleaned table if step 06 has not been run, so that a
#' partial cascade still gets as far as it can.
read_data_analysis <- function() {
  path <- processed_path("data_analysis.csv")
  if (!file.exists(path)) {
    warning(
      "data_analysis.csv not found - falling back to data_clean.csv. ",
      "Habitat models will be skipped. Run step 06 first.",
      call. = FALSE
    )
    return(read_data_clean())
  }
  readr::read_csv(path, show_col_types = FALSE)
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

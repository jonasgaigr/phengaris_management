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

PATHS <- list(
  input      = "Data/Input",
  processed  = "Data/Processed",
  temp       = "Data/Temp",
  outputs    = "Outputs",
  tables     = "Outputs/Tables",
  figures    = "Outputs/Figures",
  reports    = "Outputs/Reports"
)

#' Create every output directory the cascade writes into.
init_output_dirs <- function() {
  for (p in c(PATHS$processed, PATHS$outputs,
              PATHS$tables, PATHS$figures, PATHS$reports)) {
    if (!dir.exists(p)) dir.create(p, recursive = TRUE)
  }
  invisible(PATHS)
}

#--------------------------------------------------#
## Tables -----
#--------------------------------------------------#

#' Write a result table to Outputs/Tables as CSV.
#'
#' Windows-1250 is kept as the output encoding because the tables carry Czech
#' site, observer and habitat names and are opened in Excel on Czech Windows.
#'
#' @param x     data frame to write
#' @param stem  file name without extension, e.g. "07_positivity_by_year"
#' @return the path written, invisibly
write_table_csv <- function(x, stem) {
  x <- as.data.frame(x)
  path <- file.path(PATHS$tables, paste0(stem, ".csv"))
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

#' Write a figure to Outputs/Figures as PNG.
#'
#' @param plot  a ggplot object
#' @param stem  file name without extension
#' @param width,height,dpi  passed to ggplot2::ggsave
#' @return the path written, invisibly
write_figure_png <- function(plot, stem, width = 8, height = 6, dpi = 300) {
  path <- file.path(PATHS$figures, paste0(stem, ".png"))
  ggplot2::ggsave(
    filename = path,
    plot     = plot,
    width    = width,
    height   = height,
    dpi      = dpi
  )
  invisible(path)
}

#--------------------------------------------------#
## Reading the analysis tables -----
#--------------------------------------------------#

#' Read the cleaned occurrence table (output of step 04).
read_data_clean <- function() {
  readr::read_csv(
    file.path(PATHS$processed, "data_clean.csv"),
    show_col_types = FALSE
  )
}

#' Read the analysis table with habitat attributes (output of step 06).
#'
#' Falls back to the cleaned table if step 06 has not been run, so that a
#' partial cascade still gets as far as it can.
read_data_analysis <- function() {
  path <- file.path(PATHS$processed, "data_analysis.csv")
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

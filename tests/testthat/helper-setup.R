#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Test helper: load the project's functions
#
#----------------------------------------------------------#
#
# Runs before the tests. It sources R/00_setup.R from the project root, which
# checks the packages, reads config.yml and sources R/functions, without
# running any step or touching any data. The tests then work on the small
# fixtures in tests/testthat/fixtures and on objects built inside the tests.
#
#   testthat::test_dir("tests/testthat")
#
#----------------------------------------------------------#

project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

local({
  old <- setwd(project_root)
  on.exit(setwd(old), add = TRUE)
  suppressMessages(source("R/00_setup.R", encoding = "UTF-8"))
})

#' Path of a fixture file.
fixture <- function(...) testthat::test_path("fixtures", ...)

#' A small sf layer of square cells in S-JTSK, for range and habitat fixtures.
#'
#' @param x0,y0  lower-left corner of the first cell
#' @param size   cell side in metres
#' @param n      number of cells, laid out west to east
square_cells <- function(x0 = -700000, y0 = -1100000, size = 1000, n = 2) {
  polys <- lapply(seq_len(n) - 1, function(i) {
    x <- x0 + i * size
    sf::st_polygon(list(rbind(
      c(x, y0), c(x + size, y0), c(x + size, y0 + size), c(x, y0 + size), c(x, y0)
    )))
  })
  sf::st_sf(row_n = seq_len(n), geometry = sf::st_sfc(polys, crs = 5514))
}

#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: the landscape around the sites (P4.7)
#
#----------------------------------------------------------#
#
# The only multi-scale study of the two species found the scale of effect of
# the landscape far beyond the meadow itself: grassland cover at 3-4 km,
# arable cover at 0.3-0.4 km, water at 4 km, settlements at 1 km (Kajzer-Bonk
# et al. 2016), and most studies choose radii that miss the true scale
# (Jackson & Fahrig 2015). Step 21 therefore describes every site by the
# shares of the land-cover groups of KVES (config.yml `landscape: groups`)
# and of the natural and Sanguisorba habitats of VMB in circles of several
# radii around it; step 22 picks the radius of each variable by AIC.
#
# The rasters come from R/tools/build_landscape_layers.R: a 25 m land-cover
# raster and VMB shares at 25 m, and the same aggregated to 100 m. Radii up
# to config.yml `landscape: fine_max_radius_m` are summarised on the 25 m
# rasters; larger ones on the 100 m rasters, where a circle still holds
# hundreds of cells.
#
#----------------------------------------------------------#

#' Name of a landscape column.
landscape_column <- function(variable, radius) paste0(variable, "_", radius)

#' The landscape variables: one per land-cover group, and the VMB shares.
landscape_variables <- function(groups = CONFIG$landscape$groups) {
  c(names(groups), "NATURAL", "SANGUISORBA")
}

#' Circles around points, as a SpatVector.
.circles <- function(points, radius, epsg = CONFIG$crs$epsg) {
  centres <- sf::st_as_sf(points, coords = c("SITE_X", "SITE_Y"), crs = epsg)
  terra::vect(sf::st_buffer(centres, radius, nQuadSegs = 16))
}

#' Shares of the landscape in circles around the sites.
#'
#' A share is the fraction of the cells of a circle that lie in Czechia (are
#' not NA) and in the group, or, for VMB, the mean per cent of natural (or
#' Sanguisorba) habitat of those cells / 100.
#'
#' @param points     ID_LOKAL, SITE_X, SITE_Y of the sites
#' @param landcover  the 25 m land-cover raster (group number per cell, 0
#'                   for no group)
#' @param vmb        the 25 m VMB raster: natural and Sanguisorba per cent
#' @param coarse     the 100 m raster: per cent of each group, then the two
#'                   VMB per cents
#' @param radii      config.yml `landscape: radii_m`
#' @param groups     config.yml `landscape: groups`
#' @param fine_max   config.yml `landscape: fine_max_radius_m`
#' @return one row per site: ID_LOKAL and <VARIABLE>_<radius> for every
#'         variable of landscape_variables() and radius, shares in [0, 1]
landscape_shares <- function(points, landcover, vmb, coarse,
                             radii = unlist(CONFIG$landscape$radii_m),
                             groups = CONFIG$landscape$groups,
                             fine_max = CONFIG$landscape$fine_max_radius_m) {
  out <- tibble::tibble(ID_LOKAL = points$ID_LOKAL)
  variables <- landscape_variables(groups)
  for (radius in radii) {
    circles <- .circles(points, radius)
    if (radius <= fine_max) {
      cells <- terra::extract(landcover, circles)
      names(cells) <- c("ID", "class")
      cells <- cells[!is.na(cells$class), ]
      inside <- tabulate(cells$ID, nbins = nrow(points))
      for (g in seq_along(groups)) {
        hits <- tabulate(cells$ID[cells$class == g], nbins = nrow(points))
        out[[landscape_column(names(groups)[g], radius)]] <- ifelse(inside > 0, hits / inside, NA_real_)
      }
      means <- terra::extract(vmb, circles, fun = mean, na.rm = TRUE, ID = FALSE)
    } else {
      means <- terra::extract(coarse, circles, fun = mean, na.rm = TRUE, ID = FALSE)
      for (g in seq_along(groups)) {
        out[[landscape_column(names(groups)[g], radius)]] <- means[[g]] / 100
      }
      means <- means[, length(groups) + 1:2, drop = FALSE]
    }
    out[[landscape_column("NATURAL", radius)]] <- means[[1]] / 100
    out[[landscape_column("SANGUISORBA", radius)]] <- means[[2]] / 100
  }
  # Ordered by variable, then radius.
  out[c("ID_LOKAL", as.vector(t(outer(variables, radii, landscape_column))))]
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

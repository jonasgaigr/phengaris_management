#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: persistent sites across years
#
#----------------------------------------------------------#
#
# The recording system gives every site a new ID_LOKAL each year, so the same
# meadow surveyed in 2020 and 2022 has two unrelated identifiers. match_sites()
# reconstructs persistent sites from the geometries: two site geometries
# belong to the same persistent site when they overlap or lie within a set
# distance of each other, and chains of such pairs are merged (single
# linkage).
#
# In the 2019-2024 data few sites recur whatever the distance: 3.5 % of the
# reconstructed sites span more than one year when only touching geometries
# are merged, 5.4 % at 200 m (REVIEW.md, item 3).
#
#----------------------------------------------------------#

#' Connected components of a neighbour list.
#'
#' @param neighbours  list, element i holding the indices linked to i (as
#'                    returned by sf::st_intersects() on one layer)
#' @return integer component number per element, numbered in order of first
#'         appearance
connected_components <- function(neighbours) {
  n <- length(neighbours)
  component <- rep(NA_integer_, n)
  current <- 0L
  for (i in seq_len(n)) {
    if (!is.na(component[i])) next
    current <- current + 1L
    queue <- i
    while (length(queue)) {
      j <- queue[1]
      queue <- queue[-1]
      if (!is.na(component[j])) next
      component[j] <- current
      queue <- c(queue, neighbours[[j]][is.na(component[neighbours[[j]]])])
    }
  }
  component
}

#' Pairs of geometries closer than a distance, as sf::st_is_within_distance().
#'
#' st_is_within_distance() compares every pair of projected geometries, which
#' takes minutes for the 8678 sites. Here the candidate pairs come from
#' st_intersects() on slightly enlarged buffers, which uses a spatial index,
#' and each candidate pair is then checked with its exact distance, so the
#' result is the same.
#'
#' @param sites       geometries in a projected CRS (sf)
#' @param distance_m  the distance, in the units of the CRS
#' @return a list, element i holding the indices within the distance of i
within_distance <- function(sites, distance_m) {
  geometry <- sf::st_geometry(sites)
  # A buffer is a polygon inscribed in the true buffer, so it is widened a
  # little to keep every pair at exactly the distance among the candidates.
  candidates <- sf::st_intersects(sf::st_buffer(geometry, distance_m * 1.01 + 0.01), geometry)
  i <- rep(seq_along(candidates), lengths(candidates))
  j <- unlist(candidates)
  d <- sf::st_distance(geometry[i], geometry[j], by_element = TRUE)
  keep <- as.numeric(d) <= distance_m
  out <- split(j[keep], factor(i[keep], levels = seq_along(geometry)))
  unname(lapply(out, as.integer))
}

#' Reconstruct persistent sites from the site geometries.
#'
#' @param sites       site geometries with ID_LOKAL (sf)
#' @param distance_m  geometries closer than this (in metres) are one site;
#'                    0 merges only geometries that touch or overlap.
#'                    config.yml `sites: match_distance_m`
#' @return a tibble with ID_LOKAL and SITE_ID, one row per site geometry
match_sites <- function(sites, distance_m = CONFIG$sites$match_distance_m) {
  checkmate::assert_number(distance_m, lower = 0)
  neighbours <- if (distance_m == 0) {
    sf::st_intersects(sites)
  } else {
    within_distance(sites, distance_m)
  }
  component <- connected_components(neighbours)
  tibble::tibble(
    ID_LOKAL = sites$ID_LOKAL,
    SITE_ID = sprintf("S%05d", component)
  )
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

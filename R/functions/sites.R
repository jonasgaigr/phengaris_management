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
    sf::st_is_within_distance(sites, sites, dist = distance_m)
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

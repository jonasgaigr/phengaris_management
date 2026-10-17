#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: the populations known before and around a site
#
#----------------------------------------------------------#
#
# Two questions of Phase 4 need to know where each species has been found,
# from 2012 on and from every source:
#
#   P4.3  Is an imputed absence plausible? An absence imputed at a visit
#         where only the other species was recorded is believable only where
#         the imputed species could have been there: within its flight
#         season (the flight curve, P4.1) and near a known population of it
#         (Phillips et al. 2009: a record of another species shows effort,
#         not absence).
#   P4.8  Did the populations known in 2012-2018 persist into 2019-2024,
#         inside and outside the Natura 2000 sites designated for them?
#
# site_history_table() answers both for every surveyed site of 2019-2024 and
# each species: the distance to the nearest positive record of the species
# in 2012-2024 (any source, any year), and whether a positive record of
# 2012-2018 lies within the matching distance of the site.
#
#----------------------------------------------------------#

#' Distance from each point to the nearest of a set of points.
#'
#' @param x,y    coordinates of the points (m)
#' @param tx,ty  coordinates of the targets (m)
#' @return distances in km; Inf when there is no target
nearest_distance_km <- function(x, y, tx, ty) {
  if (!length(tx)) {
    return(rep(Inf, length(x)))
  }
  vapply(seq_along(x), function(i) sqrt(min((tx - x[i])^2 + (ty - y[i])^2)), numeric(1)) / 1000
}

#' The known populations around every surveyed site.
#'
#' @param site_design      site_design_table() (SITE_X, SITE_Y of each site)
#' @param sites            site geometries of 2019-2024 (sf), sites.gpkg
#' @param records          records of 2019-2024 (records.csv)
#' @param records_history  records of 2012-2018 (records_2012_2018.csv)
#' @param sites_history    site geometries of 2012-2018 (sf)
#' @param surveyed         ID_LOKAL of the sites to describe
#' @param distance_m       config.yml `sites: match_distance_m`
#' @return one row per surveyed site and species: NEAREST_POSITIVE_KM, the
#'         distance from the site's representative point to the nearest
#'         positive record of the species of 2012-2024 (its coordinates);
#'         HIST_POSITIVE, 1 when a site with a positive record of the species
#'         in 2012-2018 lies within `distance_m` of the site (0 otherwise);
#'         HIST_LAST_YEAR, the last such year
site_history_table <- function(site_design, sites, records, records_history, sites_history, surveyed,
                               distance_m = CONFIG$sites$match_distance_m) {
  positives <- dplyr::bind_rows(
    dplyr::mutate(records, PERIOD = "2019-2024"),
    dplyr::mutate(records_history, PERIOD = "2012-2018")
  ) %>%
    dplyr::filter(NEGATIV == 0, !is.na(X), !is.na(Y)) %>%
    dplyr::mutate(YEAR = as.integer(substr(DATUM_OD, 1, 4)))
  focal <- site_design %>%
    dplyr::filter(ID_LOKAL %in% surveyed) %>%
    dplyr::select(ID_LOKAL, SITE_X, SITE_Y)
  focal_sites <- sites[sites$ID_LOKAL %in% focal$ID_LOKAL, "ID_LOKAL"]

  dplyr::bind_rows(lapply(c(SPECIES_NAU, SPECIES_TEL), function(species) {
    pos <- positives[positives$DRUH == species, ]
    old <- pos[pos$PERIOD == "2012-2018", ]
    hits <- NULL
    if (nrow(old)) {
      old_sites <- sites_history %>%
        dplyr::select(ID_LOKAL) %>%
        dplyr::inner_join(
          old %>%
            dplyr::group_by(ID_LOKAL) %>%
            dplyr::summarise(last_year = max(YEAR), .groups = "drop"),
          by = "ID_LOKAL"
        )
      hits <- sf::st_is_within_distance(focal_sites, old_sites, dist = distance_m)
    }
    last_year <- if (is.null(hits)) {
      rep(NA_integer_, nrow(focal_sites))
    } else {
      vapply(hits, function(h) if (length(h)) as.integer(max(old_sites$last_year[h])) else NA_integer_, integer(1))
    }
    history <- tibble::tibble(ID_LOKAL = focal_sites$ID_LOKAL, HIST_LAST_YEAR = last_year)
    focal %>%
      dplyr::mutate(
        DRUH = species,
        NEAREST_POSITIVE_KM = nearest_distance_km(SITE_X, SITE_Y, pos$X, pos$Y)
      ) %>%
      dplyr::left_join(history, by = "ID_LOKAL") %>%
      dplyr::mutate(HIST_POSITIVE = as.numeric(!is.na(HIST_LAST_YEAR))) %>%
      dplyr::select(ID_LOKAL, DRUH, NEAREST_POSITIVE_KM, HIST_POSITIVE, HIST_LAST_YEAR)
  }))
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

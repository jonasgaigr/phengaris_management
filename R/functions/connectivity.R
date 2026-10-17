#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: site design and connectivity
#
#----------------------------------------------------------#
#
# site_design_table() describes every site geometry once: its persistent site
# (match_sites(), at the configured distance and at the sensitivity
# distances), its geometry type, its area and a representative point.
#
# connectivity_table() gives every surveyed site its connectivity to the
# populations of each species around it (P3.5), the incidence function model
# index
#
#   S_i = sum_j exp(-alpha * d_ij) * A_j ^ b
#
# over the sites j where the species was found in the same or the previous
# year, other than the persistent site of i itself. d_ij is in km between
# representative points, A_j in ha. Point and line sites have no area and
# count with the median area of the species' occupied polygon sites. It is
# computed at every alpha x b pair of the profile likelihood of step 17
# (P4.2, connectivity_grid()).
#
#----------------------------------------------------------#

#--------------------------------------------------#
## Site design -----
#--------------------------------------------------#

#' Name of the persistent-site column built at a distance.
site_id_column <- function(distance_m) {
  if (distance_m == CONFIG$sites$match_distance_m) "SITE_ID" else sprintf("SITE_ID_%gM", distance_m)
}

#' One row per site geometry: persistent sites, geometry type, area, position.
#'
#' @param sites        site geometries with ID_LOKAL (sf), sites.gpkg of step 01
#' @param distance_m   config.yml `sites: match_distance_m`
#' @param sensitivity  config.yml `sites: sensitivity_distances_m`
#' @param elevation    the elevation model (a SpatRaster in S-JTSK, metres),
#'                     or NULL for none
#' @return a tibble: ID_LOKAL, SITE_ID and SITE_ID_<d>M for each sensitivity
#'         distance, GEOMETRY_TYPE ("point", "line", "polygon"), SITE_AREA_HA
#'         (NA for points and lines), SITE_X and SITE_Y (a point on the
#'         surface, S-JTSK), and ELEVATION at that point (NA without an
#'         elevation model)
site_design_table <- function(sites, distance_m = CONFIG$sites$match_distance_m,
                              sensitivity = unlist(CONFIG$sites$sensitivity_distances_m),
                              elevation = NULL) {
  type <- as.character(sf::st_geometry_type(sites))
  geometry_type <- dplyr::case_when(
    type %in% c("POINT", "MULTIPOINT") ~ "point",
    type %in% c("LINESTRING", "MULTILINESTRING") ~ "line",
    type %in% c("POLYGON", "MULTIPOLYGON") ~ "polygon"
  )
  area <- units::drop_units(sf::st_area(sites)) / 10000
  points <- sf::st_coordinates(suppressWarnings(sf::st_point_on_surface(sf::st_geometry(sites))))

  out <- tibble::tibble(
    ID_LOKAL = sites$ID_LOKAL,
    GEOMETRY_TYPE = geometry_type,
    SITE_AREA_HA = ifelse(geometry_type == "polygon", area, NA_real_),
    SITE_X = points[, "X"],
    SITE_Y = points[, "Y"],
    ELEVATION = if (is.null(elevation)) {
      NA_real_
    } else {
      terra::extract(elevation, unname(points[, c("X", "Y"), drop = FALSE]))[, 1]
    }
  )
  for (d in unique(c(distance_m, sensitivity))) {
    out[[site_id_column(d)]] <- match_sites(sites, d)$SITE_ID
  }
  out %>%
    dplyr::relocate(dplyr::starts_with("SITE_ID"), .after = ID_LOKAL)
}

#' Persistent sites at each matching distance, and how many recur.
#'
#' @param site_design  site_design_table()
#' @param records      records with ID_LOKAL and DATUM_OD
summarise_persistent_sites <- function(site_design, records) {
  years <- records %>%
    dplyr::distinct(ID_LOKAL, YEAR = as.integer(substr(DATUM_OD, 1, 4)))
  columns <- grep("^SITE_ID", names(site_design), value = TRUE)
  dplyr::bind_rows(lapply(columns, function(col) {
    per_site <- site_design %>%
      dplyr::select(ID_LOKAL, site = dplyr::all_of(col)) %>%
      dplyr::inner_join(years, by = "ID_LOKAL") %>%
      dplyr::group_by(site) %>%
      dplyr::summarise(years = dplyr::n_distinct(YEAR), geometries = dplyr::n_distinct(ID_LOKAL))
    tibble::tibble(
      column = col,
      sites = nrow(per_site),
      in_more_than_one_year = sum(per_site$years > 1),
      percent_recurring = round(100 * mean(per_site$years > 1), 1),
      max_geometries_per_site = max(per_site$geometries)
    )
  }))
}

#--------------------------------------------------#
## Connectivity -----
#--------------------------------------------------#

#' The incidence function model connectivity of focal sites.
#'
#' @param focal    focal sites: SITE_X, SITE_Y (m) and SITE_ID
#' @param sources  occupied sites: SITE_X, SITE_Y, SITE_ID and AREA_HA
#' @param alpha    1 / mean dispersal distance, per km
#' @param b        exponent of the source area
#' @param max_km   sources farther than this are ignored
#' @return S for every focal site, 0 where no source is in reach
connectivity_index <- function(focal, sources, alpha, b, max_km) {
  if (!nrow(sources)) {
    return(rep(0, nrow(focal)))
  }
  vapply(seq_len(nrow(focal)), function(i) {
    d <- sqrt((sources$SITE_X - focal$SITE_X[i])^2 + (sources$SITE_Y - focal$SITE_Y[i])^2) / 1000
    use <- d <= max_km & sources$SITE_ID != focal$SITE_ID[i]
    sum(exp(-alpha * d[use]) * sources$AREA_HA[use]^b)
  }, numeric(1))
}

#' Name of the connectivity column for one alpha and b.
connectivity_column <- function(alpha, b) {
  paste0("CONN_A", formatC(alpha, format = "fg"), "_B", formatC(b, format = "fg"))
}

#' The alpha x b pairs of the profile (P4.2), with the fixed pair.
#'
#' @param settings  config.yml `connectivity`
#' @return a data frame of alpha and b, one row per pair
connectivity_grid <- function(settings = CONFIG$connectivity) {
  grid <- expand.grid(
    alpha = unlist(settings$alpha_grid_per_km),
    b = unlist(settings$area_exponent_grid)
  )
  unique(rbind(grid, data.frame(alpha = settings$alpha_per_km, b = settings$area_exponent)))
}

#' Connectivity of every surveyed site, by species and year.
#'
#' @param records      occurrence records (records.csv of step 01)
#' @param site_design  site_design_table()
#' @param focal_sources ZDROJ values of the surveyed records whose sites get
#'                     a connectivity
#' @param settings     config.yml `connectivity`
#' @return one row per surveyed site (ID_LOKAL), year and species: CONN at
#'         the fixed alpha and b, CONN_A<alpha>_B<b> at every pair of
#'         connectivity_grid(), and CONN_SOURCES, the occupied sites within
#'         reach
connectivity_table <- function(records, site_design, focal_sources = target_mon_zdroj,
                               settings = CONFIG$connectivity) {
  pairs <- connectivity_grid(settings)
  records <- records %>%
    dplyr::mutate(YEAR = as.integer(substr(DATUM_OD, 1, 4))) %>%
    dplyr::left_join(site_design, by = "ID_LOKAL")

  focal_sites <- records %>%
    dplyr::filter(ZDROJ %in% focal_sources) %>%
    dplyr::distinct(ID_LOKAL, YEAR, SITE_ID, SITE_X, SITE_Y)

  occupied <- records %>%
    dplyr::filter(
      NEGATIV == 0,
      settings$sources == "all" | ZDROJ %in% target_mon_zdroj
    )

  out <- list()
  for (species in c(SPECIES_NAU, SPECIES_TEL)) {
    occ <- occupied %>%
      dplyr::filter(DRUH == species)
    fallback_area <- stats::median(
      occ %>% dplyr::distinct(ID_LOKAL, SITE_AREA_HA) %>% dplyr::pull(SITE_AREA_HA),
      na.rm = TRUE
    )
    for (year in sort(unique(focal_sites$YEAR))) {
      focal <- focal_sites %>%
        dplyr::filter(YEAR == year)
      # One source per persistent site in the window, at its largest
      # geometry.
      sources <- occ %>%
        dplyr::filter(YEAR %in% c(year - 1, year)) %>%
        dplyr::distinct(ID_LOKAL, SITE_ID, SITE_X, SITE_Y, SITE_AREA_HA) %>%
        dplyr::mutate(AREA_HA = dplyr::coalesce(SITE_AREA_HA, fallback_area)) %>%
        dplyr::arrange(SITE_ID, dplyr::desc(AREA_HA)) %>%
        dplyr::distinct(SITE_ID, .keep_all = TRUE)

      block <- focal %>%
        dplyr::select(ID_LOKAL, YEAR) %>%
        dplyr::mutate(DRUH = species)
      block$CONN <- connectivity_index(
        focal, sources, settings$alpha_per_km, settings$area_exponent, settings$max_distance_km
      )
      for (i in seq_len(nrow(pairs))) {
        block[[connectivity_column(pairs$alpha[i], pairs$b[i])]] <- connectivity_index(
          focal, sources, pairs$alpha[i], pairs$b[i], settings$max_distance_km
        )
      }
      # Occupied sites within reach, whatever their distance weight.
      block$CONN_SOURCES <- connectivity_index(
        focal, dplyr::mutate(sources, AREA_HA = 1), 0, 0, settings$max_distance_km
      )
      out[[length(out) + 1]] <- block
    }
  }
  dplyr::bind_rows(out) %>%
    dplyr::relocate(CONN_SOURCES, .after = dplyr::last_col())
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: protected-area membership
#
#----------------------------------------------------------#
#
# A site counts as inside a protected area when its geometry intersects the
# area at all. Three kinds of area are distinguished:
#
#   EVL_any     any Natura 2000 site of Community importance (EVL)
#   EVL_target  an EVL designated for a Phengaris species; PA_SPECIES says
#               which species the EVL was designated for
#   MZCHU       a small-scale specially protected area
#
#----------------------------------------------------------#

#' Site identifiers falling inside a protected-area layer.
#'
#' DRUH is the species of the intersecting *record*, not the species the area
#' was designated for; PA_SPECIES carries the latter.
#'
#' @param sites       records with site geometry (sf), with DRUH and ID_LOKAL
#' @param areas       protected-area polygons
#' @param pa_type     label written into the PA_TYPE column
#' @param pa_species  species the areas were designated for, if any
#' @return one row per record x intersecting area, without geometry
protected_area_ids <- function(sites, areas, pa_type, pa_species = NA_character_) {
  # Attributes are declared constant over each geometry, which they are,
  # so that sf does not warn about it on every intersection.
  sf::st_set_agr(sites, "constant") %>%
    sf::st_intersection(
      .,
      sf::st_set_agr(areas, "constant")
    ) %>%
    sf::st_make_valid() %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      PA_TYPE    = pa_type,
      PA_SPECIES = pa_species
    ) %>%
    dplyr::select(
      ID_LOKAL,
      DRUH,
      PA_TYPE,
      PA_SPECIES
    )
}

#' Natura 2000 sites designated for one species.
#'
#' @param evl             the EVL polygons, with SITECODE
#' @param sites_subjects  the Natura 2000 site-subject table (site_code,
#'                        nazev_lat)
#' @param n2k_name        the species' name in that table, which still uses
#'                        the former genus Maculinea
evl_designated_for <- function(evl, sites_subjects, n2k_name) {
  evl %>%
    dplyr::filter(
      SITECODE %in% dplyr::filter(
        sites_subjects,
        nazev_lat == n2k_name
      )$site_code
    )
}

#' Protected-area membership of every record's site.
#'
#' @param sites           records with site geometry (sf)
#' @param evl,mzchu       the protected-area polygons, in the CRS of `sites`
#' @param sites_subjects  the Natura 2000 site-subject table
#' @return the rows of EVL_any, then EVL_target for each species, then MZCHU
protected_area_membership <- function(sites, evl, mzchu, sites_subjects) {
  dplyr::bind_rows(
    protected_area_ids(sites, evl, "EVL_any"),
    protected_area_ids(
      sites,
      evl_designated_for(evl, sites_subjects, CONFIG$species$nausithous$n2k_name),
      "EVL_target",
      pa_species = SPECIES_NAU
    ),
    protected_area_ids(
      sites,
      evl_designated_for(evl, sites_subjects, CONFIG$species$teleius$n2k_name),
      "EVL_target",
      pa_species = SPECIES_TEL
    ),
    protected_area_ids(sites, mzchu, "MZCHU")
  )
}

#' Records and distinct sites by protection category.
summarise_membership <- function(protected_area_id) {
  protected_area_id %>%
    dplyr::group_by(PA_TYPE, DRUH) %>%
    dplyr::summarise(
      records = dplyr::n(),
      sites   = dplyr::n_distinct(ID_LOKAL),
      .groups = "drop"
    )
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

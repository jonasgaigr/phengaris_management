#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#      Step 03 - Identify sites inside protected areas
#
#
#----------------------------------------------------------#
#
# Flags every monitoring site by the protection it falls under:
#
#   EVL_any     inside any Natura 2000 site of Community importance
#   EVL_target  inside a Natura 2000 site designated for that species
#   MZCHU       inside a small-scale specially protected area
#
# Reads:  objects from step 01 (phengaris_lokal_new, evl, mzchu, sites_subjects)
# Writes: Data/Processed/protected_area_id.csv
#
#----------------------------------------------------------#

message("Step 03: identifying protected areas")

report_start(
  "03",
  "Protected area membership",
  paste(
    "Monitoring sites intersected with Natura 2000 sites and small-scale",
    "specially protected areas. A site counts as EVL_target when the Natura",
    "2000 site it falls in was designated for that particular species."
  )
)

#----------------------------------------------------------#
# Helper -----
#----------------------------------------------------------#

#' Site identifiers falling inside a protected-area layer.
#'
#' DRUH is the species of the intersecting *record*, not the species the area
#' was designated for. PA_SPECIES carries the latter, so that a species-specific
#' EVL_target flag can be built later if the manuscript needs one.
#'
#' @param sites       site geometries with DRUH and ID_LOKAL
#' @param areas       protected-area polygons
#' @param pa_type     label written into the PA_TYPE column
#' @param pa_species  species the areas were designated for, if any
protected_area_ids <- function(sites, areas, pa_type, pa_species = NA_character_) {
  sites %>%
    sf::st_intersection(
      .,
      areas
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
#' The site-subject table still uses the former genus name Maculinea.
evl_designated_for <- function(latin_name) {
  evl %>%
    dplyr::filter(
      SITECODE %in% dplyr::filter(
        sites_subjects,
        nazev_lat == latin_name
      )$site_code
    )
}

#----------------------------------------------------------#
# Protected area membership -----
#----------------------------------------------------------#
#--------------------------------------------------#
## Natura 2000, any site -----
#--------------------------------------------------#

phengaris_evl_id <- protected_area_ids(
  phengaris_lokal_new,
  evl,
  "EVL_any"
)

#--------------------------------------------------#
## Natura 2000, designated for P. nausithous -----
#--------------------------------------------------#

evl_id_Pnau <- protected_area_ids(
  phengaris_lokal_new,
  evl_designated_for("Maculinea nausithous"),
  "EVL_target",
  pa_species = SPECIES_NAU
)

#--------------------------------------------------#
## Natura 2000, designated for P. teleius -----
#--------------------------------------------------#

evl_id_Ptel <- protected_area_ids(
  phengaris_lokal_new,
  evl_designated_for("Maculinea teleius"),
  "EVL_target",
  pa_species = SPECIES_TEL
)

#--------------------------------------------------#
## Small-scale specially protected areas -----
#--------------------------------------------------#

phengaris_mzchu_id <- protected_area_ids(
  phengaris_lokal_new,
  mzchu,
  "MZCHU"
)

#----------------------------------------------------------#
# Combine -----
#----------------------------------------------------------#

protected_area_id <-
  dplyr::bind_rows(
    phengaris_evl_id,
    evl_id_Pnau,
    evl_id_Ptel,
    phengaris_mzchu_id
  )

#----------------------------------------------------------#
# Results -----
#----------------------------------------------------------#

protection_summary <-
  protected_area_id %>%
  dplyr::group_by(PA_TYPE, DRUH) %>%
  dplyr::summarise(
    records = dplyr::n(),
    sites   = dplyr::n_distinct(ID_LOKAL),
    .groups = "drop"
  )

report_table(
  protection_summary,
  "Records and distinct sites by protection category",
  "03_protection_summary"
)

#----------------------------------------------------------#
# Export -----
#----------------------------------------------------------#

readr::write_csv(
  protected_area_id,
  file.path(PATHS$processed, "protected_area_id.csv")
)

report_finish()

message("Step 03 done: ", nrow(protected_area_id), " protected-area assignments")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

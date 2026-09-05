#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: descriptive summary tables
#
#----------------------------------------------------------#
#
# One function per summary table. Step 07 reports them, step 08 plots them and
# step 12 plots the model-related ones, so each step can be run on its own
# without depending on objects another step happened to leave in the session.
#
#----------------------------------------------------------#

#--------------------------------------------------#
## General and temporal -----
#--------------------------------------------------#

#' Share of positive records by species and year.
summarise_positivity <- function(data) {
  data %>%
    dplyr::group_by(
      DRUH,
      YEAR
    ) %>%
    dplyr::reframe(
      records       = dplyr::n(),
      mean_positive = mean(POSITIVE)
    )
}

#' Records by species and occupancy.
summarise_occupancy <- function(data) {
  data %>%
    dplyr::group_by(DRUH, POSITIVE) %>%
    dplyr::summarise(COUNT = dplyr::n(), .groups = "drop")
}

#' Number of monitored years per locality.
summarise_years_per_locality <- function(data) {
  data %>%
    dplyr::group_by(NAZ_LOKAL) %>%
    dplyr::summarise(roky = length(unique(YEAR)), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(roky))
}

#' Records by year and occupancy.
summarise_year_stats <- function(data) {
  data %>%
    dplyr::group_by(YEAR, POSITIVE) %>%
    dplyr::summarise(COUNT = dplyr::n(), .groups = "drop")
}

#--------------------------------------------------#
## Observers -----
#--------------------------------------------------#

#' Number of records per observer, most active first.
summarise_observers <- function(data) {
  data %>%
    dplyr::group_by(
      AUTOR
    ) %>%
    dplyr::reframe(
      obs_num = dplyr::n()
    ) %>%
    dplyr::arrange(
      dplyr::desc(
        obs_num
      )
    )
}

#' Mapping fields covered per observer and year.
summarise_fields_per_observer <- function(data) {
  data %>%
    dplyr::group_by(ID_LOKAL) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(YEAR, AUTOR) %>%
    dplyr::summarise(fields = length(unique(SITMAP)), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(fields))
}

#' Sites monitored per mapping field.
summarise_sites_per_field <- function(data) {
  data %>%
    dplyr::group_by(ID_LOKAL) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SITMAP) %>%
    dplyr::summarise(sites = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(sites)
}

#--------------------------------------------------#
## Occurrence and abundance -----
#--------------------------------------------------#

#' Records by occupancy, species and co-occurrence of the other species.
summarise_species_cooccurrence <- function(data) {
  data %>%
    dplyr::group_by(POSITIVE, DRUH, SPEC_NUM) %>%
    dplyr::summarise(COUNT = dplyr::n(), .groups = "drop")
}

#' Counted specimens per species: n, mean, median and standard deviation.
summarise_abundance <- function(data) {
  data %>%
    dplyr::filter(!is.na(POCET)) %>%
    dplyr::group_by(DRUH) %>%
    dplyr::summarise(
      records = dplyr::n(),
      mean    = mean(POCET),
      median  = stats::median(POCET),
      sd      = stats::sd(POCET),
      .groups = "drop"
    )
}

#--------------------------------------------------#
## Habitat and host plant -----
#--------------------------------------------------#

#' Recorded habitat types by species and occupancy.
summarise_habitat_counts <- function(data) {
  data %>%
    dplyr::group_by(
      DRUH,
      POSITIVE
    ) %>%
    dplyr::reframe(
      TTP    = sum(TTP == 1, na.rm = TRUE),
      ZARUST = sum(ZARUST == 1, na.rm = TRUE),
      PRIKOP = sum(PRIKOP == 1, na.rm = TRUE),
      JINY   = sum(JINY == 1, na.rm = TRUE)
    )
}

#' Recorded habitat types at occupied sites of one species, long format.
#'
#' The percentage is out of all occupied records of that species.
summarise_recorded_habitats <- function(data, species) {
  denominator <- nrow(
    data %>%
      dplyr::filter(DRUH == species) %>%
      dplyr::filter(POSITIVE == 1)
  )

  data %>%
    dplyr::select(DRUH, POSITIVE, TTP, ZARUST, PRIKOP, JINY) %>%
    tidyr::pivot_longer(cols = c(TTP, ZARUST, PRIKOP, JINY)) %>%
    dplyr::filter(value == 1) %>%
    dplyr::select(-value) %>%
    dplyr::filter(POSITIVE == 1) %>%
    dplyr::filter(DRUH == species) %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(
      COUNT   = dplyr::n(),
      PERC    = dplyr::n() / denominator * 100,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      name = dplyr::case_when(
        name == "TTP"    ~ "managed\ngrassland",
        name == "ZARUST" ~ "neglected\ngrassland",
        name == "JINY"   ~ "other",
        name == "PRIKOP" ~ "road verge,\nditch"
      )
    )
}

#' Host plant abundance by occupancy and species.
summarise_host_plant <- function(data) {
  data %>%
    dplyr::group_by(POSITIVE, DRUH, PLANT_QUANT) %>%
    dplyr::summarise(COUNT = dplyr::n(), .groups = "drop")
}

#--------------------------------------------------#
## Management -----
#--------------------------------------------------#

#' Records by mowing method.
summarise_method <- function(data) {
  data %>%
    dplyr::group_by(POSITIVE, DRUH, METHOD) %>%
    dplyr::summarise(COUNT = dplyr::n(), .groups = "drop")
}

#' Records by mowing timing.
summarise_timing <- function(data) {
  data %>%
    dplyr::group_by(POSITIVE, DRUH, TIMING) %>%
    dplyr::reframe(
      COUNT = dplyr::n()
    )
}

#' Records by the combination of mowing method and timing.
summarise_management <- function(data) {
  data %>%
    dplyr::mutate(
      MANAGEMENT = dplyr::case_when(
        TIMING == 1 & METHOD == 1 ~ "appropriate mow and appropriate timing",
        TIMING == 0 & METHOD == 1 ~ "appropriate mow only",
        TIMING == 1 & METHOD == 0 ~ "appropriate timing only",
        TIMING == 0 & METHOD == 0 ~ "inappropriate mow and inappropriate timing"
      )
    ) %>%
    dplyr::group_by(
      POSITIVE,
      DRUH,
      MANAGEMENT
    ) %>%
    dplyr::reframe(
      COUNT = dplyr::n()
    )
}

#' Management recorded at occupied sites of one species, long format.
summarise_management_types <- function(data, species) {
  denominator <- nrow(
    data %>%
      dplyr::filter(DRUH == species) %>%
      dplyr::filter(POSITIVE == 1)
  )

  data %>%
    dplyr::select(DRUH, POSITIVE, MOW, GRAZE, ZARUST) %>%
    tidyr::pivot_longer(cols = c(MOW, GRAZE, ZARUST)) %>%
    dplyr::filter(value == 1) %>%
    dplyr::select(-value) %>%
    dplyr::filter(POSITIVE == 1) %>%
    dplyr::filter(DRUH == species) %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(
      COUNT   = dplyr::n(),
      PERC    = dplyr::n() / denominator * 100,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      name = dplyr::case_when(
        name == "MOW"    ~ "mowing",
        name == "ZARUST" ~ "neglected\ngrassland",
        name == "GRAZE"  ~ "grazing"
      )
    )
}

#--------------------------------------------------#
## Protected areas and mapping grid -----
#--------------------------------------------------#

#' Records by every protection variable at once.
summarise_protection <- function(data) {
  data %>%
    dplyr::group_by(
      EVL,
      EVL_target,
      EVL_comb,
      MZCHU,
      POSITIVE,
      DRUH
    ) %>%
    dplyr::summarise(COUNT = dplyr::n(), .groups = "drop")
}

#' Records by Natura 2000 membership.
summarise_evl <- function(data) {
  data %>%
    dplyr::group_by(EVL, POSITIVE, DRUH) %>%
    dplyr::summarise(COUNT = dplyr::n(), .groups = "drop")
}

#' Records by whether the Natura 2000 site is designated for Phengaris.
#'
#' The original scripts plotted this table but never built it, so the two
#' figures using it could not be produced.
summarise_evl_target <- function(data) {
  data %>%
    dplyr::group_by(EVL_target, POSITIVE, DRUH) %>%
    dplyr::summarise(COUNT = dplyr::n(), .groups = "drop")
}

#' Records by the combined Natura 2000 status.
summarise_evl_combined <- function(data) {
  data %>%
    dplyr::group_by(EVL_comb, POSITIVE, DRUH) %>%
    dplyr::summarise(COUNT = dplyr::n(), .groups = "drop")
}

#' Records by small-scale protected area membership.
summarise_mzchu <- function(data) {
  data %>%
    dplyr::group_by(MZCHU, POSITIVE, DRUH) %>%
    dplyr::summarise(COUNT = dplyr::n(), .groups = "drop")
}

#' One row per species and range polygon, occupied where the species occurs.
summarise_mapping_fields <- function(data) {
  data %>%
    dplyr::select(
      DRUH,
      POSITIVE,
      row_n
    ) %>%
    dplyr::group_by(
      DRUH,
      row_n
    ) %>%
    dplyr::arrange(
      dplyr::desc(
        POSITIVE
      )
    ) %>%
    dplyr::slice(
      1
    ) %>%
    dplyr::ungroup()
}

#' Distinct mapping fields covered, overall and per occupied species.
summarise_grid_coverage <- function(data) {
  data.frame(
    subset = c(
      "all monitored records",
      "occupied P. nausithous records",
      "occupied P. teleius records"
    ),
    mapping_fields = c(
      dplyr::n_distinct(data$SITMAP),
      dplyr::n_distinct(
        data$SITMAP[data$DRUH == SPECIES_NAU & data$POSITIVE == 1]
      ),
      dplyr::n_distinct(
        data$SITMAP[data$DRUH == SPECIES_TEL & data$POSITIVE == 1]
      )
    ),
    stringsAsFactors = FALSE
  )
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

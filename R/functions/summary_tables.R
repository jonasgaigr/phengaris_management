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
        name == "TTP" ~ "managed\ngrassland",
        name == "ZARUST" ~ "neglected\ngrassland",
        name == "JINY" ~ "other",
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
        name == "MOW" ~ "mowing",
        name == "ZARUST" ~ "neglected\ngrassland",
        name == "GRAZE" ~ "grazing"
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

#--------------------------------------------------#
## The records of the revised models -----
#--------------------------------------------------#

# These need the survey-design variables of the revised specification
# (design_variables: true): the flight season, the imputed absences that are
# plausible (P4.3) and the protection designated for the record's species.

#' The outcomes of a record, as the revised models see it.
RECORD_OUTCOMES <- c(
  "positive", "observed negative", "imputed negative, plausible", "imputed negative, not plausible"
)

#' The outcome of each record: the species found, not found at an observed
#' visit, or an imputed absence, plausible or not.
record_outcome <- function(positive, imputed, plausible) {
  factor(
    dplyr::case_when(
      positive == 1 ~ RECORD_OUTCOMES[1],
      !imputed ~ RECORD_OUTCOMES[2],
      plausible %in% TRUE ~ RECORD_OUTCOMES[3],
      TRUE ~ RECORD_OUTCOMES[4]
    ),
    levels = RECORD_OUTCOMES
  )
}

#' Jeffreys 95% interval of a proportion of x out of n.
jeffreys_interval <- function(x, n) {
  data.frame(
    low = ifelse(x == 0, 0, stats::qbeta(0.025, x + 0.5, n - x + 0.5)),
    high = ifelse(x == n, 1, stats::qbeta(0.975, x + 0.5, n - x + 0.5))
  )
}

#' Records in the flight season by species, year and outcome.
summarise_records_by_outcome <- function(data) {
  data %>%
    dplyr::filter(IN_FLIGHT_WINDOW) %>%
    dplyr::mutate(outcome = record_outcome(POSITIVE, IMPUTED, IMPUTED_PLAUSIBLE)) %>%
    dplyr::count(DRUH, YEAR, outcome, name = "records", .drop = FALSE) %>%
    dplyr::arrange(DRUH, YEAR, outcome)
}

#' Observed visits by species, position in the season (classes of DOY_DEV,
#' the days from the year's median positive record), outcome and whether
#' they fall in the flight season.
summarise_visit_timing <- function(data, width = 5) {
  data %>%
    dplyr::filter(!IMPUTED, !is.na(DOY_DEV)) %>%
    dplyr::mutate(
      day_class = width * floor(DOY_DEV / width),
      outcome = ifelse(POSITIVE == 1, "positive", "negative")
    ) %>%
    dplyr::count(DRUH, day_class, outcome, in_flight_season = IN_FLIGHT_WINDOW, name = "records") %>%
    dplyr::arrange(DRUH, day_class, outcome)
}

#' The variables of the management model and the labels of their values.
MANAGEMENT_VARIABLES <- list(
  TIMING = c(`0` = "harmful", `1` = "appropriate"),
  METHOD = c(`0` = "whole area mown", `1` = "patches left uncut"),
  PLANT_QUANT = c(`1` = "single plants", `2` = "abundant", `3` = "dominant"),
  EVL_species = c(`0` = "outside", `1` = "inside")
)

#' Variable names of MANAGEMENT_VARIABLES as table and figure headings.
MANAGEMENT_VARIABLE_LABELS <- c(
  TIMING = "mowing timing", METHOD = "mowing method", PLANT_QUANT = "host plant",
  EVL_species = "EVL designated for the species"
)

#' Share of the in-season records with the species at each value of the
#' management model's variables, per species and sample (the samples of
#' revised_model_data()), with a Jeffreys 95% interval.
summarise_share_positive <- function(data, variables = MANAGEMENT_VARIABLES) {
  rows <- list()
  for (species in sort(unique(data$DRUH))) {
    for (sample in REVISED_SAMPLES) {
      d <- revised_model_data(data, species, sample)
      for (v in names(variables)) {
        values <- variables[[v]]
        x <- as.character(d[[v]])
        keep <- x %in% names(values)
        tab <- data.frame(
          DRUH = species, sample = sample, variable = v,
          value = factor(values[x[keep]], levels = unname(values)),
          positive = d$POSITIVE[keep]
        ) %>%
          dplyr::group_by(DRUH, sample, variable, value) %>%
          dplyr::summarise(records = dplyr::n(), positive = sum(positive), .groups = "drop")
        rows[[length(rows) + 1]] <- dplyr::bind_cols(
          tab,
          share_positive = tab$positive / tab$records, jeffreys_interval(tab$positive, tab$records)
        )
      }
    }
  }
  dplyr::bind_rows(rows) %>%
    dplyr::mutate(value = as.character(value))
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

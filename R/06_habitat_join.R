#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#     Step 06 - Attach habitat attributes to the analysis table
#
#
#----------------------------------------------------------#
#
# Brings the habitat mapping attributes onto the cleaned occurrence records and
# writes the table the models are fitted on.
#
# Each specification chooses its site x segment intersection (config.yml,
# habitat_source): "frozen", the intersection of the original analysis, or
# "current", the site areas intersected with the current VMB layer by step 05
# (falling back to the frozen one when step 05 had no current layer).
#
# With the current layers a record takes its KVES information from the KVES
# version of its year (KVES_VERSION, config.yml `habitat:
# kves_version_of_year`), and a point or line site is described over the
# KVES polygon of that version it lies in (AREA_SOURCE "KVES"). The current
# intersections are joined to the records by ID_LOKAL and KVES_VERSION.
#
# A monitoring site can overlap many habitat segments, so they have to be
# reduced to one description per record. The rule is set per specification
# (config.yml, habitat_segment_rule) and implemented in
# select_habitat_segment(): "legacy" keeps the original rule (order by
# NATURAL, then by AREA_SITE, keep the first segment), "area_weighted"
# summarises all the segments of a site. With the current layers, the
# specification also gets the dominant KVES ecosystem class of the site
# (KVES_KOD, KVES_KATEGORIE; summarise_kves()).
#
# Variables added under the legacy rule:
#
#   BIOTOP, BIOTOP_SEZ, FSB, FSB_EVAL, HABITAT   habitat mapping attributes
#   AREA_SITE   area of the site x segment overlap, in m2 (AREA_real)
#   HET_OUT     between-habitat heterogeneity, i.e. the number of habitats
#               listed in the segment mosaic BIOTOP_SEZ
#   NATURAL     1 when a natural habitat is mapped, 0 for X codes and no data
#
# Reads:  Data/Processed/<spec>/data_clean.csv               (step 04)
#         Data/Processed/common/data_lokal_vmb.csv           (step 05, frozen)
#         Data/Processed/common/data_lokal_vmb_current.csv   (step 05, current)
#         Data/Processed/common/data_lokal_kves.csv          (step 05, KVES)
# Writes: Data/Processed/<spec>/data_analysis.csv
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("06")

report_start(
  "06",
  "Habitat attributes",
  paste(
    "The cleaned occurrence records joined to the habitat mapping layer, one",
    "habitat segment per record. This produces data_analysis.csv, the table",
    "every model in steps 09 to 11 is fitted on."
  )
)

data_clean <- read_data_clean()

habitat_source <- spec_settings()$habitat_source
overlaps <- choose_habitat_overlaps(
  habitat_source,
  current = processed_path("data_lokal_vmb_current.csv", "common"),
  frozen = processed_path("data_lokal_vmb.csv", "common")
)
path_kves <- processed_path("data_lokal_kves.csv", "common")

# The current intersections describe a site per KVES version.
current_key <- c("ID_LOKAL", "KVES_VERSION")
if (habitat_source == "current") {
  data_clean$KVES_VERSION <- kves_version_of(data_clean$YEAR)
}

#----------------------------------------------------------#
# Join the habitat attributes -----
#----------------------------------------------------------#

if (overlaps$source != "none") {
  habitat_segments <- read_habitat_overlaps(overlaps$path)
  key <- if (overlaps$source == "current") current_key else "ID_LOKAL"

  # How each record is described by the segments its site overlaps; the rule
  # is set per specification in config.yml. Only the current layer has one
  # row per segment x mapped habitat with its share; the frozen intersection
  # has one row per Natura habitat record, which no share describes.
  data_analysis <- select_habitat_segment(
    data_clean,
    habitat_segments,
    rule = spec_settings()$habitat_segment_rule,
    share_column = if (overlaps$source == "current") CONFIG$habitat$share_column else NULL,
    by = key
  )
  # Which area each record's site is described over, and for a polygon site
  # the share of it near its edge (P4.6).
  if (overlaps$source == "current") {
    data_analysis <- data_analysis %>%
      dplyr::left_join(dplyr::distinct(habitat_segments, dplyr::across(dplyr::all_of(key)), AREA_SOURCE), by = key)
    path_shape <- processed_path("site_area_shape.csv", "common")
    if (file.exists(path_shape)) {
      data_analysis <- data_analysis %>%
        dplyr::left_join(
          readr::read_csv2(path_shape, show_col_types = FALSE, progress = FALSE) %>%
            dplyr::transmute(ID_LOKAL, KVES_VERSION = as.character(KVES_VERSION), EDGE_SHARE),
          by = key
        )
    }
    stopifnot(nrow(data_analysis) == nrow(data_clean))
  }

  habitat_join_available <- TRUE

  if (habitat_source == "current" && overlaps$source == "frozen") {
    report_warning(
      "This specification asks for the current VMB layer, but step 05 had",
      "none, so the frozen intersection of the original analysis is used.",
      "Run R/tools/download_habitat_layers.R to make the local copy."
    )
  }
} else {
  # Keep the cascade running without habitat data: the analysis table is the
  # cleaned table with empty habitat columns, and the habitat models in later
  # steps will report themselves as skipped.
  data_analysis <-
    data_clean %>%
    dplyr::mutate(
      SEGMENT_ID = NA_integer_,
      BIOTOP = NA_character_,
      BIOTOP_SEZ = NA_character_,
      FSB = NA_character_,
      FSB_EVAL = NA_character_,
      HABITAT = NA_character_,
      STEJ_PR = NA_real_,
      SHAPE_Area = NA_real_,
      AREA_real = NA_real_,
      LENGTH_real = NA_real_,
      REPRE = NA_real_,
      ZACH = NA_real_,
      AREA_SITE = NA_real_,
      HET_OUT = NA_integer_,
      NATURAL = NA_integer_
    )

  habitat_join_available <- FALSE

  report_warning(
    "Step 05 wrote no site x habitat intersection, so no habitat",
    "attributes could be attached. The habitat columns are empty and the",
    "models that use AREA_SITE, FSB, BIOTOP or HET_OUT will be skipped.",
    "Run R/tools/download_habitat_layers.R, then step 05."
  )
}

# The KVES class of the site, with the current layers only: the frozen
# intersection of the original analysis had no ecosystem layer.
kves_join_available <- habitat_source == "current" && file.exists(path_kves)
if (kves_join_available) {
  data_analysis <- summarise_kves(data_analysis, read_kves_overlaps(path_kves), by = current_key)
}

#----------------------------------------------------------#
# Results -----
#----------------------------------------------------------#

habitat_rule <- spec_settings()$habitat_segment_rule

if (habitat_join_available && habitat_rule == "area_weighted") {
  report_warning(
    "Habitat attributes by the area-weighted rule, on",
    if (overlaps$source == "current") {
      paste(
        "the current VMB layer, each overlap weighted by the habitat's share",
        "of the segment (`habitat: share_column`). A point or line site is",
        "described over the KVES polygon it lies in, from the KVES version of",
        "the record's year, cut to", CONFIG$habitat$margin_m, "m around it.",
        "Step 05 checks the intersection against the frozen one on the",
        "polygon sites (05_current_vs_frozen)."
      )
    } else {
      paste(
        "the frozen site x segment intersection of the original analysis.",
        "Point sites have no area there: their AREA_SITE is 0 and their",
        "shares NA, and they get the habitat they lie in only when it is a",
        "single one."
      )
    },
    "The rule itself is tested on synthetic fixtures. NATURAL_SHARE and",
    "X_SHARE are the shares of the site in natural and in X habitats; X",
    "habitats are kept apart from the unmapped area, since they can hold the",
    "species. The Sanguisorba-meadow habitats are",
    paste0(paste(unlist(CONFIG$habitat$sanguisorba_meadow_codes), collapse = ", "), "."),
    "No revised model uses these variables."
  )

  report_table(
    data_analysis %>%
      dplyr::group_by(DRUH, GEOMETRY_TYPE = if ("GEOMETRY_TYPE" %in% names(data_analysis)) GEOMETRY_TYPE else NA) %>%
      dplyr::summarise(
        records = dplyr::n(),
        with_overlaps = sum(!is.na(AREA_SITE)),
        with_area = sum(AREA_SITE > 0, na.rm = TRUE),
        area_from_kves = if ("AREA_SOURCE" %in% names(data_analysis)) sum(AREA_SOURCE == "KVES", na.rm = TRUE) else NA,
        dominant_habitat_known = sum(!is.na(NATURAL)),
        median_area_ha = stats::median(AREA_SITE, na.rm = TRUE) / 10000,
        median_natural_share = stats::median(NATURAL_SHARE, na.rm = TRUE),
        median_x_share = stats::median(X_SHARE, na.rm = TRUE),
        median_sanguisorba_share = stats::median(SANGUISORBA_SHARE, na.rm = TRUE),
        records_with_sanguisorba_meadow = sum(SANGUISORBA_AREA > 0, na.rm = TRUE),
        median_habitats = stats::median(N_HABITATS, na.rm = TRUE),
        .groups = "drop"
      ),
    paste(
      "Coverage of the area-weighted habitat summaries by species and site",
      "geometry: records whose site overlaps the habitat layer, with an",
      "overlap area, with the area of their KVES polygon, and with a dominant",
      "habitat; median site area and shares of natural, X and",
      "Sanguisorba-meadow habitats"
    ),
    "06_area_weighted_coverage"
  )

  report_table(
    data_analysis %>%
      dplyr::group_by(DRUH) %>%
      dplyr::summarise(
        records = dplyr::n(),
        occupied = sum(POSITIVE == 1),
        natural_share_occupied = stats::median(NATURAL_SHARE[POSITIVE == 1], na.rm = TRUE),
        natural_share_unoccupied = stats::median(NATURAL_SHARE[POSITIVE == 0], na.rm = TRUE),
        x_share_occupied = stats::median(X_SHARE[POSITIVE == 1], na.rm = TRUE),
        x_share_unoccupied = stats::median(X_SHARE[POSITIVE == 0], na.rm = TRUE),
        with_sanguisorba_meadow_occupied = mean(SANGUISORBA_AREA[POSITIVE == 1] > 0, na.rm = TRUE),
        with_sanguisorba_meadow_unoccupied = mean(SANGUISORBA_AREA[POSITIVE == 0] > 0, na.rm = TRUE),
        with_wet_meadow_occupied = mean(WET_SHARE[POSITIVE == 1] > 0, na.rm = TRUE),
        with_wet_meadow_unoccupied = mean(WET_SHARE[POSITIVE == 0] > 0, na.rm = TRUE),
        edge_share_occupied = if ("EDGE_SHARE" %in% names(data_analysis)) {
          stats::median(EDGE_SHARE[POSITIVE == 1], na.rm = TRUE)
        } else {
          NA_real_
        },
        edge_share_unoccupied = if ("EDGE_SHARE" %in% names(data_analysis)) {
          stats::median(EDGE_SHARE[POSITIVE == 0], na.rm = TRUE)
        } else {
          NA_real_
        },
        .groups = "drop"
      ),
    paste(
      "Median shares of natural and X habitats, and the share of records with",
      "a Sanguisorba meadow and with a wet meadow, at occupied and unoccupied",
      "sites; the median share of a polygon site within the edge zone",
      "(P4.6). Descriptive; the habitat models of step 22 use them"
    ),
    "06_habitat_shares_by_occupancy"
  )

  report_table(
    data_analysis %>%
      dplyr::filter(POSITIVE == 1) %>%
      dplyr::count(DRUH, BIOTOP, name = "occupied_records") %>%
      dplyr::group_by(DRUH) %>%
      dplyr::slice_max(occupied_records, n = 10, with_ties = FALSE) %>%
      dplyr::ungroup(),
    "Dominant habitats at occupied sites, the ten most frequent per species",
    "06_dominant_habitats"
  )
}

if (kves_join_available) {
  report_table(
    data_analysis %>%
      dplyr::group_by(DRUH, KVES_KOD, KVES_KATEGORIE) %>%
      dplyr::summarise(
        records = dplyr::n(),
        occupied_records = sum(POSITIVE == 1),
        .groups = "drop"
      ) %>%
      dplyr::mutate(occupied_share = round(occupied_records / records, 3)) %>%
      dplyr::group_by(DRUH) %>%
      dplyr::slice_max(records, n = 10, with_ties = FALSE) %>%
      dplyr::ungroup(),
    paste(
      "Records and occupied records by the dominant KVES 2025 class of the",
      "site, the ten most frequent classes per species (NA: a point on a",
      "boundary between classes, or outside the layer). No model uses the class."
    ),
    "06_kves_classes"
  )
}

if (habitat_join_available && habitat_rule == "legacy") {
  habitat_coverage <- data.frame(
    variable = c("AREA_SITE", "BIOTOP", "FSB", "HET_OUT"),
    records_with_value = c(
      sum(!is.na(data_analysis$AREA_SITE)),
      sum(!is.na(data_analysis$BIOTOP)),
      sum(!is.na(data_analysis$FSB)),
      sum(!is.na(data_analysis$HET_OUT))
    ),
    records_total = nrow(data_analysis),
    stringsAsFactors = FALSE
  )
  habitat_coverage$percent <-
    round(100 * habitat_coverage$records_with_value / habitat_coverage$records_total, 1)

  report_table(
    habitat_coverage,
    "Coverage of the habitat variables in the analysis table",
    "06_habitat_coverage"
  )

  #--------------------------------------------------#
  ## Mapped habitats at occupied sites -----
  #--------------------------------------------------#

  #' Mapped habitats at occupied sites of one species.
  mapped_habitat_summary <- function(species) {
    data_analysis %>%
      dplyr::filter(DRUH == species, POSITIVE == 1) %>%
      dplyr::mutate(
        FSB = dplyr::case_when(is.na(FSB) == TRUE ~ "-", TRUE ~ FSB),
        BIOTOP = dplyr::case_when(is.na(BIOTOP) == TRUE ~ "-1", TRUE ~ BIOTOP)
      ) %>%
      dplyr::group_by(BIOTOP) %>%
      dplyr::summarise(
        AREA        = sum(AREA_SITE, na.rm = TRUE) / 10000,
        mean_area   = mean(AREA_SITE, na.rm = TRUE),
        median_area = stats::median(AREA_SITE, na.rm = TRUE),
        COUNT       = dplyr::n(),
        .groups     = "drop"
      ) %>%
      dplyr::distinct() %>%
      dplyr::arrange(dplyr::desc(COUNT))
  }

  habitat_nau <- mapped_habitat_summary(SPECIES_NAU)
  habitat_tel <- mapped_habitat_summary(SPECIES_TEL)

  report_table(
    habitat_nau,
    "Mapped habitats at occupied P. nausithous sites",
    "06_mapped_habitats_nausithous"
  )
  report_table(
    habitat_tel,
    "Mapped habitats at occupied P. teleius sites",
    "06_mapped_habitats_teleius"
  )

  #--------------------------------------------------#
  ## Figures -----
  #--------------------------------------------------#

  #' Bar chart of the most frequent mapped habitats.
  plot_mapped_habitats <- function(x, species_label) {
    ggplot2::ggplot(
      data = x %>%
        dplyr::slice(1:10) %>%
        dplyr::mutate(order = dplyr::row_number()),
      ggplot2::aes(
        x = forcats::fct_reorder(BIOTOP, order),
        y = COUNT
      )
    ) +
      ggplot2::geom_bar(stat = "identity", fill = "#595959") +
      scale_y_count() +
      ggplot2::xlab(paste0("\ncatalogued habitats preferred by ", species_label)) +
      ggplot2::ylab("number of sites\n") +
      ggplot2::theme_classic(base_size = 14)
  }

  report_figure(
    plot_mapped_habitats(habitat_nau, "P. nausithous"),
    "Catalogued habitats at occupied P. nausithous sites",
    "06_mapped_habitats_nausithous"
  )
  report_figure(
    plot_mapped_habitats(habitat_tel, "P. teleius"),
    "Catalogued habitats at occupied P. teleius sites",
    "06_mapped_habitats_teleius"
  )

  #' Bar chart of the total area of the most extensive mapped habitats.
  plot_mapped_habitat_area <- function(x, species_label) {
    ggplot2::ggplot(
      data = x %>%
        dplyr::arrange(dplyr::desc(AREA)) %>%
        dplyr::slice(1:10),
      ggplot2::aes(
        x = forcats::fct_reorder(BIOTOP, AREA, .desc = TRUE),
        y = log(AREA)
      )
    ) +
      ggplot2::geom_bar(stat = "identity", fill = "#595959") +
      scale_y_count() +
      ggplot2::xlab(paste0("\ncatalogued habitats preferred by ", species_label)) +
      ggplot2::ylab("log habitat area sum (ha)\n") +
      ggplot2::theme_classic(base_size = 14)
  }

  report_figure(
    plot_mapped_habitat_area(habitat_nau, "P. nausithous"),
    "Habitat area at occupied P. nausithous sites",
    "06_mapped_habitat_area_nausithous"
  )
  report_figure(
    plot_mapped_habitat_area(habitat_tel, "P. teleius"),
    "Habitat area at occupied P. teleius sites",
    "06_mapped_habitat_area_teleius"
  )

  #--------------------------------------------------#
  ## Segment selection rule -----
  #--------------------------------------------------#

  report_section("Segment selection rule")

  report_warning(
    "The rule is carried over unchanged from the original script:",
    "`arrange(NATURAL, AREA_SITE) %>% slice(1)`. Both keys sort ascending, so",
    "the segment kept for each record is the *least* natural and, among those,",
    "the *smallest*. If the intention was to describe each site by its most",
    "natural and largest habitat patch, the keys need",
    "`desc(NATURAL), desc(AREA_SITE)`. This was left as it stands because",
    "changing it would change every habitat model."
  )

  # NATURAL is defined by BIOTOP being present and not an X code, so sorting it
  # ascending is the same as preferring a segment with no habitat code at all.
  # The consequence is large enough to state in numbers.
  biotop_present <- sum(!is.na(data_analysis$BIOTOP))

  report_warning(
    "The direction of that sort has a large effect. NATURAL is defined by",
    "BIOTOP being present and not an X code, so ordering it ascending prefers",
    "a segment with no habitat code whenever the site has one. After the",
    "selection only", biotop_present, "of", nrow(data_analysis), "records",
    sprintf("(%.1f%%)", 100 * biotop_present / nrow(data_analysis)),
    "still carry a BIOTOP value, and the models using BIOTOP are fitted on",
    "that subset alone. Reversing the sort to `desc(NATURAL)` would keep the",
    "mapped habitat wherever one exists. This is a decision about the",
    "manuscript, not a bug, so the rule is left as written."
  )

  report_table(
    as.data.frame(table(
      NATURAL        = data_analysis$NATURAL,
      BIOTOP_present = !is.na(data_analysis$BIOTOP)
    )),
    "Selected segments: natural habitat flag against presence of a habitat code",
    "06_natural_vs_biotop"
  )

  natural_share <- data.frame(
    NATURAL = c(0, 1),
    records = c(
      sum(data_analysis$NATURAL == 0, na.rm = TRUE),
      sum(data_analysis$NATURAL == 1, na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )

  report_table(
    natural_share,
    "Records by whether the selected segment is a natural habitat",
    "06_natural_habitat_share"
  )
}

#----------------------------------------------------------#
# Validate -----
#----------------------------------------------------------#

# The habitat join adds columns but must never add or drop a record.
validate_table(
  data_analysis, "data_analysis",
  columns = c(AREA_SITE = "numeric", BIOTOP = "character", FSB = "character"),
  key = "ID_RECORD",
  not_na = c("POSITIVE", "YEAR"),
  xy = c("X", "Y"),
  n_rows = nrow(data_clean)
)
checkmate::assert_true(
  identical(data_analysis$ID_RECORD, data_clean$ID_RECORD),
  .var.name = "data_analysis keeps the records of data_clean in their order"
)

#----------------------------------------------------------#
# Export -----
#----------------------------------------------------------#

readr::write_csv(
  data_analysis,
  processed_path("data_analysis.csv")
)

report_finish()

message("  ", nrow(data_analysis), " records in the analysis table")
step_end("06")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

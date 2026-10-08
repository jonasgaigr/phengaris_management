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
# A monitoring site can overlap many habitat segments, so they have to be
# reduced to one description per record. The rule is set per specification
# (config.yml, habitat_segment_rule) and implemented in
# select_habitat_segment(): "legacy" keeps the original rule (order by
# NATURAL, then by AREA_SITE, keep the first segment), "area_weighted"
# summarises all the segments of a site and is NOT VALIDATED ON REAL DATA.
#
# Variables added under the legacy rule:
#
#   BIOTOP, BIOTOP_SEZ, FSB, FSB_EVAL, HABITAT   habitat mapping attributes
#   AREA_SITE   area of the site x segment overlap, in m2 (AREA_real)
#   HET_OUT     between-habitat heterogeneity, i.e. the number of habitats
#               listed in the segment mosaic BIOTOP_SEZ
#   NATURAL     1 when a natural habitat is mapped, 0 for X codes and no data
#
# Reads:  Data/Processed/<spec>/data_clean.csv       (step 04)
#         Data/Processed/common/data_lokal_vmb.csv   (step 05)
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

path_lokal_vmb_csv <- processed_path("data_lokal_vmb.csv", "common")

#----------------------------------------------------------#
# Join the habitat attributes -----
#----------------------------------------------------------#

if (file.exists(path_lokal_vmb_csv)) {

  habitat_segments <- read_habitat_overlaps(path_lokal_vmb_csv)

  # How each record is described by the segments its site overlaps; the rule
  # is set per specification in config.yml.
  data_analysis <- select_habitat_segment(
    data_clean,
    habitat_segments,
    rule = spec_settings()$habitat_segment_rule
  )

  habitat_join_available <- TRUE

} else {

  # Keep the cascade running without habitat data: the analysis table is the
  # cleaned table with empty habitat columns, and the habitat models in later
  # steps will report themselves as skipped.
  data_analysis <-
    data_clean %>%
    dplyr::mutate(
      SEGMENT_ID = NA_integer_,
      BIOTOP     = NA_character_,
      BIOTOP_SEZ = NA_character_,
      FSB        = NA_character_,
      FSB_EVAL   = NA_character_,
      HABITAT    = NA_character_,
      STEJ_PR    = NA_real_,
      SHAPE_Area = NA_real_,
      AREA_real  = NA_real_,
      LENGTH_real = NA_real_,
      REPRE      = NA_real_,
      ZACH       = NA_real_,
      AREA_SITE  = NA_real_,
      HET_OUT    = NA_integer_,
      NATURAL    = NA_integer_
    )

  habitat_join_available <- FALSE

  report_warning(
    "Data/Processed/common/data_lokal_vmb.csv does not exist, so no habitat",
    "attributes could be attached. The habitat columns are empty and the",
    "models that use AREA_SITE, FSB, BIOTOP or HET_OUT will be skipped.",
    "Run step 05 from a machine with access to //bali.nature.cz."
  )

}

#----------------------------------------------------------#
# Results -----
#----------------------------------------------------------#

if (habitat_join_available) {

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

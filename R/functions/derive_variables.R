#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: the analysis variables of step 04
#
#----------------------------------------------------------#
#
# Turns the targeted monitoring records into the analysis variables:
# occupancy, year, protection status, host plant abundance, habitat type,
# threats and pressures, and the recorded management. Most of them are read
# out of two free-text fields the surveyors fill in:
#
#   POP_BIOT     habitat description  -> TTP, PRIKOP, ZARUST, JINY
#   STRUKT_POZN  structured notes     -> host plant, threats, management
#
#----------------------------------------------------------#

#--------------------------------------------------#
## Protection and co-occurrence helpers -----
#--------------------------------------------------#

#' Site identifiers belonging to one protection category.
#'
#' @param protected_area_id  the table written by step 03
#' @param pa_type            "EVL_any", "EVL_target" or "MZCHU"
pa_site_ids <- function(protected_area_id, pa_type) {
  protected_area_id %>%
    dplyr::filter(PA_TYPE == pa_type) %>%
    dplyr::pull(ID_LOKAL) %>%
    unique()
}

#' Visits (site x date) at which both species were recorded.
#'
#' One ID_NALEZ is kept per visit, which is how SPEC_NUM was defined
#' originally.
#'
#' @param x  records to summarise
both_species_visits <- function(x) {
  x %>%
    dplyr::group_by(
      NAZ_LOKAL,
      DATUM_OD
    ) %>%
    dplyr::summarise(
      ID_NALEZ = unique(ID_NALEZ)[1],
      SPEC_NUM = length(unique(DRUH)),
      .groups  = "drop"
    ) %>%
    dplyr::filter(
      SPEC_NUM == 2
    )
}

#--------------------------------------------------#
## Text-field decoding helpers -----
#--------------------------------------------------#

#' Flag a phrase in the structured notes.
#'
#' Absent phrase and empty notes both count as 0, matching how threats and
#' management presence were coded originally.
flag_note <- function(x, pattern) {
  dplyr::case_when(
    grepl(pattern, x) ~ 1,
    TRUE ~ 0
  )
}

#' Flag any of a set of phrases in the habitat description.
#'
#' An empty habitat description stays NA rather than becoming 0, because no
#' habitat was assessed at all in that case.
#'
#' @param x        the POP_BIOT strings
#' @param phrases  character vector; the flag is 1 when any of them occurs
flag_biotope <- function(x, phrases) {
  hit <- Reduce(`|`, lapply(phrases, function(p) grepl(p, x, fixed = TRUE)))
  dplyr::case_when(
    hit ~ 1,
    is.na(x) == FALSE ~ 0
  )
}

#--------------------------------------------------#
## The analysis variables -----
#--------------------------------------------------#

#' Derive the analysis variables from the targeted records.
#'
#' The structured notes are parsed once with parse_strukt_pozn(); host plant,
#' mowing, grazing and threats are then read from the value of the right key
#' rather than searched for anywhere in the string.
#'
#' @param data               targeted records, observed and imputed
#' @param protected_area_id  the protected-area membership from step 03
#' @param both_present       visits at which both species were found, from
#'                           both_species_visits()
#' @param threat_classes     threat classes and their phrases, as in
#'                           config.yml `strukt_pozn: threat_classes`
#' @param threat_vocabulary  "legacy": the phrases of `threat_classes`,
#'                           matched anywhere in the threat list;
#'                           "harmonised": the items of both field-form lists,
#'                           config.yml `strukt_pozn: threat_items`, matched
#'                           as whole items
#' @return `data` with the analysis variables added, every row kept
derive_analysis_variables <- function(data, protected_area_id, both_present,
                                      threat_classes = CONFIG$strukt_pozn$threat_classes,
                                      threat_vocabulary = c("legacy", "harmonised")) {
  threat_vocabulary <- match.arg(threat_vocabulary)
  evl_any_ids <- pa_site_ids(protected_area_id, "EVL_any")
  evl_target_ids <- pa_site_ids(protected_area_id, "EVL_target")
  mzchu_ids <- pa_site_ids(protected_area_id, "MZCHU")

  notes <- parse_strukt_pozn(data$STRUKT_POZN)
  plant_levels <- unlist(CONFIG$strukt_pozn$host_plant_levels)
  # A record without any threat list says nothing about threats: NA, not 0.
  threats <- if (threat_vocabulary == "legacy") {
    threat_class_flags(notes$threats, threat_classes, missing = NA)
  } else {
    threat_classes <- CONFIG$strukt_pozn$threat_items
    threat_item_flags(notes$threats, threat_classes, missing = NA)
  }
  grazing_from <- CONFIG$strukt_pozn$grazing_recorded_from_year
  pop_biot <- CONFIG$pop_biot$flags

  data %>%
    dplyr::mutate(
      #--------------------------------------------------#
      ## Occupancy and time -----
      #--------------------------------------------------#
      POSITIVE = dplyr::case_when(
        NEGATIV == 1 ~ 0,
        NEGATIV == 0 ~ 1
      ),
      # The calendar year of the visit, as a number. Models that need it as a
      # grouping factor convert it there.
      YEAR = as.integer(substr(DATUM_OD, 1, 4)),

      #--------------------------------------------------#
      ## Protection -----
      #--------------------------------------------------#
      EVL = dplyr::case_when(
        ID_LOKAL %in% evl_any_ids ~ 1,
        TRUE ~ 0
      ),
      # Inside a Natura 2000 site designated for either Phengaris species,
      # whichever species the record is of.
      EVL_target = dplyr::case_when(
        DRUH %in% c(SPECIES_NAU, SPECIES_TEL) &
          ID_LOKAL %in% evl_target_ids ~ 1,
        TRUE ~ 0
      ),
      EVL_comb = dplyr::case_when(
        EVL_target == 1 ~ 1,
        EVL == 1 &
          EVL_target == 0
        ~ 0.5,
        TRUE ~ 0
      ),
      MZCHU = dplyr::case_when(
        ID_LOKAL %in% mzchu_ids ~ 1,
        TRUE ~ 0
      ),
      PROTECT = dplyr::case_when(
        EVL == 1 |
          MZCHU == 1
        ~ 1,
        TRUE ~ 0
      ),

      #--------------------------------------------------#
      ## Host plant (Sanguisorba officinalis) -----
      #--------------------------------------------------#
      # 0 none, 1 single plants, 2 abundant, 3 dominant.
      PLANT_QUANT = as.numeric(unname(plant_levels[notes$host_plant])),
      PLANT_QUAL = dplyr::case_when(
        PLANT_QUANT %in% c(1, 2, 3) ~ 1,
        PLANT_QUANT == 0 ~ 0
      ),

      #--------------------------------------------------#
      ## Recorded habitat type -----
      #--------------------------------------------------#
      # Phrases per flag in config.yml `pop_biot: flags`.
      TTP = flag_biotope(POP_BIOT, pop_biot$TTP),
      PRIKOP = flag_biotope(POP_BIOT, pop_biot$PRIKOP),
      ZARUST = flag_biotope(POP_BIOT, pop_biot$ZARUST),
      JINY = flag_biotope(POP_BIOT, pop_biot$JINY),
      VODA = flag_biotope(POP_BIOT, pop_biot$VODA),
      PASTVINA = flag_biotope(POP_BIOT, pop_biot$PASTVINA),

      #--------------------------------------------------#
      ## Threats and pressures -----
      #--------------------------------------------------#
      # One 0/1 column per threat class of config.yml, from the threat list.
      !!!as.list(threats),
      # The surveyor recorded that no threat was seen. (The original pattern,
      # "žádné", only ever matched "Přítomnost rostlin: žádné", so None was 0
      # on every analysed record.) NA without a threat list.
      None = dplyr::if_else(
        is.na(notes$threats),
        NA_real_,
        as.numeric(grepl(CONFIG$strukt_pozn$no_threat_phrase, notes$threats, fixed = TRUE))
      ),

      #--------------------------------------------------#
      ## Management -----
      #--------------------------------------------------#
      # Mowing method: 1 = partial (patches left uncut), 0 = whole area cut.
      METHOD = dplyr::case_when(
        notes$mow_whole_area == "ne" ~ 1,
        notes$mow_whole_area == "ano" ~ 0
      ),
      # Mowing timing: 1 = appropriate for the butterflies, 0 = harmful.
      TIMING = dplyr::case_when(
        notes$mow_timing == "ano" ~ 1,
        notes$mow_timing == "ne" ~ 0
      ),
      # 1 when the form records anything about mowing.
      MOW = as.numeric(!is.na(notes$mow_timing) | !is.na(notes$mow_whole_area)),
      # Grazing: 1 grazed (extensively or intensively), 0 not grazed, NA in
      # the years whose form had no grazing entry. From grazing_from on, a
      # record without a grazing entry counts as not grazed (config.yml).
      GRAZE = dplyr::case_when(
        YEAR < grazing_from ~ NA_real_,
        notes$grazing %in% c("extenzivní", "intenzivní") ~ 1,
        notes$grazing == "ne" | is.na(notes$grazing) ~ 0
      ),
      # Grazing method: 1 = extensive, 0 = intensive.
      GRAZE_MET = dplyr::case_when(
        notes$grazing == "extenzivní" ~ 1,
        notes$grazing == "intenzivní" ~ 0
      ),
      MANAGEMENT_HET = dplyr::case_when(
        is.na(METHOD) == FALSE &
          is.na(GRAZE) == FALSE
        ~ 1,
        TRUE ~ 0
      ),
      # Appropriate mowing method and timing together; NA when either is
      # unknown.
      OVERALL = dplyr::case_when(
        is.na(METHOD) | is.na(TIMING) ~ NA_real_,
        METHOD == 1 & TIMING == 1 ~ 1,
        TRUE ~ 0
      ),

      #--------------------------------------------------#
      ## Co-occurrence and mapping grid -----
      #--------------------------------------------------#
      SPEC_NUM = dplyr::case_when(
        ID_NALEZ %in% both_present$ID_NALEZ ~ 1,
        TRUE ~ 0
      ),
      SITMAP = as.factor(SITMAP)
    ) %>%
    dplyr::mutate(
      # Number of threat classes recorded; None is not a threat. NA when the
      # record has no threat list at all.
      SUM_THREATS = rowSums(
        dplyr::across(dplyr::all_of(names(threat_classes)))
      ),
      # Within-site habitat heterogeneity: how many habitat types were recorded.
      # NA when no habitat was described at all.
      HET_INN = dplyr::if_else(
        is.na(POP_BIOT),
        NA_real_,
        rowSums(dplyr::across(c(TTP, PRIKOP, ZARUST, JINY, VODA, PASTVINA)), na.rm = TRUE)
      )
    )
}

#' Keep the records at which the host plant was recorded as present.
#'
#' Drops both the records where the host plant was recorded absent
#' (PLANT_QUAL = 0) and the records without any host plant information.
keep_host_plant_present <- function(data) {
  data %>%
    dplyr::filter(
      is.na(PLANT_QUAL) == FALSE &
        PLANT_QUAL != 0
    )
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

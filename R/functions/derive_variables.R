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

#' Flag a phrase in the habitat description.
#'
#' An empty habitat description stays NA rather than becoming 0, because no
#' habitat was assessed at all in that case.
flag_biotope <- function(x, pattern) {
  dplyr::case_when(
    grepl(pattern, x) ~ 1,
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
#' @return `data` with the analysis variables added, every row kept
derive_analysis_variables <- function(data, protected_area_id, both_present,
                                      threat_classes = CONFIG$strukt_pozn$threat_classes) {
  evl_any_ids    <- pa_site_ids(protected_area_id, "EVL_any")
  evl_target_ids <- pa_site_ids(protected_area_id, "EVL_target")
  mzchu_ids      <- pa_site_ids(protected_area_id, "MZCHU")

  notes <- parse_strukt_pozn(data$STRUKT_POZN)
  plant_levels <- unlist(CONFIG$strukt_pozn$host_plant_levels)
  threats <- threat_class_flags(notes$threats, threat_classes)

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
        EVL == 1
        | MZCHU == 1
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
      TTP    = flag_biotope(POP_BIOT, "TTP s pravidelným managementem"),
      PRIKOP = flag_biotope(POP_BIOT, "příkop u komunikace"),
      ZARUST = flag_biotope(POP_BIOT, "zarůstající louka bez managementu"),
      JINY   = flag_biotope(POP_BIOT, "jiný"),

      #--------------------------------------------------#
      ## Threats and pressures -----
      #--------------------------------------------------#
      # One 0/1 column per threat class of config.yml, from the threat list.
      !!!as.list(threats),
      # The original definition, kept until it is corrected separately: the
      # pattern "žádné" only ever matches "Přítomnost rostlin: žádné".
      None = flag_note(STRUKT_POZN, "žádné"),

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
      # 1 when the form has a grazing entry at all, as originally defined.
      GRAZE = as.numeric(!is.na(notes$grazing)),
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
      OVERALL = dplyr::case_when(
        METHOD == 1
        & TIMING == 1
        ~ 1,
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
      SUM_THREATS = rowSums(
        dplyr::across(dplyr::all_of(c(names(threat_classes), "None"))),
        na.rm = TRUE
      ),
      # Within-site habitat heterogeneity: how many habitat types were recorded.
      HET_INN = rowSums(
        dplyr::across(c(TTP, PRIKOP, ZARUST, JINY)),
        na.rm = TRUE
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

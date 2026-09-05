#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#        Step 04 - Build the cleaned occurrence table
#
#
#----------------------------------------------------------#
#
# Turns the raw monitoring records into the analysis variables: occupancy,
# year, protection status, host plant abundance, habitat type, threats and
# pressures, and the recorded management.
#
# Most variables are read out of two free-text fields the surveyors fill in:
#
#   POP_BIOT     habitat description  -> TTP, PRIKOP, ZARUST, JINY
#   STRUKT_POZN  structured notes     -> host plant, threats, management
#
# Reads:  Data/Processed/data_with_imputed.csv   (step 02)
#         Data/Processed/protected_area_id.csv   (step 03)
# Writes: Data/Processed/data_clean.csv
#
#----------------------------------------------------------#

message("Step 04: cleaning occurrence data")

report_start(
  "04",
  "Cleaned occurrence data",
  paste(
    "Derivation of the analysis variables from the targeted monitoring",
    "records. Only records from the targeted monitoring campaigns are kept,",
    "only records with the host plant present, and the two years with",
    "insufficient coverage are dropped."
  )
)

#----------------------------------------------------------#
# Load -----
#----------------------------------------------------------#

data_with_imputed <-
  readr::read_csv(
    file.path(PATHS$processed, "data_with_imputed.csv"),
    show_col_types = FALSE
  )

protected_area_id <-
  readr::read_csv(
    file.path(PATHS$processed, "protected_area_id.csv"),
    show_col_types = FALSE
  )

#--------------------------------------------------#
## Site identifiers by protection category -----
#--------------------------------------------------#

#' Site identifiers belonging to one protection category.
pa_site_ids <- function(pa_type) {
  protected_area_id %>%
    dplyr::filter(PA_TYPE == pa_type) %>%
    dplyr::pull(ID_LOKAL) %>%
    unique()
}

evl_any_ids    <- pa_site_ids("EVL_any")
evl_target_ids <- pa_site_ids("EVL_target")
mzchu_ids      <- pa_site_ids("MZCHU")

#----------------------------------------------------------#
# Keep targeted monitoring only -----
#----------------------------------------------------------#

data_new_source <-
  data_with_imputed %>%
  dplyr::filter(
    ZDROJ %in% target_mon_zdroj
  )

#----------------------------------------------------------#
# Visits covering both species -----
#----------------------------------------------------------#

#' Visits (site x date) at which both species were recorded.
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

# Both species monitored on the visit, regardless of the outcome.
phengaris_both_monitored <- both_species_visits(data_new_source)

# Both species actually found on the visit.
phengaris_both_present <-
  both_species_visits(
    data_new_source %>%
      dplyr::filter(
        NEGATIV == 0                        # drop negative occurrence records
      )
  )

#----------------------------------------------------------#
# Text-field decoding helpers -----
#----------------------------------------------------------#

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

#----------------------------------------------------------#
# Derive the analysis variables -----
#----------------------------------------------------------#

data_clean <-
  data_new_source %>%
  dplyr::mutate(
    #--------------------------------------------------#
    ## Occupancy and time -----
    #--------------------------------------------------#
    POSITIVE = dplyr::case_when(
      NEGATIV == 1 ~ 0,
      NEGATIV == 0 ~ 1
    ),
    YEAR = as.factor(
      substr(
        DATUM_OD,
        1,
        4
      )
    ),

    #--------------------------------------------------#
    ## Protection -----
    #--------------------------------------------------#
    EVL = dplyr::case_when(
      ID_LOKAL %in% evl_any_ids ~ 1,
      TRUE ~ 0
    ),
    EVL_target = dplyr::case_when(
      # Natura 2000 site designated for the species of this record
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
    PLANT_QUANT = dplyr::case_when(
      grepl("dominantně",  STRUKT_POZN) ~ 3,
      grepl("hojně",       STRUKT_POZN) ~ 2,
      grepl("jednotlivě",  STRUKT_POZN) ~ 1,
      grepl("žádné",       STRUKT_POZN) ~ 0
    ),
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
    LandUseChange           = flag_note(STRUKT_POZN, "změna zemědělského využívání půdy a terénní úpravy"),
    Abandonment             = flag_note(STRUKT_POZN, "absence či nedostatek péče"),
    HarmfulMow              = flag_note(STRUKT_POZN, "nevhodná seč"),
    HarmfulGrazing          = flag_note(STRUKT_POZN, "nevhodná pastva"),
    GrazingByeffects        = flag_note(STRUKT_POZN, "dopady chovu dobytka mimo pastvu"),
    FertilizerUse           = flag_note(STRUKT_POZN, "aplikace hnojiv"),
    Afforestation           = flag_note(STRUKT_POZN, "zalesňování bezlesí"),
    Invasives               = flag_note(STRUKT_POZN, "invazní druhy"),
    NativeDominants         = flag_note(STRUKT_POZN, "expanzní druhy"),
    AbioticNaturalProcesses = flag_note(STRUKT_POZN, "abiotické přírodní procesy"),
    Encroachment            = flag_note(STRUKT_POZN, "sukcese"),
    BiomassAccumulation     = flag_note(STRUKT_POZN, "hromadění organického materiálu"),
    Eutrophization          = flag_note(STRUKT_POZN, "eutrofizace či okyselování"),
    None                    = flag_note(STRUKT_POZN, "žádné"),

    #--------------------------------------------------#
    ## Management -----
    #--------------------------------------------------#
    # Mowing method: 1 = partial (patches left uncut), 0 = whole area cut.
    METHOD = dplyr::case_when(
      grepl("Seč celoplošná: ne",  STRUKT_POZN) ~ 1,
      grepl("Seč celoplošná: ano", STRUKT_POZN) ~ 0
    ),
    # Mowing timing: 1 = appropriate for the butterflies, 0 = harmful.
    TIMING = dplyr::case_when(
      grepl("Seč vhodně načasovaná: ano", STRUKT_POZN) ~ 1,
      grepl("Seč vhodně načasovaná: ne",  STRUKT_POZN) ~ 0
    ),
    MOW   = flag_note(STRUKT_POZN, "Seč"),
    GRAZE = flag_note(STRUKT_POZN, "Pastva:"),
    # Grazing method: 1 = extensive, 0 = intensive.
    GRAZE_MET = dplyr::case_when(
      grepl("Pastva: extenzivní", STRUKT_POZN) ~ 1,
      grepl("Pastva: intenzivní", STRUKT_POZN) ~ 0
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
      ID_NALEZ %in% phengaris_both_present$ID_NALEZ ~ 1,
      TRUE ~ 0
    ),
    SITMAP = as.factor(SITMAP)
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    SUM_THREATS = sum(
      LandUseChange,
      Abandonment,
      HarmfulMow,
      HarmfulGrazing,
      GrazingByeffects,
      FertilizerUse,
      Afforestation,
      Invasives,
      NativeDominants,
      AbioticNaturalProcesses,
      Encroachment,
      BiomassAccumulation,
      Eutrophization,
      None,
      na.rm = TRUE
    ),
    # Within-site habitat heterogeneity: how many habitat types were recorded.
    HET_INN = sum(
      TTP,
      PRIKOP,
      ZARUST,
      JINY,
      na.rm = TRUE
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    is.na(PLANT_QUAL) == FALSE &
      PLANT_QUAL != 0 &
      !YEAR %in% EXCLUDED_YEARS
  )

#----------------------------------------------------------#
# Results -----
#----------------------------------------------------------#

cleaning_summary <- data.frame(
  stage = c(
    "records with imputed absences (step 02)",
    "targeted monitoring only",
    "host plant present, excluded years dropped"
  ),
  records = c(
    nrow(data_with_imputed),
    nrow(data_new_source),
    nrow(data_clean)
  ),
  stringsAsFactors = FALSE
)

report_table(
  cleaning_summary,
  "Records retained at each filtering stage",
  "04_cleaning_summary"
)

protection_counts <-
  data_clean %>%
  dplyr::summarise(
    dplyr::across(
      c(EVL, EVL_target, MZCHU, PROTECT, SPEC_NUM),
      ~ sum(.x == 1, na.rm = TRUE)
    )
  ) %>%
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to  = "variable",
    values_to = "records_flagged"
  ) %>%
  dplyr::mutate(
    records_total = nrow(data_clean),
    percent       = round(100 * records_flagged / records_total, 1)
  )

report_table(
  protection_counts,
  "Records flagged by each binary site attribute",
  "04_flag_counts"
)

report_table(
  phengaris_both_monitored %>%
    dplyr::count(name = "visits") %>%
    dplyr::mutate(description = "visits at which both species were monitored") %>%
    dplyr::bind_rows(
      phengaris_both_present %>%
        dplyr::count(name = "visits") %>%
        dplyr::mutate(description = "visits at which both species were found")
    ) %>%
    dplyr::select(description, visits),
  "Visits covering both species",
  "04_both_species_visits"
)

#--------------------------------------------------#
## Corrections applied in this step -----
#--------------------------------------------------#

report_section("Corrections applied to the original code")

report_warning(
  "MZCHU and SPEC_NUM were previously derived with `ID %in% <data frame>`",
  "rather than `ID %in% <vector of ids>`. Comparing against a data frame made",
  "both variables constant: in the previous `data_clean.csv` all 4540 records",
  "had MZCHU = 0 and SPEC_NUM = 0, and PROTECT was therefore an exact copy of",
  "EVL. Every model and figure using MZCHU, PROTECT or SPEC_NUM was fitted on",
  "a constant. Both now compare against the identifier vector."
)

report_note(paste(
  "The MZCHU flag now marks",
  sum(data_clean$MZCHU == 1),
  "of", nrow(data_clean),
  "records as lying inside a small-scale specially protected area, and",
  "PROTECT is no longer identical to EVL."
))

# SPEC_NUM inherits one ID_NALEZ per visit from the summarise above, so it
# marks one record per both-species visit rather than every record of such a
# visit. The visit-level alternative is reported here so that the difference is
# visible before it is used in the manuscript.
spec_num_visit_level <-
  data_clean %>%
  dplyr::mutate(
    .visit = paste(NAZ_LOKAL, DATUM_OD),
    .both  = .visit %in% paste(
      phengaris_both_present$NAZ_LOKAL,
      phengaris_both_present$DATUM_OD
    )
  ) %>%
  dplyr::summarise(records = sum(.both)) %>%
  dplyr::pull(records)

report_warning(
  "SPEC_NUM keeps the original definition, which takes one ID_NALEZ per",
  "both-species visit, and so now flags", sum(data_clean$SPEC_NUM == 1),
  "records. Defining it at the visit level instead, by matching site and date,",
  "would flag", spec_num_visit_level, "records. Decide which of the two the",
  "co-occurrence model should use before the manuscript is finalised."
)

# EVL_target keeps the original, species-blind definition: a record is flagged
# when its site lies in a Natura 2000 site designated for *either* Phengaris
# species. Step 03 now records which species each site was designated for, so
# the species-specific alternative can be quantified without changing the flag.
# PA_SPECIES is only written by step 03 from this version onwards. When an
# older protected_area_id.csv is in place the comparison is simply skipped:
# it is a diagnostic, and it must not stop the cascade.
if ("PA_SPECIES" %in% names(protected_area_id)) {

  designated_site_species <-
    protected_area_id %>%
    dplyr::filter(PA_TYPE == "EVL_target") %>%
    dplyr::transmute(key = paste(ID_LOKAL, PA_SPECIES)) %>%
    dplyr::pull(key) %>%
    unique()

  evl_target_species_specific <- sum(
    paste(data_clean$ID_LOKAL, data_clean$DRUH) %in% designated_site_species
  )

  report_warning(
    "EVL_target keeps the original definition, under which a record is flagged",
    "when its site lies in a Natura 2000 site designated for either Phengaris",
    "species. It flags", sum(data_clean$EVL_target == 1), "records. Matching",
    "the designation to the species of the record instead would flag",
    evl_target_species_specific, "records. This was left unchanged because it",
    "is an interpretation of the variable, not a coding error."
  )

} else {

  report_warning(
    "EVL_target keeps the original definition, under which a record is flagged",
    "when its site lies in a Natura 2000 site designated for either Phengaris",
    "species. It flags", sum(data_clean$EVL_target == 1), "records. The",
    "species-specific alternative could not be quantified because",
    "protected_area_id.csv predates the PA_SPECIES column; re-run step 03 to",
    "get that comparison."
  )

}

#----------------------------------------------------------#
# Export -----
#----------------------------------------------------------#

readr::write_csv(
  data_clean,
  file.path(PATHS$processed, "data_clean.csv")
)

report_finish()

message("Step 04 done: ", nrow(data_clean), " cleaned records written")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

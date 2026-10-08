#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: the original ordination of threats (step 13)
#
#----------------------------------------------------------#
#
# The input of the original ordination: the records with at least one threat,
# collapsed to one row per species x occupancy class, i.e. four rows. With
# four rows an ordination has almost no residual degrees of freedom
# (REVIEW.md, item 12); these functions reproduce it as the legacy
# specification and nothing more.
#
#----------------------------------------------------------#

#' Threat profiles per species x occupancy class, as the original analysis.
#'
#' The threat variables enter twice: as a count of affected records (_sum) and
#' as the share of records affected.
#'
#' @param data  the analysis table
#' @return one row per species x occupancy class, with ID = species + occupancy
legacy_threat_profiles <- function(data) {
  data %>%
    dplyr::filter(
      SUM_THREATS > 0
    ) %>%
    dplyr::mutate(
      DRUH = dplyr::case_when(
        DRUH == SPECIES_NAU ~ "nausithous",
        DRUH == SPECIES_TEL ~ "teleius"
      )
    ) %>%
    dplyr::select(
      DRUH, POSITIVE,
      PLANT_QUANT, TTP, ZARUST, PRIKOP, HET_INN,
      EVL, EVL_target, MZCHU,
      LandUseChange, Abandonment, HarmfulMow, HarmfulGrazing,
      GrazingByeffects, FertilizerUse, Afforestation, Invasives,
      NativeDominants, AbioticNaturalProcesses,
      Encroachment, BiomassAccumulation, Eutrophization, None
    ) %>%
    dplyr::group_by(DRUH, POSITIVE) %>%
    dplyr::summarise(
      ID = paste0(unique(DRUH), unique(POSITIVE)),
      PLANT_QUANT = mean(PLANT_QUANT),
      TTP = mean(TTP),
      ZARUST = mean(ZARUST),
      PRIKOP = mean(PRIKOP),
      HET_INN = mean(HET_INN),
      EVL = mean(EVL),
      EVL_TAR = mean(EVL_target),
      MZCHU = mean(MZCHU),
      LandUseChange_sum = sum(LandUseChange),
      Abandonment_sum = sum(Abandonment),
      HarmfulMow_sum = sum(HarmfulMow),
      HarmfulGrazing_sum = sum(HarmfulGrazing),
      GrazingByeffects_sum = sum(GrazingByeffects),
      FertilizerUse_sum = sum(FertilizerUse),
      Afforestation_sum = sum(Afforestation),
      Invasives_sum = sum(Invasives),
      NativeDominants_sum = sum(NativeDominants),
      AbioticNaturalProcesses_sum = sum(AbioticNaturalProcesses),
      Encroachment_sum = sum(Encroachment),
      BiomassAccumulation_sum = sum(BiomassAccumulation),
      Eutrophization_sum = sum(Eutrophization),
      None_sum = sum(None),
      LandUseChange = mean(LandUseChange),
      Abandonment = mean(Abandonment),
      HarmfulMow = mean(HarmfulMow),
      HarmfulGrazing = mean(HarmfulGrazing),
      GrazingByeffects = mean(GrazingByeffects),
      FertilizerUse = mean(FertilizerUse),
      Afforestation = mean(Afforestation),
      Invasives = mean(Invasives),
      NativeDominants = mean(NativeDominants),
      AbioticNaturalProcesses = mean(AbioticNaturalProcesses),
      Encroachment = mean(Encroachment),
      BiomassAccumulation = mean(BiomassAccumulation),
      Eutrophization = mean(Eutrophization),
      None = mean(None),
      .groups = "drop"
    )
}

#' Drop columns an ordination cannot use.
#'
#' Non-numeric, all-NA and zero-variance columns all break the scaling step.
drop_unusable_columns <- function(x) {
  unusable <- sapply(x, function(col) {
    if (!is.numeric(col)) return(TRUE)
    if (all(is.na(col))) return(TRUE)
    s <- suppressWarnings(stats::sd(col, na.rm = TRUE))
    is.na(s) || s == 0
  })
  if (any(unusable)) {
    warning(
      "Removing problematic columns: ",
      paste(names(unusable)[unusable], collapse = ", "),
      call. = FALSE
    )
  }
  x[, !unusable, drop = FALSE]
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

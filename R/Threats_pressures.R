
#----------------------------------------------------------#
## Threats and pressures -----
#----------------------------------------------------------#
data_tap_all <- data %>%
  dplyr::filter(
    SUM_THREATS > 0
    ) %>%
  dplyr::mutate(
    DRUH = dplyr::case_when(
      DRUH == "Phengaris nausithous" ~ "nausithous",
      DRUH == "Phengaris teleius" ~ "teleius"
      )
    ) %>%
  dplyr::select(
    DRUH, POSITIVE,
    PLANT_QUANT, TTP, ZARUST, PRIKOP, HET_INN, #NATURAL, 
    EVL, EVL_target, MZCHU,
    LandUseChange, Abandonment, HarmfulMow, HarmfulGrazing,
    GrazingByeffects, FertilizerUse, Afforestation, Invasives,
    NativeDominants, AbioticNaturalProcesses,
    Encroachment, BiomassAccumulation, Eutrophization, None
    ) %>%
  dplyr::group_by(DRUH, POSITIVE) %>%
  dplyr::summarise(ID = paste0(unique(DRUH), unique(POSITIVE)),
                   PLANT_QUANT = mean(PLANT_QUANT), 
                   TTP = mean(TTP), 
                   ZARUST = mean(ZARUST), 
                   PRIKOP = mean(PRIKOP), 
                   HET_INN = mean(HET_INN),
                   #NATURAL = mean(NATURAL),
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
                   None = mean(None)) %>%
  dplyr::ungroup()

data_tap <- data_tap_all %>%
  tibble::column_to_rownames('ID') %>%
  dplyr::select(PLANT_QUANT, 
                TTP, 
                ZARUST, 
                PRIKOP, 
                HET_INN, 
                #NATURAL,
                EVL, MZCHU,
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
                None
  )

write.csv2(data_tap_all,
           "data_pca.csv",
           fileEncoding = "Windows-1250")
write.csv2(data %>%
             st_drop_geometry(),
           "data_data_export.csv",
           fileEncoding = "Windows-1250",
           row.names = FALSE)

PCA <- rda(data_tap, scale = FALSE)
plot(PCA, display = "species", type = "text")
speciesPCA <- PCA$CA$v
speciesPCA
biplot(PCA, choices = c(1,2))
biplot(PCA, xlim = c(-0.3, 0.3), ylim = c(-0.45, 0.6))



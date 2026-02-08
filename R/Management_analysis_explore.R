#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data <- 
  readr::read_csv(
    "Data/Processed/data_clean.csv"
  )

#----------------------------------------------------------#
# 2. General Data Description and Exploration -----
#----------------------------------------------------------#

occ_num <- 
  nrow(
    data
  )

positivity <-
  data %>%
  group_by(
    DRUH,
    YEAR
  ) %>%
  reframe(
    n(),
    mean_positive = mean(
      POSITIVE
    )
  )

data_sum <- data %>%
  st_drop_geometry() %>%
  group_by(DRUH, POSITIVE) %>%
  summarise(COUNT = n()) %>%
  ungroup()

sum(data_sum$COUNT)

# Check years per location
roky <- data %>%
  st_drop_geometry() %>%
  group_by(NAZ_LOKAL) %>%
  summarise(roky = length(unique(YEAR))) %>%
  arrange(-roky)
roky

#----------------------------------------------------------#
# 3. Temporal Analysis (Observation Year) -----
#----------------------------------------------------------#

data %>%
  group_by(YEAR) %>%
  summarise(length(unique(row_n)))

year_stats <-
  data %>%
  dplyr::group_by(YEAR, POSITIVE) %>%
  dplyr::summarise(COUNT = n())

# Simple trend check
data %>%
  group_by(DRUH,YEAR) %>%
  summarise(n()) %>%
  arrange(YEAR)

#----------------------------------------------------------#
# 4. Observers -----
#----------------------------------------------------------#

observer_stats <-
  data %>%
  dplyr::group_by(
    AUTOR
  ) %>%
  dplyr::reframe(
    obs_num = n()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(
    desc(
      obs_num
    )
  )

observer_list <-
  data %>%
  dplyr::pull(AUTOR) %>% 
  unique()

observer_number <-
  observer_list %>%
  length()

#----------------------------------------------------------#
# 5. Species Occupancy (Presence/Absence) -----
#----------------------------------------------------------#

# --- P. nausithous ---
Pnau_obs <- 
  data %>%
  filter(
    DRUH == "Phengaris nausithous"
  )

Pnau_obs_num <-
  Pnau_obs %>%
  nrow()

Pnau_num_obs_pos <- 
  Pnau_obs %>%
  filter(
    POSITIVE == 1
  ) %>%
  nrow()

Pnau_num_obs_neg <- 
  Pnau_obs %>%
  filter(
    POSITIVE == 0
  ) %>%
  nrow()

Pnau_num_obs_pos_perc <- 
  Pnau_num_obs_pos/Pnau_obs_num

# --- P. teleius ---
Ptel_obs <- 
  data %>%
  filter(
    DRUH == "Phengaris teleius"
  )

Ptel_obs_num <-
  Ptel_obs %>%
  nrow()

Ptel_num_obs_pos <- 
  Ptel_obs %>%
  filter(
    POSITIVE == 1
  ) %>%
  nrow()

Ptel_num_obs_neg <- 
  Ptel_obs %>%
  filter(
    POSITIVE == 0
  ) %>%
  nrow()

Ptel_num_obs_pos_perc <- 
  Ptel_num_obs_pos/Ptel_obs_num

# --- Specimen count summary ---
data_spe_sum <- data %>%
  st_drop_geometry() %>%
  group_by(POSITIVE, DRUH, SPEC_NUM) %>%
  summarise(COUNT = n()) %>%
  ungroup()

#----------------------------------------------------------#
# 6. Abundances (Population Size) -----
#----------------------------------------------------------#

# --- P. nausithous Stats ---
Pnau_abund <- 
  data %>%
  filter(DRUH == "Phengaris nausithous") %>%
  pull(POCET) %>%
  na.omit() 

Pnau_abund_mean <-
  Pnau_abund %>%
  median()

Pnau_abund_median <-
  Pnau_abund %>%
  median()

Pnau_abund_num <-
  Pnau_abund %>%
  length()

data %>%
  dplyr::filter(DRUH == "Phengaris nausithous")  %>%
  pull(POCET) %>%
  na.omit() %>%  
  mean()

data %>%
  dplyr::filter(DRUH == "Phengaris nausithous")  %>%
  pull(POCET) %>%
  na.omit() %>%  
  median()

data %>%
  dplyr::filter(DRUH == "Phengaris nausithous")  %>%
  pull(POCET) %>%
  na.omit() %>%  
  sd()

# --- P. teleius Stats ---
data %>%
  filter(DRUH == "Phengaris teleius") %>%
  pull(POCET) %>%
  na.omit() %>%
  median()

length(data %>%
         filter(DRUH == "Phengaris teleius") %>%
         pull(POCET) %>%
         na.omit())

data %>%
  dplyr::filter(DRUH == "Phengaris teleius")  %>%
  pull(POCET) %>%
  na.omit() %>%  
  mean()

data %>%
  dplyr::filter(DRUH == "Phengaris teleius")  %>%
  pull(POCET) %>%
  na.omit() %>%  
  median()

data %>%
  dplyr::filter(DRUH == "Phengaris teleius")  %>%
  pull(POCET) %>%
  na.omit() %>%  
  sd()

#----------------------------------------------------------#
# 7. Habitats and Host Plants -----
#----------------------------------------------------------#

habitat_counts <- data %>%
  dplyr::group_by(
    DRUH,
    POSITIVE
  ) %>%
  dplyr::reframe(
    TTP = sum(TTP == 1, na.rm = TRUE),
    ZARUST = sum(ZARUST == 1, na.rm = TRUE),
    PRIKOP = sum(PRIKOP == 1, na.rm = TRUE),
    JINY = sum(JINY == 1, na.rm = TRUE)
  )

# Host plant summaries
data %>%
  st_drop_geometry() %>%
  group_by(PLANT_QUANT, DRUH, POSITIVE) %>%
  summarise(COUNT = n())

data_plant_sum <- data %>%
  st_drop_geometry() %>%
  group_by(POSITIVE, DRUH, PLANT_QUANT) %>%
  summarise(COUNT = n()) %>%
  ungroup()

# Plot check for plants
data %>%
  filter(DRUH == "Phengaris nausithous") %>%
  group_by(PLANT_QUANT) %>%
  summarise(mean(POSITIVE)) %>%
  plot()

data %>%
  filter(DRUH == "Phengaris nausithous") %>%
  group_by(TTP) %>%
  summarise(n())

#----------------------------------------------------------#
# 8. Management Methods -----
#----------------------------------------------------------#

data_method_sum <- data %>%
  st_drop_geometry() %>%
  group_by(POSITIVE, DRUH, METHOD) %>%
  summarise(COUNT = n()) %>%
  ungroup()

data_time_sum <- 
  data %>%
  dplyr::group_by(POSITIVE, DRUH, TIMING) %>%
  dplyr::reframe(
    COUNT = n()
  ) %>%
  dplyr::ungroup()

data_man_sum <- 
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
    COUNT = n()
  ) %>%
  dplyr::ungroup()

#----------------------------------------------------------#
# 9. Spatial Analysis, Protected Areas & Mapping -----
#----------------------------------------------------------#

# --- Mapping Fields ---
mapfield <- 
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
    desc(
      POSITIVE
    )
  ) %>%
  dplyr::slice(
    1
  ) %>%
  dplyr::ungroup()

# --- Protected Areas Summaries ---
data_evl_sum <- data %>%
  dplyr::group_by(
    EVL, 
    EVL_target, 
    EVL_comb,
    MZCHU,
    POSITIVE, 
    DRUH
  ) %>%
  summarise(COUNT = n()) %>%
  ungroup()

data_evlcomb_sum <- data %>%
  st_drop_geometry() %>%
  group_by(EVL_comb, POSITIVE, DRUH) %>%
  summarise(COUNT = n()) %>%
  ungroup()

data_mzchu_sum <- data %>%
  st_drop_geometry() %>%
  group_by(MZCHU, POSITIVE, DRUH) %>%
  summarise(COUNT = n()) %>%
  ungroup()

# --- Visualisation (EVL) ---
ggplot(data = data_evl_sum %>%
         filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(EVL), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside Natura 2000", "Natura 2000 sites")) +
  xlab("\nPhengaris nausithous") +
  ylab("number of sites\n") +
  theme_classic() +
  theme(text = element_text(size = 26),
        legend.position="none")

ggplot(data = data_evl_sum %>%
         filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(EVL), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside Natura 2000", "Natura 2000 sites")) +
  xlab("\nPhengaris teleius") +
  ylab("") +
  theme_classic() +
  theme(text = element_text(size = 26))

#----------------------------------------------------------#
# 10. Miscellaneous Variable Checks -----
#----------------------------------------------------------#

data$UMIST_NAL %>% unique
data$POP_BIOT %>% unique
data$AREA_SITE %>% log10() %>% hist

#----------------------------------------------------------#
# 11. Save Results (DOCX & CSV) -----
#----------------------------------------------------------#
# This section requires 'officer' and 'flextable' packages
# Creates an 'Output' folder if it doesn't exist

if(!dir.exists("Outputs")) {
  dir.create("Outputs")
}

# Helper function to save CSV in Windows-1250 encoding
save_win_csv <- function(data_obj, filename) {
  write.csv(
    data_obj, 
    file = file.path("Outputs", filename), 
    fileEncoding = "Windows-1250", 
    row.names = FALSE
  )
}

# Helper function to add table to docx
add_flex <- function(doc, title, data) {
  doc %>% 
    officer::body_add_par(title, style = "heading 1") %>% 
    flextable::body_add_flextable(flextable::flextable(data)) %>%
    officer::body_add_par("", style = "Normal") # Spacer
}

# --- A. Save General & Temporal Stats ---
save_win_csv(positivity, "01_General_Positivity.csv")
save_win_csv(data_sum, "01_General_DataSum.csv")
save_win_csv(roky, "01_General_YearsPerLoc.csv")
save_win_csv(year_stats, "02_Temporal_YearStats.csv")

doc_general <- officer::read_docx() %>%
  add_flex("Positivity by Year/Species", positivity) %>%
  add_flex("Data Summary", data_sum) %>%
  add_flex("Years per Location", roky) %>%
  add_flex("Observation Counts per Year", year_stats)

print(doc_general, target = "Outputs/01_General_and_Temporal.docx")

# --- B. Save Observer Stats ---
save_win_csv(observer_stats, "03_Observer_Stats.csv")

doc_obs <- officer::read_docx() %>%
  add_flex("Observer Activity", observer_stats)

print(doc_obs, target = "Outputs/02_Observers.docx")

# --- C. Save Species & Abundance Summaries ---
save_win_csv(data_spe_sum, "04_Species_Specimen_Count.csv")

doc_spec <- officer::read_docx() %>%
  add_flex("Specimen Counts by Species", data_spe_sum)

print(doc_spec, target = "Outputs/03_Species_Abundance.docx")

# --- D. Save Habitat & Plant Stats ---
save_win_csv(habitat_counts, "05_Habitat_Counts.csv")
save_win_csv(data_plant_sum, "05_Plant_Summary.csv")

doc_hab <- officer::read_docx() %>%
  add_flex("Habitat Type Counts", habitat_counts) %>%
  add_flex("Host Plant Abundance", data_plant_sum)

print(doc_hab, target = "Outputs/04_Habitat_Plants.docx")

# --- E. Save Management Stats ---
save_win_csv(data_method_sum, "06_Management_Method.csv")
save_win_csv(data_time_sum, "06_Management_Timing.csv")
save_win_csv(data_man_sum, "06_Management_Combined.csv")

doc_man <- officer::read_docx() %>%
  add_flex("Management Methods", data_method_sum) %>%
  add_flex("Management Timing", data_time_sum) %>%
  add_flex("Combined Management Assessment", data_man_sum)

print(doc_man, target = "Outputs/05_Management.docx")

# --- F. Save Protected Areas & Mapping ---
save_win_csv(data_evl_sum, "07_Protected_EVL_Summary.csv")
save_win_csv(data_evlcomb_sum, "07_Protected_EVL_Combined.csv")
save_win_csv(data_mzchu_sum, "07_Protected_MZCHU_Summary.csv")
save_win_csv(mapfield, "08_Mapping_Field.csv")

doc_prot <- officer::read_docx() %>%
  add_flex("EVL Sites Summary", data_evl_sum) %>%
  add_flex("EVL Combined Stats", data_evlcomb_sum) %>%
  add_flex("MZCHU Sites Summary", data_mzchu_sum) %>%
  add_flex("Mapping Field Data", mapfield)

print(doc_prot, target = "Outputs/06_Protected_Areas_Mapping.docx")
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
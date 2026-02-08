#----------------------------------------------------------#
# Load data -----
#----------------------------------------------------------#

data <- 
  readr::read_csv(
    "Data/Processed/data_clean.csv"
    )

#----------------------------------------------------------#
# Plots -----
#----------------------------------------------------------#
#--------------------------------------------------#
## Year effects -----
#--------------------------------------------------#

ggplot(
  data = year_stats, 
  aes(y = as.numeric(COUNT), 
      x = as.factor(YEAR),
      color = as.factor(POSITIVE)
      )
  ) +
  geom_point() 

#--------------------------------------------------#
## Protected areas -----
#--------------------------------------------------#

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
  scale_x_discrete(labels = c("outside Natura 2000", "within Natura 2000")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

ggplot(data = data_mzchu_sum %>%
         filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(MZCHU), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside small-scale\nprotected site", "within small-scale\nprotected sites")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

#--------------------------------------------------#
## Occurrence patterns -----
#--------------------------------------------------#

ggplot(data = data_spe_sum %>%
         filter(SPEC_NUM == 0), 
       aes(x = as.factor(DRUH), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("Phengaris nausithous\nwithout P. teleius",
                              "Phengaris teleius\nwithout P. nausithous")) +
  xlab("") +
  ylab("number of sites\n") +
  theme_classic()

#--------------------------------------------------#
## Host plant -----
#--------------------------------------------------#

ggplot(data = data_plant_sum %>%
         filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(PLANT_QUANT), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("single plants", "abundant", "dominant")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

ggplot(data = data_plant_sum %>%
         filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(PLANT_QUANT), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("single plants", "abundant", "dominant")) +
  xlab("") +
  ylab("number of sites\n") +
  theme_classic()

#--------------------------------------------------#
## Observations plots -----
#--------------------------------------------------#

(histogram_observers <- 
   ggplot2::ggplot(
     observer_stats, 
     aes(
       x = obs_num
     )
   ) +
   ggplot2::geom_histogram(
     alpha = 0.6,
     breaks = seq(
       0, 
       max(
         observer_stats$obs_num
       ), 
       by = 25
     ),
     fill = "steelblue"
   ) +
   theme_minimal(
     base_size = 14
   ) +
   scale_y_continuous(
     expand = expansion(mult = c(0, 0.1))
   ) +
   geom_vline(
     xintercept = mean(observer_stats$obs_num),
     linetype = "dotted",
     colour = "steelblue", size = 1
   ) +
   annotate(
     "text", x = mean(observer_stats$obs_num) + 220, y = 20,
     label = paste0("Mean number of\nobservations: ", round(mean(observer_stats$obs_num), 1))
   ) +
   geom_curve(
     aes(
       x = mean(observer_stats$obs_num) + 100, y = 20,
       xend = mean(observer_stats$obs_num) + 10, yend = 15
     ),
     arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
     color = "grey30", curvature = 0.3
   ) +
   labs(
     x = "\nNumber of Observations",
     y = "Number of Observers\n",
     title = "Distribution of Observation Counts per Observer"
   )
)

#--------------------------------------------------#
## Management plots -----
#--------------------------------------------------#

ggplot(data = data_man_sum %>%
         filter(DRUH == "Phengaris nausithous") %>%
         filter(is.na(MANAGEMENT) == FALSE), 
       aes(x = as.factor(MANAGEMENT), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("appropriate mow and\nappropriate timing", 
                              "appropriate mow only", 
                              "appropriate timing only",
                              "inappropriate mow and\ninappropriate timing")) +
  xlab("\nrecorded management at managed sites with P. nausithous") +
  ylab("number of findings\n") +
  theme_classic()

ggplot(data = data_man_sum %>%
         filter(DRUH == "Phengaris teleius") %>%
         filter(is.na(MANAGEMENT) == FALSE), 
       aes(x = as.factor(MANAGEMENT), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("appropriate mow and\nappropriate timing", 
                              "appropriate mow only", 
                              "appropriate timing only",
                              "inappropriate mow and\ninappropriate timing")) +
  xlab("\nrecorded management at managed sites with P. teleius") +
  ylab("number of findings\n") +
  theme_classic()


ggplot(data = data_sum, 
       aes(x = as.factor(DRUH), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("without P. nausithous", "with P. nausithous")) +
  xlab("") +
  ylab("number of sites\n") +
  theme_classic()

data %>%
  filter(DRUH == "Phengaris nausithous" & POSITIVE == 1) %>%
  pull(SITMAP) %>%
  unique() %>%
  length()

data %>%
  filter(DRUH == "Phengaris teleius" & POSITIVE == 1) %>%
  pull(SITMAP) %>%
  unique() %>%
  length()

data %>%
  pull(SITMAP) %>%
  unique() %>%
  length()
data %>%
  pull(AUTOR) %>%
  unique() %>%
  length()


# MANAGEMENT 
summan <- data %>%
  st_drop_geometry() %>%
  filter(POSITIVE == 1) %>%
  group_by(MOW, GRAZE, ZARUST) %>%
  summarise(n())

summan_phenau <- data %>%
  st_drop_geometry() %>%
  dplyr::select(DRUH, POSITIVE, MOW, GRAZE, ZARUST) %>%
  pivot_longer(.,
               cols = c(3:5)) %>%
  filter(value == 1) %>%
  dplyr::select(-value) %>%
  filter(POSITIVE == 1) %>%
  filter(DRUH == "Phengaris nausithous") %>%
  group_by(name) %>%
  summarise(COUNT = n(),
            PERC = n()/nrow(data %>%
                              dplyr::filter(DRUH == "Phengaris nausithous") %>%
                              dplyr::filter(POSITIVE == 1))*100) %>%
  mutate(name = case_when(name == "MOW" ~ "mowing",
                          name == "ZARUST" ~ "neglected\ngrassland",
                          name == "GRAZE" ~ "grazing",
                          name == "PRIKOP" ~ "road verge,\nditch"))

summan_phetel <- data %>%
  st_drop_geometry() %>%
  dplyr::select(DRUH, POSITIVE, MOW, GRAZE, ZARUST) %>%
  pivot_longer(.,
               cols = c(3:5)) %>%
  filter(value == 1) %>%
  dplyr::select(-value) %>%
  filter(POSITIVE == 1) %>%
  filter(DRUH == "Phengaris teleius") %>%
  group_by(name) %>%
  summarise(COUNT = n(),
            PERC = n()/nrow(data %>%
                              dplyr::filter(DRUH == "Phengaris teleius") %>%
                              dplyr::filter(POSITIVE == 1))*100) %>%
  mutate(name = case_when(name == "MOW" ~ "mowing",
                          name == "ZARUST" ~ "neglected\ngrassland",
                          name == "GRAZE" ~ "grazing",
                          name == "PRIKOP" ~ "road verge,\nditch"))

ggplot(data = summan_phenau, 
       aes(x = fct_reorder(name, COUNT, .desc = TRUE), 
           y = COUNT)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  xlab("management recorded at sites with P. nausithous") +
  ylab("number of sites\n") +
  theme_classic() +
  theme(text = element_text(size = 20))

ggplot(data = summan_phetel, 
       aes(x = fct_reorder(name, COUNT, .desc = TRUE), 
           y = COUNT)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  xlab("management recorded at sites with  P. teleius") +
  ylab("number of sites\n") +
  theme_classic() +
  theme(text = element_text(size = 20))

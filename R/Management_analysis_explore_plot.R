#----------------------------------------------------------#
# Visualisations and Plot Exports -----
#----------------------------------------------------------#
library(ggplot2)
library(dplyr)
library(forcats)

# Create Plots directory
if(!dir.exists("Outputs/Plots")) {
  dir.create("Outputs/Plots", recursive = TRUE)
}

# --- General Plot: Site Occupancy by Species ---
ggplot(data = data_sum, 
       aes(x = as.factor(DRUH), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("P. nausithous", "P. teleius")) + # Corrected based on DRUH grouping
  xlab("\nSpecies") +
  ylab("number of sites\n") +
  theme_classic(base_size = 14) +
  theme(legend.position = "top")

ggsave("Outputs/Plots/01_General_SiteOccupancy_BothSpecies.png", dpi = 300, width = 8, height = 6)

# --- Plot: Year Effects ---
ggplot(
  data = year_stats, 
  aes(y = as.numeric(COUNT), 
      x = as.factor(YEAR),
      color = as.factor(POSITIVE)
  )
) +
  geom_point(size = 3, alpha = 0.7) +
  labs(x = "\nYear", y = "Observation Count\n", color = "Site Occupancy") +
  scale_color_manual(labels = c("negative", "positive"),
                     values = c("grey", "#595959")) +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Outputs/Plots/02_Temporal_YearEffects_BothSpecies.png", dpi = 300, width = 8, height = 6)

# --- Plot: Distribution of Observation Counts ---
(histogram_observers <- 
    ggplot2::ggplot(
      observer_stats, 
      aes(x = obs_num)
    ) +
    ggplot2::geom_histogram(
      alpha = 0.6,
      breaks = seq(0, max(observer_stats$obs_num, na.rm = TRUE), by = 25),
      fill = "steelblue"
    ) +
    theme_minimal(base_size = 14) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    geom_vline(
      xintercept = mean(observer_stats$obs_num, na.rm = TRUE),
      linetype = "dotted",
      colour = "steelblue", linewidth = 1
    ) +
    annotate(
      "text", x = mean(observer_stats$obs_num, na.rm = TRUE) + 220, y = 20,
      label = paste0("Mean observations: ", round(mean(observer_stats$obs_num, na.rm = TRUE), 1)),
      size = 5
    ) +
    labs(
      x = "\nNumber of Observations",
      y = "Number of Observers\n",
      title = "Distribution of Observation Counts per Observer"
    )
)

ggsave("Outputs/Plots/03_Observer_Distribution_BothSpecies.png", dpi = 300, width = 8, height = 6)

# --- Plot: Occurrence Patterns ---
ggplot(data = data_spe_sum %>% filter(SPEC_NUM == 0), 
       aes(x = as.factor(DRUH), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("P. nausithous\n(without P. teleius)",
                              "P. teleius\n(without P. nausithous)")) +
  xlab("\nSpecies Occurrence Context") +
  ylab("number of sites\n") +
  theme_classic(base_size = 14)

ggsave("Outputs/Plots/04_Species_Occurrence_BothSpecies.png", dpi = 300, width = 8, height = 6)

# --- Plots: Host Plant Quantities ---
ggplot(data = data_plant_sum %>% filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(PLANT_QUANT), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("single plants", "abundant", "dominant")) +
  xlab("\nHost Plant Quantity (P. nausithous)") +
  ylab("number of findings\n") +
  theme_classic(base_size = 14)

ggsave("Outputs/Plots/05_Plants_Pnausithous.png", dpi = 300, width = 8, height = 6)

ggplot(data = data_plant_sum %>% filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(PLANT_QUANT), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("single plants", "abundant", "dominant")) +
  xlab("\nHost Plant Quantity (P. teleius)") +
  ylab("number of sites\n") +
  theme_classic(base_size = 14)

ggsave("Outputs/Plots/05_Plants_Pteleius.png", dpi = 300, width = 8, height = 6)

# --- Plots: Management Appropriateness ---
ggplot(data = data_man_sum %>%
         filter(DRUH == "Phengaris nausithous", !is.na(MANAGEMENT)), 
       aes(x = as.factor(MANAGEMENT), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("appropriate mow\n& timing", 
                              "appropriate\nmow only", 
                              "appropriate\ntiming only",
                              "inappropriate mow\n& timing")) +
  xlab("\nRecorded management at sites with P. nausithous") +
  ylab("number of findings\n") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(size = 12))

ggsave("Outputs/Plots/06_Management_Appropriateness_Pnausithous.png", dpi = 300, width = 9, height = 6)

ggplot(data = data_man_sum %>%
         filter(DRUH == "Phengaris teleius", !is.na(MANAGEMENT)), 
       aes(x = as.factor(MANAGEMENT), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("appropriate mow\n& timing", 
                              "appropriate\nmow only", 
                              "appropriate\ntiming only",
                              "inappropriate mow\n& timing")) +
  xlab("\nRecorded management at sites with P. teleius") +
  ylab("number of findings\n") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(size = 12))

ggsave("Outputs/Plots/06_Management_Appropriateness_Pteleius.png", dpi = 300, width = 9, height = 6)

# --- Plots: Specific Management Types ---
ggplot(data = summan_phenau, 
       aes(x = fct_reorder(name, COUNT, .desc = TRUE), 
           y = COUNT)) +
  geom_bar(stat = "identity", fill = "#595959") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  xlab("\nManagement recorded at sites with P. nausithous") +
  ylab("number of sites\n") +
  theme_classic(base_size = 16)

ggsave("Outputs/Plots/06_Management_Types_Pnausithous.png", dpi = 300, width = 8, height = 6)

ggplot(data = summan_phetel, 
       aes(x = fct_reorder(name, COUNT, .desc = TRUE), 
           y = COUNT)) +
  geom_bar(stat = "identity", fill = "#595959") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  xlab("\nManagement recorded at sites with P. teleius") +
  ylab("number of sites\n") +
  theme_classic(base_size = 16)

ggsave("Outputs/Plots/06_Management_Types_Pteleius.png", dpi = 300, width = 8, height = 6)

# --- Plots: Protected Areas (EVL & MZCHU) ---
ggplot(data = data_evl_sum %>% filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(EVL), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside Natura 2000", "Natura 2000 sites")) +
  xlab("\nPhengaris nausithous") +
  ylab("number of sites\n") +
  theme_classic(base_size = 18)

ggsave("Outputs/Plots/07_Protected_EVL_Pnausithous.png", dpi = 300, width = 8, height = 6)

ggplot(data = data_evl_sum %>% filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(EVL), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside Natura 2000", "within Natura 2000")) +
  xlab("\nPhengaris teleius") +
  ylab("number of findings\n") +
  theme_classic(base_size = 18)

ggsave("Outputs/Plots/07_Protected_EVL_Pteleius.png", dpi = 300, width = 8, height = 6)

ggplot(data = data_mzchu_sum %>% filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(MZCHU), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside small-scale\nprotected site", "within small-scale\nprotected sites")) +
  xlab("\nPhengaris nausithous") +
  ylab("number of findings\n") +
  theme_classic(base_size = 16)

ggsave("Outputs/Plots/07_Protected_MZCHU_Pnausithous.png", dpi = 300, width = 8, height = 6)
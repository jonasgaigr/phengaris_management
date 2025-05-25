#----------------------------------------------------------#
# Load data -----
#----------------------------------------------------------#

data <- 
  readr::read_csv(
    "Data/Processed/data_clean.csv"
    )

#----------------------------------------------------------#
# Data description and exploration -----
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
#----------------------------------------------------------#
## Observation year -----
#----------------------------------------------------------#

#----------------------------------------------------------#
## Observers -----
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

data %>%
  group_by(YEAR) %>%
  summarise(length(unique(POLE)))

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



#----------------------------------------------------------#
## Observations -----
#----------------------------------------------------------#
nrow(data %>%
       filter(DRUH == "Phengaris nausithous"))
nrow(data %>%
       filter(DRUH == "Phengaris nausithous" & POSITIVE == 1))
nrow(data %>%
       filter(DRUH == "Phengaris nausithous" & POSITIVE == 0))
nrow(data %>% 
       filter(DRUH == "Phengaris nausithous" & POSITIVE == 1))/
nrow(data %>%
       filter(DRUH == "Phengaris nausithous"))

nrow(data %>%
       filter(DRUH == "Phengaris teleius"))
nrow(data %>%
       filter(DRUH == "Phengaris teleius" & POSITIVE == 1))
nrow(data %>%
       filter(DRUH == "Phengaris teleius" & POSITIVE == 0))
nrow(data %>%
       filter(DRUH == "Phengaris teleius" & POSITIVE == 1))/
nrow(data %>%
       filter(DRUH == "Phengaris teleius"))

data_spe_sum <- data %>%
  st_drop_geometry() %>%
  group_by(POSITIVE, DRUH, SPEC_NUM) %>%
  summarise(COUNT = n()) %>%
  ungroup()

data_sum <- data %>%
  st_drop_geometry() %>%
  group_by(DRUH, POSITIVE) %>%
  summarise(COUNT = n()) %>%
  ungroup()

sum(data_sum$COUNT)
sum(data_evl_sum$COUNT)

#----------------------------------------------------------#
## Habitats -----
#----------------------------------------------------------#
nrow(data %>%
       filter(DRUH == "Phengaris nausithous" & 
                POSITIVE == 1 &
                TTP == 1))
nrow(data %>%
       filter(DRUH == "Phengaris nausithous" & 
                POSITIVE == 1 &
                ZARUST == 1))
nrow(data %>%
       filter(DRUH == "Phengaris nausithous" & 
                POSITIVE == 1 &
                PRIKOP == 1))
nrow(data %>%
       filter(DRUH == "Phengaris nausithous" & 
                POSITIVE == 1 &
                JINY == 1))

nrow(data %>%
       filter(DRUH == "Phengaris teleius" & 
                POSITIVE == 1 &
                TTP == 1))
nrow(data %>%
       filter(DRUH == "Phengaris teleius" & 
                POSITIVE == 1 &
                ZARUST == 1))
nrow(data %>%
       filter(DRUH == "Phengaris teleius" & 
                POSITIVE == 1 &
                PRIKOP == 1))
data %>%
  filter(DRUH == "Phengaris teleius" & 
           POSITIVE == 1 &
           JINY == 1) %>%
  nrow()


#----------------------------------------------------------#
## Abundances -----
#----------------------------------------------------------#
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

#----------------------------------------------------------#
## Host plant -----
#----------------------------------------------------------#

data %>%
  st_drop_geometry() %>%
  group_by(PLANT_QUANT, DRUH, POSITIVE) %>%
  summarise(COUNT = n())

data_plant_sum <- data %>%
  st_drop_geometry() %>%
  group_by(POSITIVE, DRUH, PLANT_QUANT) %>%
  summarise(COUNT = n()) %>%
  ungroup()

#----------------------------------------------------------#
## Protected areas -----
#----------------------------------------------------------#

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
  st_drop_geometry() %>% 
  group_by(YEAR, POSITIVE) %>%
  summarise(COUNT = n())


data_evl_sum <- data %>%
  st_drop_geometry() %>%
  group_by(EVL, POSITIVE, DRUH) %>%
  summarise(COUNT = n()) %>%
  ungroup()
data_evltar_sum <- data %>%
  st_drop_geometry() %>%
  group_by(EVL_target, POSITIVE, DRUH) %>%
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
## Management methods -----
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

data$UMIST_NAL %>% unique
data$POP_BIOT %>% unique
data$AREA_SITE %>% log10() %>% hist

roky <- data %>%
  st_drop_geometry() %>%
  group_by(NAZ_LOKAL) %>%
  summarise(roky = length(unique(YEAR))) %>%
  arrange(-roky)
roky

kuk <- data %>%
  filter(DRUH == "Phengaris teleius") %>%
  st_drop_geometry() %>%
  group_by(YEAR) %>%
  summarise(n())

data %>%
  filter(DRUH == "Phengaris nausithous") %>%
  st_drop_geometry() %>%
  group_by(PLANT_QUANT) %>%
  summarise(mean(POSITIVE)) %>%
  plot()

phenpole <- data %>%
  st_intersection(., sitmap)

phenpole %>%
  pull(POLE) %>%
  unique() %>%
  length()

phenpole %>%
  st_drop_geometry() %>%
  group_by(YEAR) %>%
  summarise(n())

phenpole %>%
  st_drop_geometry() %>%
  filter(POSITIVE == 1) %>%
  filter(DRUH == "Phengaris nausithous") %>%
  pull(POLE) %>%
  unique() %>%
  length

phenpole %>%
  st_drop_geometry() %>%
  filter(POSITIVE == 1) %>%
  filter(DRUH == "Phengaris teleius") %>%
  pull(POLE) %>%
  unique() %>%
  length

phenpole %>%
  st_drop_geometry() %>%
  filter(SPEC_NUM == 1) %>%
  pull(POLE) %>%
  unique() %>%
  length
data %>%
  st_drop_geometry() %>%
  filter(POSITIVE == 1) %>%
  filter(SPEC_NUM == 1) %>%
  nrow()

phenpole %>%
  st_drop_geometry() %>%
  filter(DRUH == "Phengaris nausithous") %>%
  filter(SPEC_NUM == 1) %>%
  nrow()

phenpole_analysis <- phenpole %>%
  dplyr::select(DRUH, POSITIVE, POLE) %>%
  st_drop_geometry() %>%
  group_by(DRUH, POLE) %>%
  arrange(-POSITIVE) %>%
  slice(1) %>%
  ungroup()

kukuk <- data %>%
  dplyr::select(BIOTOP, NATURAL)
kukukuk <- data %>%
  st_drop_geometry() %>%
  filter(DRUH == "Phengaris nausithous") %>%
  group_by(TTP) %>%
  summarise(n())


#----------------------------------------------------------#
# Models -----
#----------------------------------------------------------#
#----------------------------------------------------------#
## Phengaris nausithous -----
#----------------------------------------------------------#
# NULL
model_null_phenau <- 
  glmer(
    data = data %>%
      filter(
        DRUH == "Phengaris nausithous"
        ), 
    as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
    family ="binomial"
    )

summary(model_null_phenau)

model_null_phetel <- glmer(data = data %>%
                           filter(DRUH == "Phengaris teleius"), 
                         as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
                         family ="binomial")
summary(model_null_phetel)

# TEST THE YEAR EFFECT
model_nau_yf <- glm(data = data %>%
                 filter(DRUH == "Phengaris nausithous"), 
               as.factor(POSITIVE) ~ as.factor(YEAR),
               family ="binomial")
summary(model_nau_yf)

model_nau_yl <- glm(data = data %>%
                  filter(DRUH == "Phengaris nausithous"), 
                as.factor(POSITIVE) ~ as.numeric(YEAR),
                family ="binomial")
summary(model_nau_yl)

model_nau_pyl <- glm(data = data %>%
                  filter(DRUH == "Phengaris nausithous"), 
                as.factor(POSITIVE) ~ poly(as.numeric(YEAR), 2),
                family ="binomial")
summary(model_nau_pyl)

# tel
model_tel_yf <- glm(data = data %>%
                      filter(DRUH == "Phengaris teleius"), 
                    as.factor(POSITIVE) ~ as.factor(YEAR),
                    family ="binomial")
summary(model_tel_yf)

model_tel_yl <- glm(data = data %>%
                      filter(DRUH == "Phengaris teleius"), 
                    as.factor(POSITIVE) ~ as.numeric(YEAR),
                    family ="binomial")
summary(model_tel_yl)

model_tel_pyl <- glm(data = data %>%
                       filter(DRUH == "Phengaris teleius"), 
                     as.factor(POSITIVE) ~ poly(as.numeric(YEAR), 2),
                     family ="binomial")
summary(model_tel_pyl)


# SPATIAL NAUSITHOUS
model_spat_phenau <- glm(data = data %>%
                           filter(DRUH == "Phengaris nausithous"), 
                         as.factor(POSITIVE) ~ X:Y,
                         family ="binomial")
summary(model_spat_phenau)

# SPATIAL TELEIUS
model_spat_phetel <- glm(data = data %>%
                           filter(DRUH == "Phengaris teleius"), 
                         as.factor(POSITIVE) ~ X:Y,
                         family ="binomial")
summary(model_spat_phetel)

# SPATIOTEMT
model_spatemp_phenau <- glm(data = data %>%
                           filter(DRUH == "Phengaris nausithous"), 
                         as.factor(POSITIVE) ~ as.factor(YEAR) + X:Y,
                         family ="binomial")
summary(model_spatemp_phenau)

model_spatemp_phetel <- glm(data = data %>%
                              filter(YEAR != "2018") %>%
                              filter(DRUH == "Phengaris teleius"), 
                            as.factor(POSITIVE) ~ as.numeric(YEAR) + X:Y,
                            family ="binomial")
summary(model_spatemp_phetel)

# AREA NULL 
model_arean_phenau <- glmer(data = data %>%
                             filter(DRUH == "Phengaris nausithous") %>%
                             filter(AREA_SITE > 0), 
                           as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
                           family ="binomial")
summary(model_arean_phenau)

model_arean_phetel <- glmer(data = data %>%
                              filter(DRUH == "Phengaris teleius") %>%
                              filter(AREA_SITE > 0), 
                            as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_arean_phetel)


# AREA
model_area_phenau <- glmer(data = data %>%
                         filter(DRUH == "Phengaris nausithous") %>%
                         filter(AREA_SITE > 0), 
                       as.factor(POSITIVE) ~ log10(AREA_SITE) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_area_phenau)

model_area_phetel <- glmer(data = data %>%
                             filter(DRUH == "Phengaris teleius") %>%
                             filter(AREA_SITE > 0), 
                           as.factor(POSITIVE) ~ log10(AREA_SITE) + (1 | YEAR) + (1 | X:Y),
                           family ="binomial")
summary(model_area_phetel)


model_arean_phenau <- glm(data = data %>%
                             filter(DRUH == "Phengaris nausithous") %>%
                             filter(is.na(AREA_SITE) == FALSE), 
                           as.factor(POSITIVE) ~ 1,
                           family ="binomial")
summary(model_arean_phenau)

model_arean_phetel <- glm(data = data %>%
                             filter(DRUH == "Phengaris teleius") %>%
                             filter(AREA_SITE > 0), 
                           as.factor(POSITIVE) ~ 1,
                           family ="binomial")
summary(model_arean_phetel)

ggplot(data = data %>%
         filter(DRUH == "Phengaris nausithous") %>%
         filter(AREA_SITE > 0), 
       aes(x = as.factor(POSITIVE), 
           y = log10(AREA_SITE),
           fill = as.factor(PLANT_QUANT))) +
  #geom_violin() +
  geom_boxplot() +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_x_discrete(labels = c("vacant sites", "occupied sites")) +
  scale_fill_discrete(labels = c("single plants", "abundant", "dominant"),
                      name=NULL) +
  xlab("\noccupancy of P. nausithous sites") +
  ylab("log10(site area)\n") +
  theme_classic() +
  theme(text = element_text(size = 20))

ggplot(data = data %>%
         filter(DRUH == "Phengaris teleius") %>%
         filter(AREA_SITE > 0), 
       aes(x = as.factor(POSITIVE), 
           y = log10(AREA_SITE),
           fill = as.factor(PLANT_QUANT))) +
  #geom_violin() +
  geom_boxplot() +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_x_discrete(labels = c("vacant sites", "occupied sites")) +
  scale_fill_discrete(labels = c("single plants", "abundant", "dominant"),
                      name=NULL) +
  xlab("\noccupancy of P. teleius sites") +
  ylab("log10(site area)\n") +
  theme_classic() +
  theme(text = element_text(size = 20))

# AREA POLY
model_spatar_phenau <- glmer(data = data %>%
                         filter(DRUH == "Phengaris nausithous") %>%
                         filter(AREA_SITE > 0), 
                       as.factor(POSITIVE) ~ poly(log10(AREA_SITE), 2) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_spatar_phenau)


# GRAF HABITATU 



# PLANT QUANTITY
model_plant_phenau <- glmer(data = data %>%
                  filter(DRUH == "Phengaris nausithous") %>%
                    filter(AREA_SITE > 0), 
                  as.factor(POSITIVE) ~ PLANT_QUANT + (1 | YEAR) + (1 | X:Y),
                family ="binomial")
summary(model_plant_phenau)

model_plant_phetel <- glmer(data = data %>%
                              filter(DRUH == "Phengaris teleius") %>%
                              filter(AREA_SITE > 0), 
                            as.factor(POSITIVE) ~ PLANT_QUANT + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_plant_phetel)

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


# PLANT QUANTITY POLY
model_plantpoly_phenau <- glmer(data = data %>%
                              filter(DRUH == "Phengaris nausithous"), 
                            as.factor(POSITIVE) ~ poly(PLANT_QUANT, 2) + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_plantpoly_phenau)

model_plantpoly_phetel <- glmer(data = data %>%
                                  filter(DRUH == "Phengaris teleius"), 
                                as.factor(POSITIVE) ~ poly(PLANT_QUANT, 2) + (1 | YEAR) + (1 | X:Y),
                                family ="binomial")
summary(model_plantpoly_phetel)

# RESOURCE DENSITY
model_dens_phenau <- glmer(data = data %>%
                             filter(DRUH == "Phengaris nausithous") %>%
                             filter(AREA_SITE > 0), 
                             as.factor(POSITIVE) ~ log(AREA_SITE)*as.numeric(PLANT_QUANT) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_dens_phenau)

model_dens_phetel <- glmer(data = data %>%
                             filter(DRUH == "Phengaris teleius"), 
                           as.factor(POSITIVE) ~ log(AREA_SITE)*as.numeric(PLANT_QUANT) + (1 | YEAR) + (1 | X:Y),
                           family ="binomial")
summary(model_dens_phetel)

# MOW NULL
model_mann_phenau <- glmer(data = data %>%
                            filter(DRUH == "Phengaris nausithous") %>%
                            filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                          as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_mann_phenau)

model_mann_phetel <- glmer(data = data %>%
                             filter(DRUH == "Phengaris teleius") %>%
                             filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                           as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
                           family ="binomial")
summary(model_mann_phetel)

# TIMING
model_tim_phenau <- glmer(data = data %>%
                  filter(DRUH == "Phengaris nausithous") %>%
                    filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                  as.factor(POSITIVE) ~ as.factor(TIMING) + (1 | YEAR) + (1 | X:Y),
                family ="binomial")
summary(model_tim_phenau)

model_tim_phetel <- glmer(data = data %>%
                            filter(DRUH == "Phengaris teleius") %>%
                            filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                          as.factor(POSITIVE) ~ as.factor(TIMING) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_tim_phetel)

# METHOD
model_met_phenau <- glmer(data = data %>%
                            filter(DRUH == "Phengaris nausithous") %>%
                            filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                          as.factor(POSITIVE) ~ as.factor(METHOD) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_met_phenau)
summary(model_met_phenau)$AIC

model_met_phetel <- glmer(data = data %>%
                            filter(DRUH == "Phengaris teleius") %>%
                            filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                          as.factor(POSITIVE) ~ as.factor(METHOD) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_met_phetel)

# METHOD & TIMING - SELECTED
model_timmet_phenau <- glmer(data = data %>%
                            filter(DRUH == "Phengaris nausithous") %>%
                              filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                          as.factor(POSITIVE) ~ as.factor(METHOD)*as.factor(TIMING) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_timmet_phenau)
summary(model_timmet_phenau)$AIC

model_timmet_phetel <- glmer(data = data %>%
                               filter(DRUH == "Phengaris teleius") %>%
                               filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                             as.factor(POSITIVE) ~ as.factor(METHOD)*as.factor(TIMING) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_timmet_phetel)

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

# NULL MOW
model_nullm_phenau <- glm(data = data %>%
                           filter(DRUH == "Phengaris nausithous") %>%
                            filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                         as.factor(POSITIVE) ~ 1,
                         family ="binomial")
summary(model_nullm_phenau)

model_nullm_phetel <- glm(data = data %>%
                           filter(DRUH == "Phengaris teleius") %>%
                            filter(is.na(METHOD) == FALSE & is.na(TIMING) == FALSE), 
                         as.factor(POSITIVE) ~ 1,
                         family ="binomial")
summary(model_nullm_phetel)

# PASTVA
model_grazen_phenau <- glmer(data = data %>%
                              filter(DRUH == "Phengaris nausithous") %>%
                             filter(is.na(GRAZE_MET) == FALSE), 
                            as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_grazen_phenau)
model_graze_phenau <- glmer(data = data %>%
                               filter(DRUH == "Phengaris nausithous"), 
                             as.factor(POSITIVE) ~ as.factor(GRAZE) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_graze_phenau)
summary(model_timmet_phenau)$AIC
model_grazemet_phenau <- glmer(data = data %>%
                              filter(DRUH == "Phengaris nausithous"), 
                            as.factor(POSITIVE) ~ as.factor(GRAZE_MET) + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_grazemet_phenau)

model_grazen_phetel <- glmer(data = data %>%
                             filter(DRUH == "Phengaris teleius") %>%
                             filter(is.na(GRAZE_MET) == FALSE), 
                           as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
                           family ="binomial")
summary(model_grazen_phetel)

model_graze_phetel <- glmer(data = data %>%
                              filter(DRUH == "Phengaris teleius"), 
                            as.factor(POSITIVE) ~ as.factor(GRAZE) + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_graze_phetel)
model_grazemet_phetel <- glmer(data = data %>%
                              filter(DRUH == "Phengaris teleius"), 
                            as.factor(POSITIVE) ~ as.factor(GRAZE_MET) + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_grazemet_phetel)

model_manage_phenau <- glmer(data = data %>%
                               filter(DRUH == "Phengaris teleius"), 
                             as.factor(POSITIVE) ~ as.factor(GRAZE_MET) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_grazemet_phetel)

# TYP MANAGEMENTU
model_graze_both <- glmer(data = data, 
                            as.factor(POSITIVE) ~ as.factor(DRUH) + as.factor(MOW) + as.factor(GRAZE) + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")


# TIMING METHOD HETEROGENITA COMBINATION
model_mancom_phenau <- glmer(data = data %>%
                  filter(DRUH == "Phengaris nausithous"), 
                  as.factor(POSITIVE) ~ as.factor(TIMING)*as.factor(METHOD)*HET_INN + (1 | YEAR) + (1 | X:Y),
                family ="binomial")
summary(model_mancom_phenau)

summary(model_tim_phenau)$AIC
summary(model_met_phenau)$AIC
summary(model_timmet_phenau)$AIC
summary(model_mancom_phenau)$AIC

ggplot(data = data_time_sum %>%
         filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(TIMING), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("harmful mowing time", "correct mowing time")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

ggplot(data = data_method_sum %>%
         filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(METHOD), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("whole area mowing method", "partial mowing method")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

# PROTECTION
model_pro_phenau <- glmer(data = data %>%
                            filter(DRUH == "Phengaris nausithous"), 
                          as.factor(POSITIVE) ~ as.factor(PROTECT) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_pro_phenau)

model_pro_phetel <- glmer(data = data %>%
                            filter(DRUH == "Phengaris teleius"), 
                          as.factor(POSITIVE) ~ as.factor(PROTECT) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_pro_phetel)

# EVL
model_evl_phenau <- glmer(data = data %>%
                         filter(DRUH == "Phengaris nausithous"), 
                       as.factor(POSITIVE) ~ as.factor(EVL) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_evl_phenau)

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
  scale_x_discrete(labels = c("outside Natura 2000", "within Natura 2000")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

# EVL TARGET
model_evltar_phenau <- glmer(data = data %>%
                            filter(DRUH == "Phengaris nausithous"), 
                          as.factor(POSITIVE) ~ as.factor(EVL_target) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_evltar_phenau)

ggplot(data = data_evltar_sum %>%
         filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(EVL_target), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside Natura 2000\nestablished for P. teleius", 
                              "within Natura 2000\nestablished for P. teleius")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

# EVL COMBINATION - SELECTED
model_evlcom_phenau <- glmer(data = data %>%
                               filter(DRUH == "Phengaris nausithous"), 
                             as.factor(POSITIVE) ~ as.factor(EVL) + as.factor(EVL_target) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_evlcom_phenau)

model_evlcom_phetel <- glmer(data = data %>%
                               filter(DRUH == "Phengaris teleius"), 
                             as.factor(POSITIVE) ~ as.factor(EVL) + as.factor(EVL_target) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_evlcom_phetel)

summary(model_evl_phenau)$AIC
summary(model_evltar_phenau)$AIC
summary(model_evlcom_phenau)$AIC

ggplot(data = data_evlcomb_sum %>%
         filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(EVL_comb), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside Natura 2000", 
                              "Natura 2000 NOT established\nfor P. nausithous",
                              "Natura 2000 established\nfor P. nausithous")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

ggplot(data = data_evlcomb_sum %>%
         filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(EVL_comb), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside Natura 2000", 
                              "Natura 2000 NOT established\nfor P. teleius",
                              "Natura 2000 established\nfor P. teleius")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

# EVL*ROK
model_evly_phenau <- glmer(data = data %>%
                            filter(DRUH == "Phengaris nausithous"), 
                          as.factor(POSITIVE) ~ as.factor(EVL)*as.numeric(YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_evly_phenau)

# MZCHU
model_mzchu_phenau <- glmer(data = data %>%
                         filter(DRUH == "Phengaris nausithous"), 
                       as.factor(POSITIVE) ~ as.factor(MZCHU) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_mzchu_phenau)

model_mzchu_phetel <- glmer(data = data %>%
                              filter(DRUH == "Phengaris teleius"), 
                            as.factor(POSITIVE) ~ as.factor(MZCHU) + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_mzchu_phetel)

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

# MZCHU & EVL
model_mzevl_phenau <- glmer(data = data %>%
                         filter(DRUH == "Phengaris nausithous"), 
                       as.factor(POSITIVE) ~ as.factor(MZCHU) * as.factor(EVL) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_mzevl_phenau)

# TTP
model6_phenau <- glmer(data = data %>%
                         filter(DRUH == "Phengaris nausithous"), 
                       as.factor(POSITIVE) ~ as.factor(TTP) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model6_phenau)

# MANAGEMENT & TTP
model7_phenau <- glmer(data = data %>%
                         filter(DRUH == "Phengaris nausithous"), 
                       as.factor(POSITIVE) ~ as.factor(TIMING)*as.factor(METHOD)*as.factor(TTP) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model7_phenau)
summary(model6_phenau)$AIC
summary(model7_phenau)$AIC

# FSB
model_fsb_phenau <- glmer(data = data %>%
                            filter(DRUH == "Phengaris nausithous"), 
                          as.factor(POSITIVE) ~ as.factor(FSB) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_fsb_phenau)

model_fsb_phetel <- glmer(data = data %>%
                            filter(DRUH == "Phengaris teleius"), 
                          as.factor(POSITIVE) ~ as.factor(FSB) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_fsb_phetel)

# BIOTOPY + DRUH
model_biomap_both <- glmer(data = data, 
                            as.factor(POSITIVE) ~ as.factor(DRUH) + as.factor(BIOTOP) + (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_biomap_both)

model_biorec_both <- glmer(data = data, 
                            as.factor(POSITIVE) ~ as.factor(DRUH) + 
                             as.factor(TTP) + as.factor(ZARUST) + as.factor(PRIKOP) + as.factor(JINY) +
                             as.factor(MOW) + as.factor(ZARUST) + 
                             (1 | YEAR) + (1 | X:Y),
                            family ="binomial")
summary(model_biorec_both)

# VNITŘNÍ HETEROGENITA 
model_hetinn_phenau <- glmer(data = data %>%
                               filter(DRUH == "Phengaris nausithous"), 
                             as.factor(POSITIVE) ~ HET_INN + (1 | YEAR) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_hetinn_phenau)

model_hetinn_phetel <- glmer(data = data %>%
                               filter(DRUH == "Phengaris teleius"), 
                             as.factor(POSITIVE) ~ HET_INN + (1 | YEAR) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_hetinn_phetel)

# VNĚJŠÍ  HETEROGENITA 
model_hetout_phenau <- glmer(data = data %>%
                               filter(DRUH == "Phengaris nausithous"), 
                             as.factor(POSITIVE) ~ as.numeric(HET_OUT) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_hetout_phenau)
model_hetout_phenau <- glmer(data = data %>%
                               filter(DRUH == "Phengaris nausithous"), 
                             as.factor(POSITIVE) ~ as.numeric(HET_OUT) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_hetout_phenau)

model_hetout_phetel <- glmer(data = data %>%
                               filter(DRUH == "Phengaris teleius"), 
                             as.factor(POSITIVE) ~ HET_OUT + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_hetout_phetel)

# THREATS
model_tap_phenau <- glmer(data = data %>%
                               filter(DRUH == "Phengaris nausithous") %>%
                             filter(AREA_SITE > 0), 
                             as.factor(POSITIVE) ~ SUM_THREATS + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_tap_phenau)

#----------------------------------------------------------#
## Phengaris teleius -----
#----------------------------------------------------------#
# NULL
model_null_phetel <- glm(data = data %>%
                             filter(DRUH == "Phengaris teleius"), 
                           as.factor(POSITIVE) ~ 1,
                           family ="binomial")
summary(model_null_phetel)

# TEST THE YEAR EFFECT
model_y <- glm(data = data %>%
                 filter(DRUH == "Phengaris teleius"), 
               as.factor(POSITIVE) ~ as.factor(YEAR),
               family ="binomial")
summary(model_y)

model_yl <- glm(data = data %>%
                 filter(DRUH == "Phengaris teleius"), 
               as.factor(POSITIVE) ~ as.numeric(YEAR),
               family ="binomial")
summary(model_yl)

model_pyl <- glm(data = data %>%
                  filter(DRUH == "Phengaris teleius"), 
                as.factor(POSITIVE) ~ poly(as.numeric(YEAR), 2),
                family ="binomial")
summary(model_pyl)

# TEST THE SITE EFFECT
model_p <- glm(data = data %>%
                 filter(DRUH == "Phengaris teleius"), 
               as.factor(POSITIVE) ~ as.factor(SITMAP),
               family ="binomial")
summary(model_p)

model_xy <- glmer(data = data %>%
                 filter(DRUH == "Phengaris teleius"), 
               as.factor(POSITIVE) ~ (1 | YEAR) + (1 | X:Y),
               family ="binomial")
summary(model_xy)

# AREA - SELECTED
model0_phetel <- glmer(data = data %>%
                         filter(DRUH == "Phengaris teleius") %>%
                         filter(AREA_SITE > 0), 
                       as.factor(POSITIVE) ~ log10(AREA_SITE) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model0_phetel)

ggplot(data = data %>%
         filter(DRUH == "Phengaris nausithous") %>%
         filter(AREA_SITE > 0), 
       aes(x = as.factor(POSITIVE), 
           y = log10(AREA_SITE),
           fill = as.factor(PLANT_QUANT))) +
  #geom_violin() +
  geom_boxplot() +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_discrete(labels = c("single plants", "abundant", "dominant")) +
  xlab("") +
  ylab("log10(AREA)\n") +
  theme_classic()

# PLANT
model1_phetel <- glmer(data = data %>%
                         filter(DRUH == "Phengaris teleius"), 
                       as.factor(POSITIVE) ~ PLANT_QUANT + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model1_phetel)
summary(model0_phetel)$AIC
summary(model1_phetel)$AIC

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

# PLANT QUANTITY POLY
model_plantpoly_phetel <- glmer(data = data %>%
                                  filter(DRUH == "Phengaris teleius"), 
                                as.factor(POSITIVE) ~ poly(PLANT_QUANT, 2) + (1 | YEAR) + (1 | X:Y),
                                family ="binomial")
summary(model_plantpoly_phetel)

# RESOURCE DENSITY
model_dens_phetel <- glmer(data = data %>%
                             filter(DRUH == "Phengaris teleius"), 
                           as.factor(POSITIVE) ~ log(AREA_SITE)*as.numeric(PLANT_QUANT) + (1 | YEAR) + (1 | X:Y),
                           family ="binomial")
summary(model_dens_phetel)

# METHOD
model2_phetel <- glmer(data = data %>%
                         filter(DRUH == "Phengaris teleius"), 
                       as.factor(POSITIVE) ~ as.factor(METHOD) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model2_phetel)

# TIMING
model3_phetel <- glmer(data = data %>%
                         filter(DRUH == "Phengaris teleius"), 
                       as.factor(POSITIVE) ~ as.factor(TIMING) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model3_phetel)

# METHOD AND TIMING
model_mancom_phetel <- glmer(data = data %>%
                         filter(DRUH == "Phengaris teleius"), 
                       as.factor(POSITIVE) ~ as.factor(METHOD)*as.factor(TIMING) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_mancom_phetel)

summary(model3_phetel)$AIC
summary(model2_phetel)$AIC
summary(model_mancom_phetel)$AIC

ggplot(data = data_time_sum %>%
         filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(TIMING), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("harmful mowing time", "correct mowing time")) +
  xlab("") +
  ylab("number of sites\n") +
  theme_classic()

ggplot(data = data_method_sum %>%
         filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(METHOD), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("whole area mowing method", "partial mowing method")) +
  xlab("") +
  ylab("number of sites\n") +
  theme_classic()

data %>%
  filter(DRUH == "Phengaris teleius") %>%
  filter(POSITIVE == 1) %>%
  filter(METHOD == 1 & TIMING == 1) %>%
  filter(EVL == 1) %>%
  nrow()

# EVL
model_evl_phetel <- glmer(data = data %>%
                            filter(DRUH == "Phengaris teleius"), 
                          as.factor(POSITIVE) ~ as.factor(EVL) + (1 | YEAR) + (1 | X:Y),
                          family ="binomial")
summary(model_evl_phetel)

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

# EVL TARGET
model_evltar_phetel <- glmer(data = data %>%
                               filter(DRUH == "Phengaris teleius"), 
                             as.factor(POSITIVE) ~ as.factor(EVL_target) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_evltar_phetel)

ggplot(data = data_evltar_sum %>%
         filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(EVL_target), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside Natura 2000\nestablished for P. teleius", 
                              "within Natura 2000\nestablished for P. teleius")) +
  xlab("") +
  ylab("number of findings\n") +
  theme_classic()

# EVL COMBINATION
model_evlcom_phetel <- glmer(data = data %>%
                               filter(DRUH == "Phengaris teleius"), 
                             as.factor(POSITIVE) ~ as.factor(EVL) + as.factor(EVL_target) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_evlcom_phetel)

summary(model_evl_phetel)$AIC
summary(model_evltar_phetel)$AIC
summary(model_evlcom_phetel)$AIC

ggplot(data = data_evlcomb_sum %>%
         filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(EVL_comb), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("outside Natura 2000", 
                              "within Natura 2000 not\nestablished for P. teleius",
                              "within Natura 2000\nestablished for P. teleius")) +
  xlab("") +
  ylab("number of sites\n") +
  theme_classic()

model_procom_phetel <- glmer(data = data %>%
                               filter(DRUH == "Phengaris teleius"), 
                             as.factor(POSITIVE) ~ as.factor(EVL) + as.factor(EVL_target) + as.factor(MZCHU)+ (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_procom_phetel)

# MZCHU
model_mzchu_phetel <- glmer(data = data %>%
                         filter(DRUH == "Phengaris teleius"), 
                       as.factor(POSITIVE) ~ as.factor(MZCHU) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_mzchu_phetel)
ggplot(data = data_mzchu_sum %>%
         filter(DRUH == "Phengaris teleius"), 
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


# FSB
model_fsb_phetel <- glmer(data = data %>%
                            filter(DRUH == "Phengaris teleius") %>%
                            filter(FSB %in% c("T", "X", "moz.")), 
                             as.factor(POSITIVE) ~ as.factor(FSB) + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_fsb_phetel)

# HETEROENITY
model_hetinn_phetel <- glmer(data = data %>%
                         filter(DRUH == "Phengaris teleius"), 
                       as.factor(POSITIVE) ~ HET_INN + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_hetinn_phetel)

model_hetout_phetel <- glmer(data = data %>%
                               filter(DRUH == "Phengaris teleius"), 
                             as.factor(POSITIVE) ~ HET_OUT + (1 | YEAR) + (1 | X:Y),
                             family ="binomial")
summary(model_hetout_phetel)


data_sum %>%
  filter(DRUH == "Phengaris teleius") %>%
  pull(COUNT) %>%
  sum()

# selektovat 

# zrandomizovat polohu/lokalitu

# všechny one-way interakce
# heterogenita oběma způsoby, testovat postupně
# Modely pro každý druh zvlášť
# Rozloha * kytka
# heterogenita * rozloha * kytka
# přítomnost 2 druhů 
# způsob seče * načasování seče * heterogenita * rozloha

# procento varience z year random effects

#----------------------------------------------------------#
## Both species -----
#----------------------------------------------------------#
modelboth <- glmer(data = data, 
                          as.factor(POSITIVE) ~ as.factor(SPEC_NUM)*as.factor(DRUH) + (1 | YEAR) + (1 | (X:Y)),
                          family ="binomial")
summary(modelboth)

ggplot(data = data_spe_sum %>%
         filter(DRUH == "Phengaris nausithous"), 
       aes(x = as.factor(SPEC_NUM), 
           y = COUNT,
           fill = as.factor(POSITIVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_manual(labels = c("negative", "positive"),
                    name = "site occupancy",
                    values = c("grey", "#595959")) +
  scale_x_discrete(labels = c("without P. teleius", "with P. teleius")) +
  xlab("") +
  ylab("number of sites\n") +
  theme_classic()

ggplot(data = data_spe_sum %>%
         filter(DRUH == "Phengaris teleius"), 
       aes(x = as.factor(SPEC_NUM), 
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

model_species <- glmer(data = data,
                       as.factor(POSITIVE) ~ as.factor(DRUH) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_species)

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

# ABUNDANCE
wilcox.test(data %>%
               filter(DRUH == "Phengaris nausithous") %>%
               pull(POCET),
             data %>%
               filter(DRUH == "Phengaris teleius") %>%
               pull(POCET))
kruskal.test(data %>%
         pull(POCET),
       data %>%
         pull(DRUH))

wilcox.test(data %>%
              filter(DRUH == "Phengaris nausithous") %>%
              pull(POSITIVE),
            data %>%
              filter(DRUH == "Phengaris teleius") %>%
              pull(POSITIVE))
kruskal.test(phenpole_analysis %>%
               pull(POSITIVE),
             phenpole_analysis %>%
               pull(POSITIVE))

ggplot(data = data, 
       aes(x = as.factor(DRUH), 
           y = log(POCET))) +
  #geom_violin() +
  geom_boxplot() +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  #scale_x_discrete(labels = c("vacant sites", "occupied sites")) +
  scale_fill_discrete(labels = c("single plants", "abundant", "dominant"),
                      name=NULL) +
  xlab("") +
  ylab("log10(site area)\n") +
  theme_classic() +
  theme(text = element_text(size = 30))

phenpole %>%
  st_drop_geometry() %>%
  group_by(ID_LOKAL) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(POLE) %>%
  summarise(sitenum = n()) %>%
  pull(sitenum) %>%
  min()
phenpole %>%
  st_drop_geometry() %>%
  group_by(ID_LOKAL) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(YEAR, AUTOR) %>%
  summarise(pocet = length(unique(POLE))) %>%
  pull(pocet) %>%
  max()
phenpole %>%
  st_drop_geometry() %>%
  group_by(ID_LOKAL) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(YEAR, AUTOR) %>%
  summarise(pocet = length(unique(POLE))) %>%
  arrange(-pocet)


#----------------------------------------------------------#
## Threats and pressures -----
#----------------------------------------------------------#
data_tap_all <- data %>%
  dplyr::filter(SUM_THREATS > 0) %>%
  dplyr::mutate(DRUH = dplyr::case_when(DRUH == "Phengaris nausithous" ~ "nausithous",
                                        DRUH == "Phengaris teleius" ~ "teleius")) %>%
  dplyr::select(DRUH, POSITIVE,
                PLANT_QUANT, TTP, ZARUST, PRIKOP, HET_INN, NATURAL, EVL, EVL_target, MZCHU,
                LandUseChange, Abandonment, HarmfulMow, HarmfulGrazing,
                GrazingByeffects, FertilizerUse, Afforestation, Invasives,
                NativeDominants, AbioticNaturalProcesses,
                Encroachment, BiomassAccumulation, Eutrophization, None) %>%
  dplyr::group_by(DRUH, POSITIVE) %>%
  dplyr::summarise(ID = paste0(unique(DRUH), unique(POSITIVE)),
                   PLANT_QUANT = mean(PLANT_QUANT), 
                   TTP = mean(TTP), 
                   ZARUST = mean(ZARUST), 
                   PRIKOP = mean(PRIKOP), 
                   HET_INN = mean(HET_INN),
                   NATURAL = mean(NATURAL),
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
                NATURAL,
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



# ZSUTIS SEMIKVANTATIVNĚ
data %>%
  filter(is.na(POCET) == FALSE) %>%
  pull(POCET) %>%
  hist

# POSTER ----


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
  theme_classic() +
  theme(text = element_text(size = 26))





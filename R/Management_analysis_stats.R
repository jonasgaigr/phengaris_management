#----------------------------------------------------------#
# Load data -----
#----------------------------------------------------------#

data <- 
  readr::read_csv(
    "Data/Processed/data_clean.csv"
    )

#----------------------------------------------------------#
# Models -----
#----------------------------------------------------------#
#----------------------------------------------------------#
## Phengaris nausithous -----
#----------------------------------------------------------#
# NULL
model_null_phenau <- 
  lme4::glmer(
    data = data %>%
      filter(
        DRUH == "Phengaris nausithous"
        ), 
    as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
    family = "binomial"
    )

summary(model_null_phenau)

model_null_phetel <- lme4::glmer(
  data = data %>%
    filter(DRUH == "Phengaris teleius"), 
    as.factor(POSITIVE) ~ 1 + (1 | YEAR) + (1 | X:Y),
    family = "binomial"
  )
summary(model_null_phetel)

# TEST THE YEAR EFFECT
model_nau_yf <- glm(data = data %>%
                 filter(DRUH == "Phengaris nausithous"), 
               as.factor(POSITIVE) ~ as.factor(YEAR),
               family = "binomial")
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
modelboth <- glmer(
  data = data, 
  as.factor(POSITIVE) ~ as.factor(SPEC_NUM) * as.factor(DRUH) + (1 | YEAR) + (1 | (X:Y)),
  family ="binomial"
  )
summary(modelboth)


model_species <- glmer(data = data,
                       as.factor(POSITIVE) ~ as.factor(DRUH) + (1 | YEAR) + (1 | X:Y),
                       family ="binomial")
summary(model_species)


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


# ZSUTIS SEMIKVANTATIVNĚ
data %>%
  filter(is.na(POCET) == FALSE) %>%
  pull(POCET) %>%
  hist

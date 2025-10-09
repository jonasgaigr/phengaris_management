
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



# ---- 0) Prepare response matrix (Y) and metadata (meta) ----
# assume `data_tap_all` and `data_tap` are already produced by your snippet.
# If not, run your summarise pipeline first, then continue.

# metadata (keeps DRUH + POSITIVE for coloring / grouping)
meta <- data_tap_all %>%
  tibble::column_to_rownames("ID") %>%
  dplyr::select(DRUH, POSITIVE) %>%
  mutate(
    DRUH = as.factor(DRUH),
    POSITIVE = as.factor(POSITIVE)
  )

# response matrix (numeric variables used in ordination)
resp <- data_tap %>% as.data.frame()

# --- Final robust check for zero-variance / NA-only / non-numeric columns ---
zero_vars <- sapply(resp, function(x) {
  if (!is.numeric(x)) return(TRUE)                     # drop non-numeric
  if (all(is.na(x))) return(TRUE)                      # drop all-NA columns
  s <- suppressWarnings(sd(x, na.rm = TRUE))           # safe SD computation
  if (is.na(s) || s == 0) return(TRUE)                 # drop NA or zero SD
  return(FALSE)
})

if (any(zero_vars)) {
  warning("Removing problematic columns: ",
          paste(names(zero_vars)[zero_vars], collapse = ", "))
  resp <- resp[, !zero_vars, drop = FALSE]
}

# Optional sanity check:
stopifnot(all(sapply(resp, is.numeric)))

# make sure rownames match metadata
if(!all(rownames(resp) %in% rownames(meta))){
  stop("Row names of response and metadata don't match. Ensure data_tap and data_tap_all IDs match.")
}
meta <- meta[rownames(resp), , drop = FALSE]  # reorder meta to match resp rows


# -----------------------
# A) PCA (prcomp) — ggplot biplot
# -----------------------
pca <- prcomp(resp, center = TRUE, scale. = TRUE)

# % variance explained for axis labels
pca_var <- summary(pca)$importance[2, 1:2] * 100

# sample scores (PC1 & PC2)
scores_df <- as.data.frame(pca$x[, 1:2]) %>%
  rownames_to_column("ID") %>%
  rename(PC1 = 2, PC2 = 3)  # prcomp names are "PC1","PC2" but rename safely

# attach meta info
scores_df <- scores_df %>% left_join(rownames_to_column(as.data.frame(meta), "ID"), by = "ID")

# variable loadings (rotation)
loadings_df <- as.data.frame(pca$rotation[, 1:2]) %>%
  rownames_to_column("Variable") %>%
  rename(PC1 = 2, PC2 = 3)

# scale loadings to match sample score scale (classic biplot scaling)
# factor chosen to make arrows readable — tweak multiplier (0.7 / 0.9) if needed
arrow_factor <- min(
  (max(abs(scores_df$PC1)) / max(abs(loadings_df$PC1))),
  (max(abs(scores_df$PC2)) / max(abs(loadings_df$PC2)))
) * 0.9
loadings_df <- loadings_df %>% mutate(PC1 = PC1 * arrow_factor, PC2 = PC2 * arrow_factor)

# PCA ggplot biplot
pca_gg <- ggplot(scores_df, aes(x = PC1, y = PC2, color = DRUH, shape = POSITIVE)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  geom_point(size = 3) +
  stat_ellipse(aes(fill = DRUH), geom = "polygon", alpha = 0.12, level = 0.95, show.legend = FALSE) +
  # variable arrows
  geom_segment(
    data = loadings_df, inherit.aes = FALSE,
    aes(x = 0, y = 0, xend = PC1, yend = PC2),
    arrow = arrow(length = unit(0.25, "cm")), lineend = "round"
  ) +
  geom_text_repel(
    data = loadings_df, inherit.aes = FALSE,
    aes(x = PC1, y = PC2, label = Variable),
    size = 3.2, max.overlaps = 20
  ) +
  labs(
    title = "PCA biplot – threats & pressures",
    x = paste0("PC1 (", round(pca_var[1], 1), "%)"),
    y = paste0("PC2 (", round(pca_var[2], 1), "%)")
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

# print/inspect
print(pca_gg)
# ggsave("PCA_threats_biplot.png", pca_gg, width = 8, height = 6, dpi = 300)


# -----------------------
# B) RDA (constrained ordination) — ggplot triplot-style
# -----------------------
# standardize species (response) matrix prior to RDA so variables are comparable
resp_std <- decostand(resp, method = "standardize")   # mean=0, sd=1

# make sure meta is in the right format (factors are OK)
meta_rda <- meta %>% mutate(DRUH = as.factor(DRUH), POSITIVE = as.factor(POSITIVE))

# RDA model — constraining by species identity (DRUH) and POSITIVE (presence/absence grouping)
# you can add other numeric explanatory variables here if desired, e.g. EVL, MZCHU
rda_mod <- rda(resp_std ~ DRUH + POSITIVE, data = meta_rda)

# significance tests
rda_overall <- anova(rda_mod, permutations = 999)                 # overall
rda_by_term <- anova(rda_mod, by = "terms", permutations = 999)   # by term

# VIF for constrained variables (checks collinearity)
vif_vals <- tryCatch(vif.cca(rda_mod), error = function(e) NA)

# extract scores for plotting
site_scores_rda <- as.data.frame(scores(rda_mod, display = "sites", choices = 1:2)) %>%
  rownames_to_column("ID") %>%
  rename(RDA1 = 2, RDA2 = 3) %>%
  left_join(rownames_to_column(as.data.frame(meta_rda), "ID"), by = "ID")

species_scores_rda <- as.data.frame(scores(rda_mod, display = "species", choices = 1:2)) %>%
  rownames_to_column("Variable") %>%
  rename(RDA1 = 2, RDA2 = 3)

# biplot scores for constraining variables (env / explanatory)
bp_scores_rda <- as.data.frame(scores(rda_mod, display = "bp", choices = 1:2)) %>%
  rownames_to_column("ConstrVar") %>%
  rename(RDA1 = 2, RDA2 = 3)

# scale species arrows to site cloud
arrow_factor_rda <- min(
  (max(abs(site_scores_rda$RDA1)) / max(abs(species_scores_rda$RDA1))),
  (max(abs(site_scores_rda$RDA2)) / max(abs(species_scores_rda$RDA2)))
) * 0.8
species_scores_rda <- species_scores_rda %>% mutate(RDA1 = RDA1 * arrow_factor_rda, RDA2 = RDA2 * arrow_factor_rda)

# scale constraining variable arrows (usually smaller — tweak if needed)
arrow_factor_bp <- min(
  (max(abs(site_scores_rda$RDA1)) / max(abs(bp_scores_rda$RDA1))),
  (max(abs(site_scores_rda$RDA2)) / max(abs(bp_scores_rda$RDA2)))
) * 0.9
bp_scores_rda <- bp_scores_rda %>% mutate(RDA1 = RDA1 * arrow_factor_bp, RDA2 = RDA2 * arrow_factor_bp)

# build RDA ggplot
rda_gg <- ggplot(site_scores_rda, aes(x = RDA1, y = RDA2, color = DRUH, shape = POSITIVE)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey80") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey80") +
  geom_point(size = 3) +
  stat_ellipse(aes(fill = DRUH), geom = "polygon", alpha = 0.12, level = 0.95, show.legend = FALSE) +
  # species arrows (response variables / "threats")
  geom_segment(
    data = species_scores_rda, inherit.aes = FALSE,
    aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
    arrow = arrow(length = unit(0.22, "cm")), color = "grey45", lineend = "round"
  ) +
  geom_text_repel(
    data = species_scores_rda, inherit.aes = FALSE,
    aes(x = RDA1, y = RDA2, label = Variable),
    size = 3.0, color = "grey20", max.overlaps = 20
  ) +
  # constraining variables (biplot) arrows — e.g. DRUH/POSITIVE encoded as dummies
  geom_segment(
    data = bp_scores_rda, inherit.aes = FALSE,
    aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
    arrow = arrow(length = unit(0.22, "cm")), color = "black", lineend = "round"
  ) +
  geom_text_repel(
    data = bp_scores_rda, inherit.aes = FALSE,
    aes(x = RDA1, y = RDA2, label = ConstrVar),
    size = 3.0, color = "black", fontface = "bold", max.overlaps = 20
  ) +
  labs(
    title = "RDA triplot – constrained by DRUH + POSITIVE",
    subtitle = paste0("ANOVA (overall): p = ", signif(rda_overall$`Pr(>F)`[1], 3)),
    x = "RDA1", y = "RDA2"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

# print diagnostics and plot
print(rda_overall)
print(rda_by_term)
print(vif_vals)
print(rda_gg)
# ggsave("RDA_triplot.png", rda_gg, width = 8, height = 6, dpi = 300)

# ---- HULL approach ----

# Define color palette (edit as needed)
tap_cols <- c("nausithousTRUE" = "#E69F00",
              "nausithousFALSE" = "#56B4E9",
              "teleiusTRUE" = "#009E73",
              "teleiusFALSE" = "#D55E00")

# ------------------------------
# Prepare response matrix
# ------------------------------
resp <- data_tap

# Robust cleanup: remove all-NA, zero-variance, or non-numeric columns
zero_vars <- sapply(resp, function(x) {
  if (!is.numeric(x)) return(TRUE)
  if (all(is.na(x))) return(TRUE)
  s <- suppressWarnings(sd(x, na.rm = TRUE))
  if (is.na(s) || s == 0) return(TRUE)
  FALSE
})

if (any(zero_vars)) {
  warning("Removing problematic columns: ",
          paste(names(zero_vars)[zero_vars], collapse = ", "))
  resp <- resp[, !zero_vars, drop = FALSE]
}

stopifnot(all(sapply(resp, is.numeric)))

# --- PCA (unconstrained ordination) ---
pca_tap <- rda(resp, scale = TRUE)

# Fit variables as vectors
ef_pca_tap <- envfit(pca_tap, resp, permutations = 999)

# Screeplot
screeplot(pca_tap, bstick = TRUE, main = "PCA (Threats & Pressures)")

# Scores for sites (groups = DRUH+POSITIVE)
pca_scores <- as.data.frame(scores(pca_tap, display = "sites", scaling = 2))
pca_scores$ID <- rownames(resp)
pca_scores <- pca_scores %>%
  left_join(data_tap_all %>% dplyr::select(ID, DRUH, POSITIVE), by = "ID")

# Arrows (biplot scores for variables)
pca_arrows <- as.data.frame(scores(ef_pca_tap, display = "vectors", scaling = 2))

# Compute centroids by DRUH + POSITIVE
pca_centroids <- aggregate(cbind(PC1 = pca_scores$PC1, PC2 = pca_scores$PC2),
                           by = list(group = pca_scores$ID),
                           FUN = mean)

# PCA plot
pca_plot <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = interaction(DRUH, POSITIVE))) +
  geom_point(size = 3) +
  ggpubr::stat_chull(aes(group = interaction(DRUH, POSITIVE)), alpha = 0.2, geom = "polygon") +
  geom_segment(data = pca_arrows,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")),
               inherit.aes = FALSE,
               color = "red") +
  geom_text_repel(data = pca_arrows,
                  aes(x = PC1, y = PC2, label = rownames(pca_arrows)),
                  color = "red", size = 4) +
  scale_color_manual(values = tap_cols, name = "Group") +
  theme_minimal() +
  ggtitle("PCA (Threats & Pressures)\nby species × response") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Outputs/Plots/pca_tap.png", plot = pca_plot, height = 5, width = 6)
ggsave("Outputs/Plots/pca_tap.pdf", plot = pca_plot, width = 5, height = 5, device = cairo_pdf)


# ==============================
# RDA for Threats & Pressures
# ==============================

rda_tap <- rda(
  resp ~ DRUH + POSITIVE,
  data = data_tap_all,
  scale = TRUE
)

# Significance tests
anova(rda_tap)               # overall test
anova(rda_tap, by = "axis")  # by axis
anova(rda_tap, by = "terms") # by terms

# Site scores
rda_scores <- as.data.frame(scores(rda_tap, display = "sites", scaling = 2))
rda_scores$ID <- rownames(resp)
rda_scores <- rda_scores %>%
  left_join(data_tap_all %>% dplyr::select(ID, DRUH, POSITIVE), by = "ID")

# Arrows (biplot scores for variables)
rda_arrows <- as.data.frame(scores(rda_tap, display = "bp", scaling = 2))
rda_arrows$varname <- rownames(rda_arrows)
colnames(rda_arrows)[1:2] <- c("RDA1", "RDA2")

# Compute centroids by group
rda_centroids <- aggregate(cbind(RDA1 = rda_scores$RDA1, RDA2 = rda_scores$RDA2),
                           by = list(group = interaction(rda_scores$DRUH, rda_scores$POSITIVE)),
                           FUN = mean)

# RDA plot
rda_plot <- ggplot(rda_scores, aes(x = RDA1, y = RDA2, color = interaction(DRUH, POSITIVE))) +
  geom_point(size = 3) +
  stat_chull(aes(group = interaction(DRUH, POSITIVE)), alpha = 0.2, geom = "polygon") +
  geom_text_repel(data = rda_centroids,
                  aes(x = RDA1, y = RDA2, label = group),
                  size = 5, fontface = "bold", color = "black") +
  scale_color_manual(values = tap_cols, name = "Group") +
  theme_minimal() +
  ggtitle("RDA (Threats & Pressures)\n~ species + response") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "RDA1", y = "RDA2")

ggsave("Outputs/Plots/rda_tap.png", plot = rda_plot, height = 5, width = 6)
ggsave("Outputs/Plots/rda_tap.pdf", plot = rda_plot, width = 5, height = 5, device = cairo_pdf)



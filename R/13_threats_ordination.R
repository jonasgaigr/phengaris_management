#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#     Step 13 - Ordination of threats and pressures
#
#
#----------------------------------------------------------#
#
# Describes how the recorded threats and pressures separate the four groups
# formed by species and occupancy: occupied and vacant sites of each species.
#
# Two ordinations are run, as in the original script:
#
#   A. an unconstrained PCA, shown as a biplot with variable loadings;
#   B. an RDA constrained by species and occupancy, tested by permutation,
#      shown as a triplot and as a convex-hull plot.
#
# Reads:  Data/Processed/data_analysis.csv   (step 06)
# Writes: Outputs/Figures/13_*.png, Outputs/Tables/13_*.csv
#         Outputs/data_pca.csv, Outputs/data_data_export.csv
#         Outputs/Reports/13_threats_and_pressures.md
#
#----------------------------------------------------------#

message("Step 13: ordination of threats and pressures")

report_start(
  "13",
  "Threats and pressures",
  paste(
    "Ordination of the threats and pressures recorded at monitoring sites,",
    "summarised to one point per species and occupancy class. An unconstrained",
    "PCA describes the main gradients; an RDA constrained by species and",
    "occupancy tests whether those two factors explain them."
  )
)

data <- read_data_analysis()

#----------------------------------------------------------#
# Build the ordination input -----
#----------------------------------------------------------#

# One row per species x occupancy class. The threat variables enter twice: as a
# count of affected records (_sum) and as the share of records affected.
data_tap_all <- data %>%
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

# The variables that actually enter the ordination.
data_tap <- data_tap_all %>%
  tibble::column_to_rownames("ID") %>%
  dplyr::select(
    PLANT_QUANT,
    TTP,
    ZARUST,
    PRIKOP,
    HET_INN,
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

#--------------------------------------------------#
## Data exports -----
#--------------------------------------------------#

# Kept at their original paths and in the semicolon-separated Czech CSV
# dialect, because they are data exports rather than result tables.
utils::write.csv2(
  data_tap_all,
  file.path(PATHS$outputs, "data_pca.csv"),
  fileEncoding = "Windows-1250"
)

utils::write.csv2(
  data,
  file.path(PATHS$outputs, "data_data_export.csv"),
  fileEncoding = "Windows-1250",
  row.names = FALSE
)

#----------------------------------------------------------#
# Prepare the response matrix -----
#----------------------------------------------------------#

# Metadata: species and occupancy, used for colouring and for constraining.
meta <- data_tap_all %>%
  tibble::column_to_rownames("ID") %>%
  dplyr::select(DRUH, POSITIVE) %>%
  dplyr::mutate(
    DRUH = as.factor(DRUH),
    POSITIVE = as.factor(POSITIVE)
  )

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

resp <- drop_unusable_columns(as.data.frame(data_tap))

stopifnot(all(sapply(resp, is.numeric)))

if (!all(rownames(resp) %in% rownames(meta))) {
  stop("Row names of response and metadata do not match.")
}
meta <- meta[rownames(resp), , drop = FALSE]

dropped_columns <- setdiff(names(data_tap), names(resp))
if (length(dropped_columns)) {
  report_warning(
    "These threat variables were constant across the four groups and could not",
    "enter the ordination:", paste(dropped_columns, collapse = ", "), "."
  )
}

#----------------------------------------------------------#
# A. Unconstrained PCA -----
#----------------------------------------------------------#

report_section("A. Unconstrained PCA")

pca <- stats::prcomp(resp, center = TRUE, scale. = TRUE)

pca_var <- summary(pca)$importance[2, 1:2] * 100

report_table(
  as.data.frame(t(summary(pca)$importance)) %>%
    tibble::rownames_to_column("axis"),
  "PCA variance explained by axis",
  "13_pca_variance",
  max_rows = 30
)

# Sample scores on the first two axes, with species and occupancy attached.
scores_df <- as.data.frame(pca$x[, 1:2]) %>%
  tibble::rownames_to_column("ID") %>%
  dplyr::rename(PC1 = 2, PC2 = 3) %>%
  dplyr::left_join(
    tibble::rownames_to_column(as.data.frame(meta), "ID"),
    by = "ID"
  )

# Variable loadings.
loadings_df <- as.data.frame(pca$rotation[, 1:2]) %>%
  tibble::rownames_to_column("Variable") %>%
  dplyr::rename(PC1 = 2, PC2 = 3)

report_table(
  loadings_df,
  "PCA loadings on the first two axes",
  "13_pca_loadings",
  max_rows = 30
)

# Scale the loading arrows to the spread of the sample scores, so that both fit
# on one plot. This is presentation only and does not affect the ordination.
arrow_factor <- min(
  (max(abs(scores_df$PC1)) / max(abs(loadings_df$PC1))),
  (max(abs(scores_df$PC2)) / max(abs(loadings_df$PC2)))
) * 0.9
loadings_df <- loadings_df %>%
  dplyr::mutate(PC1 = PC1 * arrow_factor, PC2 = PC2 * arrow_factor)

pca_gg <- ggplot2::ggplot(
  scores_df,
  ggplot2::aes(x = PC1, y = PC2, color = DRUH, shape = POSITIVE)
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  ggplot2::geom_point(size = 3) +
  ggplot2::stat_ellipse(
    ggplot2::aes(fill = DRUH),
    geom = "polygon", alpha = 0.12, level = 0.95, show.legend = FALSE
  ) +
  ggplot2::geom_segment(
    data = loadings_df, inherit.aes = FALSE,
    ggplot2::aes(x = 0, y = 0, xend = PC1, yend = PC2),
    arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "cm")), lineend = "round"
  ) +
  ggrepel::geom_text_repel(
    data = loadings_df, inherit.aes = FALSE,
    ggplot2::aes(x = PC1, y = PC2, label = Variable),
    size = 3.2, max.overlaps = 20
  ) +
  ggplot2::labs(
    title = "PCA biplot - threats & pressures",
    x = paste0("PC1 (", round(pca_var[1], 1), "%)"),
    y = paste0("PC2 (", round(pca_var[2], 1), "%)")
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(legend.position = "right")

report_figure(
  pca_gg,
  "PCA biplot of threats and pressures",
  "13_pca_threats_biplot"
)

#----------------------------------------------------------#
# B. RDA constrained by species and occupancy -----
#----------------------------------------------------------#

report_section("B. Constrained RDA")

# Standardise before the RDA so that all variables are comparable.
resp_std <- vegan::decostand(resp, method = "standardize")

meta_rda <- meta %>%
  dplyr::mutate(DRUH = as.factor(DRUH), POSITIVE = as.factor(POSITIVE))

rda_mod <- vegan::rda(resp_std ~ DRUH + POSITIVE, data = meta_rda)

#--------------------------------------------------#
## Permutation tests -----
#--------------------------------------------------#

rda_overall <- stats::anova(rda_mod, permutations = 999)
rda_by_term <- stats::anova(rda_mod, by = "terms", permutations = 999)

#' Turn a vegan anova result into a reportable data frame.
anova_table <- function(x) {
  as.data.frame(x) %>%
    tibble::rownames_to_column("term")
}

report_table(
  anova_table(rda_overall),
  "RDA permutation test, overall",
  "13_rda_anova_overall"
)

report_table(
  anova_table(rda_by_term),
  "RDA permutation test, by term",
  "13_rda_anova_by_term"
)

vif_vals <- tryCatch(vegan::vif.cca(rda_mod), error = function(e) NA)

if (!all(is.na(vif_vals))) {
  report_table(
    data.frame(
      term = names(vif_vals),
      VIF  = as.numeric(vif_vals),
      stringsAsFactors = FALSE
    ),
    "Variance inflation factors of the constraining variables",
    "13_rda_vif"
  )
}

#--------------------------------------------------#
## Scores and triplot -----
#--------------------------------------------------#

site_scores_rda <- as.data.frame(
  vegan::scores(rda_mod, display = "sites", choices = 1:2)
) %>%
  tibble::rownames_to_column("ID") %>%
  dplyr::rename(RDA1 = 2, RDA2 = 3) %>%
  dplyr::left_join(
    tibble::rownames_to_column(as.data.frame(meta_rda), "ID"),
    by = "ID"
  )

species_scores_rda <- as.data.frame(
  vegan::scores(rda_mod, display = "species", choices = 1:2)
) %>%
  tibble::rownames_to_column("Variable") %>%
  dplyr::rename(RDA1 = 2, RDA2 = 3)

bp_scores_rda <- as.data.frame(
  vegan::scores(rda_mod, display = "bp", choices = 1:2)
) %>%
  tibble::rownames_to_column("ConstrVar") %>%
  dplyr::rename(RDA1 = 2, RDA2 = 3)

report_table(
  species_scores_rda,
  "RDA variable scores on the first two axes",
  "13_rda_variable_scores",
  max_rows = 30
)

# Arrow scaling, presentation only.
arrow_factor_rda <- min(
  (max(abs(site_scores_rda$RDA1)) / max(abs(species_scores_rda$RDA1))),
  (max(abs(site_scores_rda$RDA2)) / max(abs(species_scores_rda$RDA2)))
) * 0.8
species_scores_rda <- species_scores_rda %>%
  dplyr::mutate(RDA1 = RDA1 * arrow_factor_rda, RDA2 = RDA2 * arrow_factor_rda)

arrow_factor_bp <- min(
  (max(abs(site_scores_rda$RDA1)) / max(abs(bp_scores_rda$RDA1))),
  (max(abs(site_scores_rda$RDA2)) / max(abs(bp_scores_rda$RDA2)))
) * 0.9
bp_scores_rda <- bp_scores_rda %>%
  dplyr::mutate(RDA1 = RDA1 * arrow_factor_bp, RDA2 = RDA2 * arrow_factor_bp)

rda_gg <- ggplot2::ggplot(
  site_scores_rda,
  ggplot2::aes(x = RDA1, y = RDA2, color = DRUH, shape = POSITIVE)
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey80") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey80") +
  ggplot2::geom_point(size = 3) +
  ggplot2::stat_ellipse(
    ggplot2::aes(fill = DRUH),
    geom = "polygon", alpha = 0.12, level = 0.95, show.legend = FALSE
  ) +
  ggplot2::geom_segment(
    data = species_scores_rda, inherit.aes = FALSE,
    ggplot2::aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
    arrow = ggplot2::arrow(length = ggplot2::unit(0.22, "cm")),
    color = "grey45", lineend = "round"
  ) +
  ggrepel::geom_text_repel(
    data = species_scores_rda, inherit.aes = FALSE,
    ggplot2::aes(x = RDA1, y = RDA2, label = Variable),
    size = 3.0, color = "grey20", max.overlaps = 20
  ) +
  ggplot2::geom_segment(
    data = bp_scores_rda, inherit.aes = FALSE,
    ggplot2::aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
    arrow = ggplot2::arrow(length = ggplot2::unit(0.22, "cm")),
    color = "black", lineend = "round"
  ) +
  ggrepel::geom_text_repel(
    data = bp_scores_rda, inherit.aes = FALSE,
    ggplot2::aes(x = RDA1, y = RDA2, label = ConstrVar),
    size = 3.0, color = "black", fontface = "bold", max.overlaps = 20
  ) +
  ggplot2::labs(
    title = "RDA triplot - constrained by DRUH + POSITIVE",
    subtitle = paste0("ANOVA (overall): p = ", signif(rda_overall$`Pr(>F)`[1], 3)),
    x = "RDA1", y = "RDA2"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(legend.position = "right")

report_figure(
  rda_gg,
  "RDA triplot constrained by species and occupancy",
  "13_rda_threats_triplot"
)

#----------------------------------------------------------#
# C. Convex hull plots -----
#----------------------------------------------------------#

report_section("C. Convex hull plots")

# Colour per species x occupancy group.
tap_cols <- c(
  "nausithousTRUE"  = "#E69F00",
  "nausithousFALSE" = "#56B4E9",
  "teleiusTRUE"     = "#009E73",
  "teleiusFALSE"    = "#D55E00"
)

#--------------------------------------------------#
## PCA with fitted variable vectors -----
#--------------------------------------------------#

pca_tap <- vegan::rda(resp, scale = TRUE)

ef_pca_tap <- vegan::envfit(pca_tap, resp, permutations = 999)

# screeplot() is a stats generic; vegan only registers the method for rda
# objects, so it has to be reached through stats, not through vegan::.
report_base_figure(
  stats::screeplot(pca_tap, bstick = TRUE, main = "PCA (Threats & Pressures)"),
  "PCA screeplot with the broken-stick expectation",
  "13_pca_screeplot"
)

pca_scores <- as.data.frame(
  vegan::scores(pca_tap, display = "sites", scaling = 2)
)
pca_scores$ID <- rownames(resp)
pca_scores <- pca_scores %>%
  dplyr::left_join(
    data_tap_all %>% dplyr::select(ID, DRUH, POSITIVE),
    by = "ID"
  )

pca_arrows <- as.data.frame(
  vegan::scores(ef_pca_tap, display = "vectors", scaling = 2)
)

pca_plot <- ggplot2::ggplot(
  pca_scores,
  ggplot2::aes(x = PC1, y = PC2, color = interaction(DRUH, POSITIVE))
) +
  ggplot2::geom_point(size = 3) +
  ggpubr::stat_chull(
    ggplot2::aes(group = interaction(DRUH, POSITIVE)),
    alpha = 0.2, geom = "polygon"
  ) +
  ggplot2::geom_segment(
    data = pca_arrows,
    ggplot2::aes(x = 0, y = 0, xend = PC1, yend = PC2),
    arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
    inherit.aes = FALSE,
    color = "red"
  ) +
  ggrepel::geom_text_repel(
    data = pca_arrows,
    ggplot2::aes(x = PC1, y = PC2, label = rownames(pca_arrows)),
    color = "red", size = 4
  ) +
  ggplot2::scale_color_manual(values = tap_cols, name = "Group") +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle("PCA (Threats & Pressures)\nby species x response") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

report_figure(
  pca_plot,
  "PCA convex hulls by species and occupancy",
  "13_pca_tap",
  width = 6, height = 5
)
ggplot2::ggsave(
  file.path(PATHS$figures, "13_pca_tap.pdf"),
  plot = pca_plot, width = 5, height = 5, device = grDevices::cairo_pdf
)

#--------------------------------------------------#
## RDA convex hulls -----
#--------------------------------------------------#

rda_tap <- vegan::rda(
  resp ~ DRUH + POSITIVE,
  data = data_tap_all,
  scale = TRUE
)

report_table(
  anova_table(stats::anova(rda_tap)),
  "RDA (scaled) permutation test, overall",
  "13_rda_tap_anova_overall"
)
report_table(
  anova_table(stats::anova(rda_tap, by = "axis")),
  "RDA (scaled) permutation test, by axis",
  "13_rda_tap_anova_by_axis"
)
report_table(
  anova_table(stats::anova(rda_tap, by = "terms")),
  "RDA (scaled) permutation test, by term",
  "13_rda_tap_anova_by_term"
)

rda_scores <- as.data.frame(
  vegan::scores(rda_tap, display = "sites", scaling = 2)
)
rda_scores$ID <- rownames(resp)
rda_scores <- rda_scores %>%
  dplyr::left_join(
    data_tap_all %>% dplyr::select(ID, DRUH, POSITIVE),
    by = "ID"
  )

rda_centroids <- stats::aggregate(
  cbind(RDA1 = rda_scores$RDA1, RDA2 = rda_scores$RDA2),
  by = list(group = interaction(rda_scores$DRUH, rda_scores$POSITIVE)),
  FUN = mean
)

rda_plot <- ggplot2::ggplot(
  rda_scores,
  ggplot2::aes(x = RDA1, y = RDA2, color = interaction(DRUH, POSITIVE))
) +
  ggplot2::geom_point(size = 3) +
  ggpubr::stat_chull(
    ggplot2::aes(group = interaction(DRUH, POSITIVE)),
    alpha = 0.2, geom = "polygon"
  ) +
  ggrepel::geom_text_repel(
    data = rda_centroids,
    ggplot2::aes(x = RDA1, y = RDA2, label = group),
    size = 5, fontface = "bold", color = "black"
  ) +
  ggplot2::scale_color_manual(values = tap_cols, name = "Group") +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle("RDA (Threats & Pressures)\n~ species + response") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
  ggplot2::labs(x = "RDA1", y = "RDA2")

report_figure(
  rda_plot,
  "RDA convex hulls by species and occupancy",
  "13_rda_tap",
  width = 6, height = 5
)
ggplot2::ggsave(
  file.path(PATHS$figures, "13_rda_tap.pdf"),
  plot = rda_plot, width = 5, height = 5, device = grDevices::cairo_pdf
)

#----------------------------------------------------------#
# Caveat -----
#----------------------------------------------------------#

report_warning(
  "Both ordinations are run on", nrow(resp), "rows: one per species and",
  "occupancy class. With that many points the ellipses, convex hulls and",
  "permutation tests carry very little information, and the RDA is saturated",
  "by its two constraining factors. The ordinations describe the structure of",
  "the group means; they do not test differences between individual sites."
)

report_finish()

message("Step 13 done")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

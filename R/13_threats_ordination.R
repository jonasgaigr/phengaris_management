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
# Reads:  Data/Processed/<spec>/data_analysis.csv   (step 06)
# Writes: Outputs/<spec>/Figures/13_*.png, Outputs/Tables/13_*.csv
#         Outputs/<spec>/Exports/data_pca.csv, data_data_export.csv
#         Outputs/<spec>/Reports/13_threats_and_pressures.md
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("13")

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

# One row per species x occupancy class; see legacy_threat_profiles().
data_tap_all <- legacy_threat_profiles(data)

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
  file.path(output_dir("Exports"), "data_pca.csv"),
  fileEncoding = "Windows-1250"
)

utils::write.csv2(
  data,
  file.path(output_dir("Exports"), "data_data_export.csv"),
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
# Plot style -----
#----------------------------------------------------------#

# Short labels of the ordination variables, for the arrows.
ORDINATION_LABELS <- c(
  PLANT_QUANT = "host plant", TTP = "managed grassland", ZARUST = "neglected grassland",
  PRIKOP = "ditch", HET_INN = "habitat types", EVL = "Natura 2000", MZCHU = "small protected area",
  LandUseChange = "land-use change", Abandonment = "abandonment", HarmfulMow = "harmful mowing",
  HarmfulGrazing = "intensive grazing", GrazingByeffects = "livestock effects",
  FertilizerUse = "fertiliser", Afforestation = "afforestation", Invasives = "invasive species",
  NativeDominants = "native dominants", AbioticNaturalProcesses = "abiotic processes",
  Encroachment = "encroachment", BiomassAccumulation = "biomass", Eutrophization = "eutrophication",
  None = "no threat", DRUHteleius = "P. teleius", POSITIVE1 = "occupied"
)

#' The short label of each variable; the name itself when it has none.
ordination_label <- function(x) unname(ifelse(x %in% names(ORDINATION_LABELS), ORDINATION_LABELS[x], x))

# The four groups of species x occupancy, as interaction(DRUH, POSITIVE)
# names them: the species colour, dark and filled where the species was
# found, lighter and open where it was not; circles for P. teleius,
# triangles for P. nausithous.
TAP_GROUPS <- c("teleius.1", "teleius.0", "nausithous.1", "nausithous.0")
tap_cols <- stats::setNames(
  phengaris_col(c("deep_blue", "teleius_blue", "nausithous_brown", "nausithous_light")), TAP_GROUPS
)
tap_shapes <- stats::setNames(c(16, 1, 17, 2), TAP_GROUPS)
tap_labels <- stats::setNames(
  c(
    "italic('P. teleius')*', positive'", "italic('P. teleius')*', negative'",
    "italic('P. nausithous')*', positive'", "italic('P. nausithous')*', negative'"
  ),
  TAP_GROUPS
)

#' The colour and shape scales of the four groups; one legend.
scale_tap <- function() {
  list(
    ggplot2::scale_colour_manual(
      values = tap_cols, breaks = TAP_GROUPS, labels = parse(text = tap_labels), name = NULL
    ),
    ggplot2::scale_shape_manual(
      values = tap_shapes, breaks = TAP_GROUPS, labels = parse(text = tap_labels), name = NULL
    ),
    # The hulls take the group colour, the ellipses (by DRUH) the species one.
    ggplot2::scale_fill_manual(
      values = c(tap_cols, nausithous = tap_cols[["nausithous.1"]], teleius = tap_cols[["teleius.0"]]),
      guide = "none"
    ),
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2), shape = ggplot2::guide_legend(nrow = 2))
  )
}

#' Arrows from the origin, in the spot black.
ordination_arrows <- function(data, x, y, linewidth = 0.35) {
  ggplot2::geom_segment(
    data = data, inherit.aes = FALSE,
    ggplot2::aes(x = 0, y = 0, xend = .data[[x]], yend = .data[[y]]),
    arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm")),
    colour = phengaris_col("spot_black"), linewidth = linewidth, lineend = "round"
  )
}

#' Repelled labels of the arrows.
ordination_labels <- function(data, x, y, label, fontface = "plain") {
  ggrepel::geom_text_repel(
    data = data, inherit.aes = FALSE,
    ggplot2::aes(x = .data[[x]], y = .data[[y]], label = ordination_label(.data[[label]])),
    size = FIGURE_TEXT_MM, colour = phengaris_col("spot_black"), fontface = fontface,
    segment.colour = REFERENCE_GREY, segment.size = 0.2, min.segment.length = 0.3,
    box.padding = 0.25, max.overlaps = Inf, seed = CONFIG$seed
  )
}

#' Dashed axes through the origin.
ordination_origin <- function() {
  list(
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = GRID_GREY, linewidth = 0.3),
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = GRID_GREY, linewidth = 0.3)
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
  ggplot2::aes(
    x = PC1, y = PC2,
    colour = interaction(DRUH, POSITIVE), shape = interaction(DRUH, POSITIVE)
  )
) +
  ordination_origin() +
  ggplot2::stat_ellipse(
    ggplot2::aes(fill = DRUH, group = DRUH),
    geom = "polygon", alpha = 0.12, level = 0.95, show.legend = FALSE
  ) +
  ordination_arrows(loadings_df, "PC1", "PC2") +
  ordination_labels(loadings_df, "PC1", "PC2", "Variable") +
  ggplot2::geom_point(size = 2.4, stroke = 0.7) +
  scale_tap() +
  ggplot2::labs(
    x = paste0("PC1 (", round(pca_var[1], 1), "%)"),
    y = paste0("PC2 (", round(pca_var[2], 1), "%)")
  ) +
  theme_phengaris()

report_figure(
  pca_gg,
  "PCA biplot of threats and pressures",
  "13_pca_threats_biplot",
  height = 12
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

# Permutation tests start from the seed in config.yml, so a re-run gives the
# same p-values.
set.seed(CONFIG$seed)
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
      VIF = as.numeric(vif_vals),
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

# The threat variables in thin arrows, the two constraints in thicker ones
# with bold labels.
rda_gg <- ggplot2::ggplot(
  site_scores_rda,
  ggplot2::aes(
    x = RDA1, y = RDA2,
    colour = interaction(DRUH, POSITIVE), shape = interaction(DRUH, POSITIVE)
  )
) +
  ordination_origin() +
  ggplot2::stat_ellipse(
    ggplot2::aes(fill = DRUH, group = DRUH),
    geom = "polygon", alpha = 0.12, level = 0.95, show.legend = FALSE
  ) +
  ordination_arrows(species_scores_rda, "RDA1", "RDA2", linewidth = 0.3) +
  ordination_labels(species_scores_rda, "RDA1", "RDA2", "Variable") +
  ordination_arrows(bp_scores_rda, "RDA1", "RDA2", linewidth = 0.7) +
  ordination_labels(
    bp_scores_rda, "RDA1", "RDA2", "ConstrVar",
    fontface = ifelse(startsWith(bp_scores_rda$ConstrVar, "DRUH"), "bold.italic", "bold")
  ) +
  ggplot2::geom_point(size = 2.4, stroke = 0.7) +
  scale_tap() +
  ggplot2::labs(
    subtitle = paste0("Permutation test (overall): p = ", signif(rda_overall$`Pr(>F)`[1], 3)),
    x = "RDA1", y = "RDA2"
  ) +
  theme_phengaris()

report_figure(
  rda_gg,
  "RDA triplot constrained by species and occupancy",
  "13_rda_threats_triplot",
  height = 12
)

#----------------------------------------------------------#
# C. Convex hull plots -----
#----------------------------------------------------------#

report_section("C. Convex hull plots")

# The groups take tap_cols and tap_shapes (Plot style, above).

#--------------------------------------------------#
## PCA with fitted variable vectors -----
#--------------------------------------------------#

pca_tap <- vegan::rda(resp, scale = TRUE)

set.seed(CONFIG$seed)
ef_pca_tap <- vegan::envfit(pca_tap, resp, permutations = 999)

# The screeplot of vegan's screeplot(pca_tap, bstick = TRUE), drawn in
# ggplot: the eigenvalues of the first (up to) ten axes as bars and the
# broken-stick expectation as a line.
scree_axes <- seq_len(min(10, length(pca_tap$CA$eig)))
scree <- data.frame(
  axis = factor(names(pca_tap$CA$eig)[scree_axes], levels = names(pca_tap$CA$eig)[scree_axes]),
  eigenvalue = unname(pca_tap$CA$eig[scree_axes]),
  broken_stick = unname(vegan::bstick(pca_tap)[scree_axes])
)

report_figure(
  ggplot2::ggplot(scree, ggplot2::aes(x = axis)) +
    ggplot2::geom_col(
      ggplot2::aes(y = eigenvalue, fill = "eigenvalue"),
      width = 0.7
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = broken_stick, group = 1, colour = "broken-stick expectation"),
      linetype = "dashed", linewidth = 0.45
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = broken_stick, colour = "broken-stick expectation"),
      size = 1.6
    ) +
    ggplot2::scale_fill_manual(values = phengaris_col("deep_blue"), name = NULL) +
    ggplot2::scale_colour_manual(values = phengaris_col("spot_black"), name = NULL) +
    scale_y_count() +
    ggplot2::labs(x = "Principal component", y = "Inertia (eigenvalue)") +
    theme_phengaris(grid = "y", legend = c(0.98, 0.98)),
  "PCA screeplot with the broken-stick expectation",
  "13_pca_screeplot",
  size = "single", height = 6.5
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

pca_arrows$Variable <- rownames(pca_arrows)

pca_plot <- ggplot2::ggplot(
  pca_scores,
  ggplot2::aes(
    x = PC1, y = PC2,
    colour = interaction(DRUH, POSITIVE), shape = interaction(DRUH, POSITIVE)
  )
) +
  ordination_origin() +
  ggpubr::stat_chull(
    ggplot2::aes(group = interaction(DRUH, POSITIVE), fill = interaction(DRUH, POSITIVE)),
    alpha = 0.2, geom = "polygon", show.legend = FALSE
  ) +
  ordination_arrows(pca_arrows, "PC1", "PC2") +
  ordination_labels(pca_arrows, "PC1", "PC2", "Variable") +
  ggplot2::geom_point(size = 2.4, stroke = 0.7) +
  scale_tap() +
  theme_phengaris()

report_figure(
  pca_plot,
  "PCA convex hulls by species and occupancy",
  "13_pca_tap",
  height = 12
)
write_figure_pdf(pca_plot, "13_pca_tap", height = 12)

#--------------------------------------------------#
## RDA convex hulls -----
#--------------------------------------------------#

set.seed(CONFIG$seed)
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

# The groups are labelled at their centroids, so the plot needs no legend.
rda_plot <- ggplot2::ggplot(
  rda_scores,
  ggplot2::aes(
    x = RDA1, y = RDA2,
    colour = interaction(DRUH, POSITIVE), shape = interaction(DRUH, POSITIVE)
  )
) +
  ordination_origin() +
  ggpubr::stat_chull(
    ggplot2::aes(group = interaction(DRUH, POSITIVE), fill = interaction(DRUH, POSITIVE)),
    alpha = 0.2, geom = "polygon", show.legend = FALSE
  ) +
  ggplot2::geom_point(size = 2.4, stroke = 0.7) +
  ggrepel::geom_text_repel(
    data = rda_centroids, inherit.aes = FALSE,
    ggplot2::aes(x = RDA1, y = RDA2, label = tap_labels[as.character(group)]),
    parse = TRUE, size = FIGURE_TEXT_MM, colour = phengaris_col("spot_black"),
    box.padding = 0.5, seed = CONFIG$seed
  ) +
  scale_tap() +
  ggplot2::labs(x = "RDA1", y = "RDA2") +
  theme_phengaris(legend = "none")

report_figure(
  rda_plot,
  "RDA convex hulls by species and occupancy",
  "13_rda_tap",
  size = "single", height = 8
)
write_figure_pdf(rda_plot, "13_rda_tap", size = "single", height = 8)

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

step_end("13")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

# Step 13 - Threats and pressures

_Generated 2026-09-05 23:10:58_

Ordination of the threats and pressures recorded at monitoring sites, summarised to one point per species and occupancy class. An unconstrained PCA describes the main gradients; an RDA constrained by species and occupancy tests whether those two factors explain them.

> **Note.** These threat variables were constant across the four groups and could not enter the ordination: TTP, ZARUST, PRIKOP, None .

## A. Unconstrained PCA

**Table - PCA variance explained by axis**

| axis | Standard deviation | Proportion of Variance | Cumulative Proportion |
|---|---|---|---|
| PC1 | 3.518 | 0.728 | 0.728 |
| PC2 | 1.624 | 0.1552 | 0.8832 |
| PC3 | 1.409 | 0.1168 | 1 |
| PC4 | 2.2e-15 | 0 | 1 |

Full table: [`13_pca_variance.csv`](../Tables/13_pca_variance.csv)

**Table - PCA loadings on the first two axes**

| Variable | PC1 | PC2 |
|---|---|---|
| PLANT_QUANT | 0.2003 | -0.4079 |
| HET_INN | 0.2743 | -0.1417 |
| EVL | 0.2677 | 0.2002 |
| MZCHU | 0.2263 | 0.357 |
| LandUseChange | -0.1857 | 0.4568 |
| Abandonment | -0.2582 | -0.1896 |
| HarmfulMow | -0.2452 | 0.1908 |
| HarmfulGrazing | 0.2635 | -0.05218 |
| GrazingByeffects | 0.2659 | 0.01244 |
| FertilizerUse | -0.2238 | 0.004218 |
| Afforestation | -0.2569 | 0.182 |
| Invasives | -0.1959 | -0.1778 |
| NativeDominants | -0.2411 | -0.3047 |
| AbioticNaturalProcesses | -0.2368 | 0.2933 |
| Encroachment | -0.2523 | 0.06228 |
| BiomassAccumulation | 0.2678 | 0.1018 |
| Eutrophization | -0.2374 | -0.3228 |

Full table: [`13_pca_loadings.csv`](../Tables/13_pca_loadings.csv)

**Figure - PCA biplot of threats and pressures**

![PCA biplot of threats and pressures](../Figures/13_pca_threats_biplot.png)

## B. Constrained RDA

**Table - RDA permutation test, overall**

| term | Df | Variance | F | Pr(>F) |
|---|---|---|---|---|
| Model | 2 | 13.78 | 2.141 | 0.3333 |
| Residual | 1 | 3.218 |  |  |

Full table: [`13_rda_anova_overall.csv`](../Tables/13_rda_anova_overall.csv)

**Table - RDA permutation test, by term**

| term | Df | Variance | F | Pr(>F) |
|---|---|---|---|---|
| DRUH | 1 | 5.177 | 1.609 | 0.5 |
| POSITIVE | 1 | 8.605 | 2.674 | 0.1667 |
| Residual | 1 | 3.218 |  |  |

Full table: [`13_rda_anova_by_term.csv`](../Tables/13_rda_anova_by_term.csv)

**Table - Variance inflation factors of the constraining variables**

| term | VIF |
|---|---|
| DRUHteleius | 1 |
| POSITIVE1 | 1 |

Full table: [`13_rda_vif.csv`](../Tables/13_rda_vif.csv)

**Table - RDA variable scores on the first two axes**

| Variable | RDA1 | RDA2 |
|---|---|---|
| PLANT_QUANT | 0.5638 | -0.1802 |
| HET_INN | 0.6131 | -0.1559 |
| EVL | 0.5243 | 0.1142 |
| MZCHU | 0.4324 | 0.3492 |
| LandUseChange | -0.4898 | 0.4032 |
| Abandonment | -0.4802 | -0.01553 |
| HarmfulMow | -0.6303 | -0.04893 |
| HarmfulGrazing | 0.5371 | -0.2008 |
| GrazingByeffects | 0.5289 | -0.1473 |
| FertilizerUse | -0.5692 | -0.2863 |
| Afforestation | -0.6419 | -0.01398 |
| Invasives | -0.4727 | -0.4425 |
| NativeDominants | -0.4287 | -0.1437 |
| AbioticNaturalProcesses | -0.5458 | 0.3413 |
| Encroachment | -0.504 | 0.2473 |
| BiomassAccumulation | 0.5974 | 0.2162 |
| Eutrophization | -0.4195 | -0.1675 |

Full table: [`13_rda_variable_scores.csv`](../Tables/13_rda_variable_scores.csv)

**Figure - RDA triplot constrained by species and occupancy**

![RDA triplot constrained by species and occupancy](../Figures/13_rda_threats_triplot.png)

## C. Convex hull plots

**Figure - PCA screeplot with the broken-stick expectation**

![PCA screeplot with the broken-stick expectation](../Figures/13_pca_screeplot.png)

**Figure - PCA convex hulls by species and occupancy**

![PCA convex hulls by species and occupancy](../Figures/13_pca_tap.png)

**Table - RDA (scaled) permutation test, overall**

| term | Df | Variance | F | Pr(>F) |
|---|---|---|---|---|
| Model | 2 | 13.78 | 2.141 | 0.3333 |
| Residual | 1 | 3.218 |  |  |

Full table: [`13_rda_tap_anova_overall.csv`](../Tables/13_rda_tap_anova_overall.csv)

**Table - RDA (scaled) permutation test, by axis**

| term | Df | Variance | F | Pr(>F) |
|---|---|---|---|---|
| RDA1 | 1 | 11.47 | 3.565 | 0.3333 |
| RDA2 | 1 | 2.309 | 1.435 | 0.4167 |
| Residual | 1 | 3.218 |  |  |

Full table: [`13_rda_tap_anova_by_axis.csv`](../Tables/13_rda_tap_anova_by_axis.csv)

**Table - RDA (scaled) permutation test, by term**

| term | Df | Variance | F | Pr(>F) |
|---|---|---|---|---|
| DRUH | 1 | 5.177 | 1.609 | 0.5 |
| POSITIVE | 1 | 8.605 | 2.674 | 0.1667 |
| Residual | 1 | 3.218 |  |  |

Full table: [`13_rda_tap_anova_by_term.csv`](../Tables/13_rda_tap_anova_by_term.csv)

**Figure - RDA convex hulls by species and occupancy**

![RDA convex hulls by species and occupancy](../Figures/13_rda_tap.png)

> **Note.** Both ordinations are run on 4 rows: one per species and occupancy class. With that many points the ellipses, convex hulls and permutation tests carry very little information, and the RDA is saturated by its two constraining factors. The ordinations describe the structure of the group means; they do not test differences between individual sites.


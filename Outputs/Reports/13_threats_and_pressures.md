# Step 13 - Threats and pressures

_Generated 2026-09-05 11:02:04_

Ordination of the threats and pressures recorded at monitoring sites, summarised to one point per species and occupancy class. An unconstrained PCA describes the main gradients; an RDA constrained by species and occupancy tests whether those two factors explain them.

> **Note.** These threat variables were constant across the four groups and could not enter the ordination: TTP, ZARUST, PRIKOP, None .

## A. Unconstrained PCA

**Table - PCA variance explained by axis**

| axis | Standard deviation | Proportion of Variance | Cumulative Proportion |
|---|---|---|---|
| PC1 | 3.541 | 0.7375 | 0.7375 |
| PC2 | 1.586 | 0.1479 | 0.8854 |
| PC3 | 1.396 | 0.1146 | 1 |
| PC4 | 2.132e-15 | 0 | 1 |

Full table: [`13_pca_variance.csv`](../Tables/13_pca_variance.csv)

**Table - PCA loadings on the first two axes**

| Variable | PC1 | PC2 |
|---|---|---|
| PLANT_QUANT | 0.1995 | -0.4334 |
| HET_INN | 0.2732 | -0.1303 |
| EVL | 0.2656 | 0.2134 |
| MZCHU | 0.2507 | 0.2798 |
| LandUseChange | -0.1861 | 0.4489 |
| Abandonment | -0.2566 | -0.2191 |
| HarmfulMow | -0.2432 | 0.225 |
| HarmfulGrazing | 0.2628 | -0.01923 |
| GrazingByeffects | 0.265 | 0.04534 |
| FertilizerUse | -0.2209 | 0.05421 |
| Afforestation | -0.255 | 0.2082 |
| Invasives | -0.1926 | -0.1262 |
| NativeDominants | -0.2391 | -0.3278 |
| AbioticNaturalProcesses | -0.2367 | 0.2712 |
| Encroachment | -0.2519 | 0.02247 |
| BiomassAccumulation | 0.2651 | 0.0814 |
| Eutrophization | -0.2354 | -0.3442 |

Full table: [`13_pca_loadings.csv`](../Tables/13_pca_loadings.csv)

**Figure - PCA biplot of threats and pressures**

![PCA biplot of threats and pressures](../Figures/13_pca_threats_biplot.png)

## B. Constrained RDA

**Table - RDA permutation test, overall**

| term | Df | Variance | F | Pr(>F) |
|---|---|---|---|---|
| Model | 2 | 13.76 | 2.124 | 0.3333 |
| Residual | 1 | 3.239 |  |  |

Full table: [`13_rda_anova_overall.csv`](../Tables/13_rda_anova_overall.csv)

**Table - RDA permutation test, by term**

| term | Df | Variance | F | Pr(>F) |
|---|---|---|---|---|
| DRUH | 1 | 5.037 | 1.555 | 0.5 |
| POSITIVE | 1 | 8.724 | 2.693 | 0.1667 |
| Residual | 1 | 3.239 |  |  |

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
| PLANT_QUANT | 0.5653 | -0.1754 |
| HET_INN | 0.6144 | -0.1506 |
| EVL | 0.5233 | 0.1186 |
| MZCHU | 0.4917 | 0.2414 |
| LandUseChange | -0.4932 | 0.399 |
| Abandonment | -0.48 | -0.01963 |
| HarmfulMow | -0.6299 | -0.05431 |
| HarmfulGrazing | 0.5388 | -0.1962 |
| GrazingByeffects | 0.5301 | -0.1428 |
| FertilizerUse | -0.5668 | -0.2911 |
| Afforestation | -0.6417 | -0.01945 |
| Invasives | -0.4689 | -0.4465 |
| NativeDominants | -0.4275 | -0.1473 |
| AbioticNaturalProcesses | -0.5487 | 0.3366 |
| Encroachment | -0.5061 | 0.2429 |
| BiomassAccumulation | 0.5955 | 0.2213 |
| Eutrophization | -0.4181 | -0.1711 |

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
| Model | 2 | 13.76 | 2.124 | 0.3333 |
| Residual | 1 | 3.239 |  |  |

Full table: [`13_rda_tap_anova_overall.csv`](../Tables/13_rda_tap_anova_overall.csv)

**Table - RDA (scaled) permutation test, by axis**

| term | Df | Variance | F | Pr(>F) |
|---|---|---|---|---|
| RDA1 | 1 | 11.61 | 3.584 | 0.3333 |
| RDA2 | 1 | 2.152 | 1.329 | 0.5 |
| Residual | 1 | 3.239 |  |  |

Full table: [`13_rda_tap_anova_by_axis.csv`](../Tables/13_rda_tap_anova_by_axis.csv)

**Table - RDA (scaled) permutation test, by term**

| term | Df | Variance | F | Pr(>F) |
|---|---|---|---|---|
| DRUH | 1 | 5.037 | 1.555 | 0.5 |
| POSITIVE | 1 | 8.724 | 2.693 | 0.1667 |
| Residual | 1 | 3.239 |  |  |

Full table: [`13_rda_tap_anova_by_term.csv`](../Tables/13_rda_tap_anova_by_term.csv)

**Figure - RDA convex hulls by species and occupancy**

![RDA convex hulls by species and occupancy](../Figures/13_rda_tap.png)

> **Note.** Both ordinations are run on 4 rows: one per species and occupancy class. With that many points the ellipses, convex hulls and permutation tests carry very little information, and the RDA is saturated by its two constraining factors. The ordinations describe the structure of the group means; they do not test differences between individual sites.


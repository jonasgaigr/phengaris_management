# Step 11 - Models both species

_Generated 2026-09-05 23:09:54_

Models fitted on the two species jointly, with species as a fixed effect, and the non-parametric tests comparing abundance and occupancy between P. nausithous and P. teleius.

**Table - Model fit statistics**

| model | label | group | engine | n_obs | AIC | BIC | logLik | deviance | df | status |
|---|---|---|---|---|---|---|---|---|---|---|
| both_species | Species only | Both species | glmer | 4540 | 5426 | 5451 | -2709 | 4446 | 4 | ok |
| both_management_type | Species, mowing and grazing | Both species | glmer | 4540 | 5420 | 5459 | -2704 | 4438 | 6 | ok |
| both_mapped_habitat | Species and mapped habitat code | Both species | glmer | 684 | 907 | 1029 | -426.5 | 762.1 | 27 | ok |
| both_recorded_habitat | Species and recorded habitat types | Both species | glmer | 4514 | 5382 | 5440 | -2682 | 4401 | 9 | fitted with warnings |
| both_cooccurrence | Co-occurrence of the two species by species | Both species | glmer | 4540 | 5286 | 5318 | -2638 | 4957 | 5 | ok |

Full table: [`11_both_species_fit_statistics.csv`](../Tables/11_both_species_fit_statistics.csv)

**Table - Fixed-effect coefficients**

| model | label | term | estimate | std_error | statistic | p_value |
|---|---|---|---|---|---|---|
| both_species | Species only | (Intercept) | -0.2945 | 0.2164 | -1.361 | 0.1735 |
| both_species | Species only | as.factor(DRUH)Phengaris teleius | -1.787 | 0.105 | -17.02 | 6.106e-65 |
| both_management_type | Species, mowing and grazing | (Intercept) | -0.4163 | 0.2227 | -1.869 | 0.06165 |
| both_management_type | Species, mowing and grazing | as.factor(DRUH)Phengaris teleius | -1.789 | 0.1052 | -17 | 8.92e-65 |
| both_management_type | Species, mowing and grazing | as.factor(MOW)1 | 0.1876 | 0.07738 | 2.424 | 0.01534 |
| both_management_type | Species, mowing and grazing | as.factor(GRAZE)1 | 0.4765 | 0.2542 | 1.874 | 0.0609 |
| both_mapped_habitat | Species and mapped habitat code | (Intercept) | -15.98 | 2642 | -0.006049 | 0.9952 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(DRUH)Phengaris teleius | -1.395 | 0.2081 | -6.704 | 2.023e-11 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)L10.2 | 16.68 | 2642 | 0.006312 | 0.995 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)L2.2 | 15.64 | 2642 | 0.00592 | 0.9953 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)L2.3 | 14.95 | 2642 | 0.005659 | 0.9955 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)L2.4 | 16.68 | 2642 | 0.006313 | 0.995 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)L3.1 | 14.58 | 2642 | 0.005519 | 0.9956 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)L3.3A | -0.6421 | 4851 | -0.0001324 | 0.9999 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)L3.3B | 15.09 | 2642 | 0.00571 | 0.9954 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)L4 | 34.49 | 1.079e+04 | 0.003198 | 0.9974 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)L5.1 | 16.06 | 2642 | 0.006079 | 0.9952 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)L5.4 | -0.152 | 3060 | -4.965e-05 | 1 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)R2.1 | 16.42 | 2642 | 0.006216 | 0.995 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)R2.2 | 15.78 | 2642 | 0.005974 | 0.9952 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)R2.3 | 1.201 | 3013 | 0.0003987 | 0.9997 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)T1.1 | 16.01 | 2642 | 0.006061 | 0.9952 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)T1.2 | 15.65 | 2642 | 0.005925 | 0.9953 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)T1.6 | 16.24 | 2642 | 0.006146 | 0.9951 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)T1.7 | -0.7394 | 5022 | -0.0001472 | 0.9999 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)T1.9 | 15.83 | 2642 | 0.005992 | 0.9952 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)T2.3B | 16.45 | 2642 | 0.006226 | 0.995 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)T3.4C | -0.06441 | 3798 | -1.696e-05 | 1 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)T3.4D | 15.62 | 2642 | 0.005912 | 0.9953 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)V1F | 32.88 | 5373 | 0.006119 | 0.9951 |
| both_mapped_habitat | Species and mapped habitat code | as.factor(BIOTOP)V4A | 15.98 | 2642 | 0.006048 | 0.9952 |
| both_recorded_habitat | Species and recorded habitat types | (Intercept) | -0.3877 | 0.2392 | -1.621 | 0.1051 |
| both_recorded_habitat | Species and recorded habitat types | as.factor(DRUH)Phengaris teleius | -1.799 | 0.1059 | -16.99 | 9.01e-65 |
| both_recorded_habitat | Species and recorded habitat types | as.factor(TTP)1 | 0.2268 | 0.1722 | 1.317 | 0.1877 |
| both_recorded_habitat | Species and recorded habitat types | as.factor(ZARUST)1 | 0.0445 | 0.09855 | 0.4515 | 0.6516 |
| both_recorded_habitat | Species and recorded habitat types | as.factor(PRIKOP)1 | -0.1942 | 0.1323 | -1.468 | 0.1421 |
| both_recorded_habitat | Species and recorded habitat types | as.factor(JINY)1 | -0.281 | 0.1219 | -2.305 | 0.02114 |
| both_recorded_habitat | Species and recorded habitat types | as.factor(MOW)1 | -0.04108 | 0.1609 | -0.2553 | 0.7985 |
| both_cooccurrence | Co-occurrence of the two species by species | (Intercept) | -0.3555 | 0.1975 | -1.8 | 0.07192 |
| both_cooccurrence | Co-occurrence of the two species by species | as.factor(SPEC_NUM)1 | 21.94 | 54.58 | 0.402 | 0.6877 |
| both_cooccurrence | Co-occurrence of the two species by species | as.factor(DRUH)Phengaris teleius | -1.553 | 0.09458 | -16.42 | 1.425e-60 |

Full table: [`11_both_species_coefficients.csv`](../Tables/11_both_species_coefficients.csv)

**Table - Random-effect variances**

| model | label | group | term | variance | sd |
|---|---|---|---|---|---|
| both_species | Species only | X:Y | (Intercept) | 0.5986 | 0.7737 |
| both_species | Species only | YEAR | (Intercept) | 0.2691 | 0.5188 |
| both_management_type | Species, mowing and grazing | X:Y | (Intercept) | 0.5989 | 0.7739 |
| both_management_type | Species, mowing and grazing | YEAR | (Intercept) | 0.2735 | 0.523 |
| both_mapped_habitat | Species and mapped habitat code | X:Y | (Intercept) | 0.3351 | 0.5788 |
| both_mapped_habitat | Species and mapped habitat code | YEAR | (Intercept) | 3.365e-07 | 0.0005801 |
| both_recorded_habitat | Species and recorded habitat types | X:Y | (Intercept) | 0.5993 | 0.7742 |
| both_recorded_habitat | Species and recorded habitat types | YEAR | (Intercept) | 0.2899 | 0.5384 |
| both_cooccurrence | Co-occurrence of the two species by species | X:Y | (Intercept) | 0.1689 | 0.411 |
| both_cooccurrence | Co-occurrence of the two species by species | YEAR | (Intercept) | 0.2238 | 0.473 |

Full table: [`11_both_species_random_effects.csv`](../Tables/11_both_species_random_effects.csv)

**Table - Models that failed or warned during fitting**

| model | label | status | message |
|---|---|---|---|
| both_recorded_habitat | Species and recorded habitat types | warning | Model failed to converge with max\|grad\| = 0.0303152 (tol = 0.002, component 1)   See ?lme4::convergence and ?lme4::troubleshooting. |

Full table: [`11_both_species_fitting_issues.csv`](../Tables/11_both_species_fitting_issues.csv)

**Table - Model specifications as fitted**

| model | label | engine | formula |
|---|---|---|---|
| both_species | Species only | glmer | as.factor(POSITIVE) ~ as.factor(DRUH) + (1 \| YEAR) + (1 \| X:Y) |
| both_management_type | Species, mowing and grazing | glmer | as.factor(POSITIVE) ~ as.factor(DRUH) + as.factor(MOW) + as.factor(GRAZE) +      (1 \| YEAR) + (1 \| X:Y) |
| both_mapped_habitat | Species and mapped habitat code | glmer | as.factor(POSITIVE) ~ as.factor(DRUH) + as.factor(BIOTOP) + (1 \|      YEAR) + (1 \| X:Y) |
| both_recorded_habitat | Species and recorded habitat types | glmer | as.factor(POSITIVE) ~ as.factor(DRUH) + as.factor(TTP) + as.factor(ZARUST) +      as.factor(PRIKOP) + as.factor(JINY) + as.factor(MOW) + as.factor(ZARUST) +      (1 \| YEAR) + (1 \| X:Y) |
| both_cooccurrence | Co-occurrence of the two species by species | glmer | as.factor(POSITIVE) ~ as.factor(SPEC_NUM) * as.factor(DRUH) +      (1 \| YEAR) + (1 \| (X:Y)) |

Full table: [`11_both_species_specifications.csv`](../Tables/11_both_species_specifications.csv)

**Table - Both-species models**

| model | label | n_obs | df | AIC | BIC | logLik | delta_AIC |
|---|---|---|---|---|---|---|---|
| both_mapped_habitat | Species and mapped habitat code | 684 | 27 | 907 | 1029 | -426.5 | 0 |
| both_cooccurrence | Co-occurrence of the two species by species | 4540 | 5 | 5286 | 5318 | -2638 | 4379 |
| both_recorded_habitat | Species and recorded habitat types | 4514 | 9 | 5382 | 5440 | -2682 | 4475 |
| both_management_type | Species, mowing and grazing | 4540 | 6 | 5420 | 5459 | -2704 | 4513 |
| both_species | Species only | 4540 | 4 | 5426 | 5451 | -2709 | 4518 |

Full table: [`11_both_species_aic.csv`](../Tables/11_both_species_aic.csv)

## Comparisons between the species

**Table - Non-parametric comparisons between the species**

| comparison | method | statistic | parameter | p_value |
|---|---|---|---|---|
| Counted specimens, P. nausithous vs P. teleius | Wilcoxon rank sum test with continuity correction | 4.088e+05 |  | 0.6785 |
| Counted specimens by species | Kruskal-Wallis rank sum test | 0.1718 | 1 | 0.6785 |
| Site occupancy, P. nausithous vs P. teleius | Wilcoxon rank sum test with continuity correction | 2.777e+06 |  | 0 |

Full table: [`11_species_comparisons.csv`](../Tables/11_species_comparisons.csv)

> **Note.** The original script also ran a Kruskal-Wallis test comparing POSITIVE against POSITIVE on an object that was never created. That test compared a vector with itself and could not return anything meaningful, so it is not reproduced here.


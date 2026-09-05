# Step 09 - Models P nausithous

_Generated 2026-09-05 23:07:48_

Binomial occupancy models for Phengaris nausithous, grouped by the hypothesis each set addresses: baseline space and time, habitat extent and host plant, management, and conservation status.

**Table - Model fit statistics**

| model | label | group | engine | n_obs | AIC | BIC | logLik | deviance | df | status |
|---|---|---|---|---|---|---|---|---|---|---|
| nau_null | Null model with year and site random effects | Baseline | glmer | 3216 | 4311 | 4329 | -2152 | 3902 | 3 | ok |
| nau_year_factor | Year as a factor | Baseline | glm | 3216 | 4296 | 4332 | -2142 | 4284 | 6 | ok |
| nau_year_linear | Year as a linear trend | Baseline | glm | 3216 | 4383 | 4395 | -2190 | 4379 | 2 | ok |
| nau_year_poly | Year as a quadratic trend | Baseline | glm | 3216 | 4361 | 4379 | -2177 | 4355 | 3 | ok |
| nau_spatial | Spatial position | Baseline | glm | 3216 | 4426 | 4438 | -2211 | 4422 | 2 | ok |
| nau_spatiotemporal | Year and spatial position | Baseline | glm | 3216 | 4279 | 4322 | -2133 | 4265 | 7 | ok |
| nau_area_null | Null model on records with a positive site area | Habitat | glmer | 1805 | 2318 | 2335 | -1156 | 1986 | 3 | ok |
| nau_area | Site area | Habitat | glmer | 1805 | 2319 | 2341 | -1155 | 1989 | 4 | ok |
| nau_area_null_glm | Intercept-only model on records with a known site area | Habitat | glm | 3216 | 4436 | 4442 | -2217 | 4434 | 1 | ok |
| nau_area_poly | Site area, quadratic | Habitat | glmer | 1805 | 2320 | 2348 | -1155 | 1989 | 5 | ok |
| nau_plant | Host plant abundance | Habitat | glmer | 1805 | 2267 | 2289 | -1130 | 1890 | 4 | ok |
| nau_plant_poly | Host plant abundance, quadratic | Habitat | glmer | 3216 | 4182 | 4213 | -2086 | 3730 | 5 | ok |
| nau_resource_density | Site area by host plant abundance | Habitat | glmer | 1805 | 2269 | 2302 | -1128 | 1888 | 6 | ok |
| nau_mow_null | Null model on records where mowing was assessed | Management | glmer | 1753 | 2363 | 2380 | -1179 | 2085 | 3 | ok |
| nau_timing | Mowing timing | Management | glmer | 1753 | 2296 | 2318 | -1144 | 1981 | 4 | ok |
| nau_method | Mowing method | Management | glmer | 1753 | 2356 | 2378 | -1174 | 2072 | 4 | ok |
| nau_method_timing | Mowing method by timing (selected management model) | Management | glmer | 1753 | 2295 | 2328 | -1141 | 1969 | 6 | ok |
| nau_mow_null_glm | Intercept-only model on records where mowing was assessed | Management | glm | 1753 | 2422 | 2428 | -1210 | 2420 | 1 | ok |
| nau_graze_null | Null model on records where grazing was assessed | Management | glmer | 77 | 111.5 | 118.6 | -52.77 | 95.73 | 3 | ok |
| nau_graze | Grazing present | Management | glmer | 3216 | 4312 | 4337 | -2152 | 3901 | 4 | ok |
| nau_graze_method | Grazing intensity | Management | glmer | 77 | 113.2 | 122.6 | -52.59 | 95.62 | 4 | ok |
| nau_management_het | Mowing timing by method by within-site heterogeneity | Management | glmer | 1753 | 2301 | 2355 | -1140 | 1965 | 10 | fitted with warnings |
| nau_protect | Any protection | Conservation | glmer | 3216 | 4300 | 4325 | -2146 | 3881 | 4 | ok |
| nau_evl | Natura 2000 membership | Conservation | glmer | 3216 | 4302 | 4326 | -2147 | 3885 | 4 | ok |
| nau_evl_target | Natura 2000 designated for Phengaris | Conservation | glmer | 3216 | 4307 | 4331 | -2149 | 3892 | 4 | ok |
| nau_evl_combined | Natura 2000 membership and designation (selected protection model) | Conservation | glmer | 3216 | 4303 | 4334 | -2147 | 3884 | 5 | ok |
| nau_evl_year | Natura 2000 membership by year | Conservation | glmer | 3216 | 4373 | 4404 | -2182 | 3971 | 5 | fitted with warnings |
| nau_mzchu | Small-scale protected area | Conservation | glmer | 3216 | 4313 | 4337 | -2152 | 3902 | 4 | ok |
| nau_mzchu_evl | Small-scale protected area by Natura 2000 | Conservation | glmer | 3216 | 4300 | 4337 | -2144 | 3878 | 6 | ok |
| nau_ttp | Regularly managed grassland | Conservation | glmer | 3195 | 4283 | 4308 | -2138 | 3871 | 4 | ok |
| nau_management_ttp | Mowing timing by method by grassland type | Conservation | glmer | 1752 | 2292 | 2330 | -1139 | 1964 | 7 | fitted with warnings |
| nau_fsb | Habitat quality evaluation | Conservation | glmer | 3216 | 4299 | 4366 | -2139 | 3873 | 11 | ok |
| nau_het_inner | Within-site habitat heterogeneity | Conservation | glmer | 3216 | 4314 | 4344 | -2152 | 3900 | 5 | ok |
| nau_het_outer | Between-habitat heterogeneity | Conservation | glmer | 3216 | 4310 | 4334 | -2151 | 3899 | 4 | ok |
| nau_threats | Number of threats and pressures | Conservation | glmer | 1805 | 2313 | 2335 | -1153 | 1969 | 4 | ok |

Full table: [`09_nausithous_fit_statistics.csv`](../Tables/09_nausithous_fit_statistics.csv)

**Table - Fixed-effect coefficients**

| model | label | term | estimate | std_error | statistic | p_value |
|---|---|---|---|---|---|---|
| nau_null | Null model with year and site random effects | (Intercept) | -0.2973 | 0.2199 | -1.352 | 0.1765 |
| nau_year_factor | Year as a factor | (Intercept) | 0.1429 | 0.07221 | 1.979 | 0.04781 |
| nau_year_factor | Year as a factor | as.factor(YEAR)2020 | -0.3634 | 0.1056 | -3.443 | 0.0005756 |
| nau_year_factor | Year as a factor | as.factor(YEAR)2021 | -0.4273 | 0.1131 | -3.777 | 0.0001587 |
| nau_year_factor | Year as a factor | as.factor(YEAR)2022 | 0.1468 | 0.1117 | 1.315 | 0.1885 |
| nau_year_factor | Year as a factor | as.factor(YEAR)2023 | -0.4829 | 0.1391 | -3.471 | 0.0005192 |
| nau_year_factor | Year as a factor | as.factor(YEAR)2024 | -1.463 | 0.1465 | -9.987 | 1.744e-23 |
| nau_year_linear | Year as a linear trend | (Intercept) | 324.1 | 44.14 | 7.342 | 2.109e-13 |
| nau_year_linear | Year as a linear trend | as.numeric(YEAR) | -0.1604 | 0.02184 | -7.346 | 2.05e-13 |
| nau_year_poly | Year as a quadratic trend | (Intercept) | -0.1823 | 0.03592 | -5.076 | 3.853e-07 |
| nau_year_poly | Year as a quadratic trend | poly(as.numeric(YEAR), 2)1 | -15.65 | 2.103 | -7.441 | 9.981e-14 |
| nau_year_poly | Year as a quadratic trend | poly(as.numeric(YEAR), 2)2 | -10.24 | 2.084 | -4.917 | 8.806e-07 |
| nau_spatial | Spatial position | (Intercept) | 0.09164 | 0.0827 | 1.108 | 0.2678 |
| nau_spatial | Spatial position | X:Y | -4.293e-17 | 1.212e-17 | -3.542 | 0.0003969 |
| nau_spatiotemporal | Year and spatial position | (Intercept) | 0.4946 | 0.1103 | 4.486 | 7.272e-06 |
| nau_spatiotemporal | Year and spatial position | as.factor(YEAR)2020 | -0.3997 | 0.1063 | -3.761 | 0.0001691 |
| nau_spatiotemporal | Year and spatial position | as.factor(YEAR)2021 | -0.4589 | 0.1138 | -4.034 | 5.489e-05 |
| nau_spatiotemporal | Year and spatial position | as.factor(YEAR)2022 | 0.1266 | 0.1121 | 1.129 | 0.2588 |
| nau_spatiotemporal | Year and spatial position | as.factor(YEAR)2023 | -0.5206 | 0.1398 | -3.723 | 0.0001968 |
| nau_spatiotemporal | Year and spatial position | as.factor(YEAR)2024 | -1.502 | 0.1473 | -10.2 | 1.92e-24 |
| nau_spatiotemporal | Year and spatial position | X:Y | -5.305e-17 | 1.249e-17 | -4.248 | 2.161e-05 |
| nau_area_null | Null model on records with a positive site area | (Intercept) | 0.1513 | 0.8203 | 0.1845 | 0.8537 |
| nau_area | Site area | (Intercept) | -0.0001637 | 0.8313 | -0.0001969 | 0.9998 |
| nau_area | Site area | log10(AREA_SITE) | 0.06323 | 0.04654 | 1.359 | 0.1742 |
| nau_area_null_glm | Intercept-only model on records with a known site area | (Intercept) | -0.1733 | 0.0354 | -4.896 | 9.779e-07 |
| nau_area_poly | Site area, quadratic | (Intercept) | 0.1516 | 0.8251 | 0.1838 | 0.8542 |
| nau_area_poly | Site area, quadratic | poly(log10(AREA_SITE), 2)1 | 3.144 | 2.31 | 1.361 | 0.1736 |
| nau_area_poly | Site area, quadratic | poly(log10(AREA_SITE), 2)2 | 1.264 | 2.302 | 0.5493 | 0.5828 |
| nau_plant | Host plant abundance | (Intercept) | -0.8292 | 0.8133 | -1.02 | 0.3079 |
| nau_plant | Host plant abundance | PLANT_QUANT | 0.6704 | 0.09813 | 6.832 | 8.396e-12 |
| nau_plant_poly | Host plant abundance, quadratic | (Intercept) | -0.2931 | 0.2097 | -1.398 | 0.1621 |
| nau_plant_poly | Host plant abundance, quadratic | poly(PLANT_QUANT, 2)1 | 24.12 | 2.411 | 10.01 | 1.408e-23 |
| nau_plant_poly | Host plant abundance, quadratic | poly(PLANT_QUANT, 2)2 | -8.904 | 2.247 | -3.963 | 7.412e-05 |
| nau_resource_density | Site area by host plant abundance | (Intercept) | -1.307 | 0.8712 | -1.5 | 0.1336 |
| nau_resource_density | Site area by host plant abundance | log(AREA_SITE) | 0.0866 | 0.05468 | 1.584 | 0.1132 |
| nau_resource_density | Site area by host plant abundance | as.numeric(PLANT_QUANT) | 0.9327 | 0.2222 | 4.198 | 2.695e-05 |
| nau_resource_density | Site area by host plant abundance | log(AREA_SITE):as.numeric(PLANT_QUANT) | -0.04747 | 0.03474 | -1.366 | 0.1718 |
| nau_mow_null | Null model on records where mowing was assessed | (Intercept) | -0.2487 | 0.2198 | -1.132 | 0.2578 |
| nau_timing | Mowing timing | (Intercept) | -0.9405 | 0.2399 | -3.92 | 8.849e-05 |
| nau_timing | Mowing timing | as.factor(TIMING)1 | 0.99 | 0.1288 | 7.688 | 1.49e-14 |
| nau_method | Mowing method | (Intercept) | -0.4075 | 0.2285 | -1.783 | 0.07456 |
| nau_method | Mowing method | as.factor(METHOD)1 | 0.3311 | 0.1091 | 3.034 | 0.00241 |
| nau_method_timing | Mowing method by timing (selected management model) | (Intercept) | -1.091 | 0.2516 | -4.336 | 1.453e-05 |
| nau_method_timing | Mowing method by timing (selected management model) | as.factor(METHOD)1 | 0.4974 | 0.2201 | 2.26 | 0.02383 |
| nau_method_timing | Mowing method by timing (selected management model) | as.factor(TIMING)1 | 1.162 | 0.1672 | 6.948 | 3.692e-12 |
| nau_method_timing | Mowing method by timing (selected management model) | as.factor(METHOD)1:as.factor(TIMING)1 | -0.5338 | 0.2576 | -2.073 | 0.0382 |
| nau_mow_null_glm | Intercept-only model on records where mowing was assessed | (Intercept) | -0.152 | 0.04791 | -3.174 | 0.001506 |
| nau_graze_null | Null model on records where grazing was assessed | (Intercept) | -0.2432 | 0.2384 | -1.02 | 0.3076 |
| nau_graze | Grazing present | (Intercept) | -0.3004 | 0.2205 | -1.362 | 0.1731 |
| nau_graze | Grazing present | as.factor(GRAZE)1 | 0.1071 | 0.2556 | 0.4191 | 0.6751 |
| nau_graze_method | Grazing intensity | (Intercept) | -0.5492 | 0.6277 | -0.875 | 0.3816 |
| nau_graze_method | Grazing intensity | as.factor(GRAZE_MET)1 | 0.3767 | 0.6523 | 0.5774 | 0.5636 |
| nau_management_het | Mowing timing by method by within-site heterogeneity | (Intercept) | -0.8353 | 0.3777 | -2.212 | 0.027 |
| nau_management_het | Mowing timing by method by within-site heterogeneity | as.factor(TIMING)1 | 0.6414 | 0.5313 | 1.207 | 0.2273 |
| nau_management_het | Mowing timing by method by within-site heterogeneity | as.factor(METHOD)1 | -0.1657 | 0.5503 | -0.301 | 0.7634 |
| nau_management_het | Mowing timing by method by within-site heterogeneity | HET_INN | -0.1939 | 0.2177 | -0.8907 | 0.3731 |
| nau_management_het | Mowing timing by method by within-site heterogeneity | as.factor(TIMING)1:as.factor(METHOD)1 | 0.4487 | 0.7295 | 0.615 | 0.5385 |
| nau_management_het | Mowing timing by method by within-site heterogeneity | as.factor(TIMING)1:HET_INN | 0.4406 | 0.4463 | 0.9871 | 0.3236 |
| nau_management_het | Mowing timing by method by within-site heterogeneity | as.factor(METHOD)1:HET_INN | 0.459 | 0.3477 | 1.32 | 0.1868 |
| nau_management_het | Mowing timing by method by within-site heterogeneity | as.factor(TIMING)1:as.factor(METHOD)1:HET_INN | -0.7461 | 0.5416 | -1.377 | 0.1684 |
| nau_protect | Any protection | (Intercept) | -0.3652 | 0.2223 | -1.643 | 0.1003 |
| nau_protect | Any protection | as.factor(PROTECT)1 | 0.3383 | 0.0975 | 3.47 | 0.0005214 |
| nau_evl | Natura 2000 membership | (Intercept) | -0.358 | 0.2218 | -1.614 | 0.1066 |
| nau_evl | Natura 2000 membership | as.factor(EVL)1 | 0.3347 | 0.1012 | 3.307 | 0.0009427 |
| nau_evl_target | Natura 2000 designated for Phengaris | (Intercept) | -0.3258 | 0.2213 | -1.472 | 0.141 |
| nau_evl_target | Natura 2000 designated for Phengaris | as.factor(EVL_target)1 | 0.3564 | 0.1459 | 2.442 | 0.01459 |
| nau_evl_combined | Natura 2000 membership and designation (selected protection model) | (Intercept) | -0.3582 | 0.222 | -1.614 | 0.1066 |
| nau_evl_combined | Natura 2000 membership and designation (selected protection model) | as.factor(EVL)1 | 0.2947 | 0.1282 | 2.299 | 0.0215 |
| nau_evl_combined | Natura 2000 membership and designation (selected protection model) | as.factor(EVL_target)1 | 0.09401 | 0.1848 | 0.5087 | 0.6109 |
| nau_evl_year | Natura 2000 membership by year | (Intercept) | 360.5 | 0.6547 | 550.7 | 0 |
| nau_evl_year | Natura 2000 membership by year | as.factor(EVL)1 | -101 | 1.565 | -64.49 | 0 |
| nau_evl_year | Natura 2000 membership by year | as.numeric(YEAR) | -0.1785 | 0.0003239 | -551 | 0 |
| nau_evl_year | Natura 2000 membership by year | as.factor(EVL)1:as.numeric(YEAR) | 0.05011 | 0.0007747 | 64.68 | 0 |
| nau_mzchu | Small-scale protected area | (Intercept) | -0.2992 | 0.2203 | -1.358 | 0.1744 |
| nau_mzchu | Small-scale protected area | as.factor(MZCHU)1 | 0.028 | 0.1614 | 0.1734 | 0.8623 |
| nau_mzchu_evl | Small-scale protected area by Natura 2000 | (Intercept) | -0.363 | 0.2218 | -1.636 | 0.1018 |
| nau_mzchu_evl | Small-scale protected area by Natura 2000 | as.factor(MZCHU)1 | 0.3041 | 0.284 | 1.071 | 0.2843 |
| nau_mzchu_evl | Small-scale protected area by Natura 2000 | as.factor(EVL)1 | 0.4488 | 0.1141 | 3.932 | 8.438e-05 |
| nau_mzchu_evl | Small-scale protected area by Natura 2000 | as.factor(MZCHU)1:as.factor(EVL)1 | -0.7582 | 0.3579 | -2.118 | 0.03414 |
| nau_ttp | Regularly managed grassland | (Intercept) | -0.3482 | 0.2264 | -1.538 | 0.124 |
| nau_ttp | Regularly managed grassland | as.factor(TTP)1 | 0.07393 | 0.08089 | 0.914 | 0.3607 |
| nau_management_ttp | Mowing timing by method by grassland type | (Intercept) | 12.06 | 628 | 0.01921 | 0.9847 |
| nau_management_ttp | Mowing timing by method by grassland type | as.factor(TIMING)1 | 1.167 | 0.1542 | 7.568 | 3.793e-14 |
| nau_management_ttp | Mowing timing by method by grassland type | as.factor(METHOD)1 | 0.5093 | 0.2126 | 2.395 | 0.01662 |
| nau_management_ttp | Mowing timing by method by grassland type | as.factor(TTP)1 | -13.17 | 628 | -0.02097 | 0.9833 |
| nau_management_ttp | Mowing timing by method by grassland type | as.factor(TIMING)1:as.factor(METHOD)1 | -0.5378 | 0.2484 | -2.165 | 0.03036 |
| nau_fsb | Habitat quality evaluation | (Intercept) | -0.3954 | 0.2172 | -1.821 | 0.06866 |
| nau_fsb | Habitat quality evaluation | as.factor(FSB)K | -0.5255 | 0.2729 | -1.925 | 0.05417 |
| nau_fsb | Habitat quality evaluation | as.factor(FSB)L | -0.2289 | 0.3123 | -0.7329 | 0.4636 |
| nau_fsb | Habitat quality evaluation | as.factor(FSB)M | -0.4393 | 0.333 | -1.319 | 0.187 |
| nau_fsb | Habitat quality evaluation | as.factor(FSB)moz. | 0.06992 | 0.152 | 0.4599 | 0.6456 |
| nau_fsb | Habitat quality evaluation | as.factor(FSB)R | -0.184 | 0.819 | -0.2246 | 0.8223 |
| nau_fsb | Habitat quality evaluation | as.factor(FSB)T | 0.3158 | 0.1041 | 3.034 | 0.002417 |
| nau_fsb | Habitat quality evaluation | as.factor(FSB)V | -0.5566 | 0.496 | -1.122 | 0.2619 |
| nau_fsb | Habitat quality evaluation | as.factor(FSB)X | 0.3366 | 0.1087 | 3.096 | 0.001962 |
| nau_het_inner | Within-site habitat heterogeneity | (Intercept) | -0.2315 | 0.2331 | -0.9929 | 0.3208 |
| nau_het_inner | Within-site habitat heterogeneity | HET_INN | -0.05982 | 0.06886 | -0.8688 | 0.385 |
| nau_het_outer | Between-habitat heterogeneity | (Intercept) | -0.1369 | 0.2418 | -0.566 | 0.5714 |
| nau_het_outer | Between-habitat heterogeneity | as.numeric(HET_OUT) | -0.1461 | 0.0916 | -1.595 | 0.1106 |
| nau_threats | Number of threats and pressures | (Intercept) | 0.2679 | 0.8056 | 0.3325 | 0.7395 |
| nau_threats | Number of threats and pressures | SUM_THREATS | -0.1021 | 0.03845 | -2.654 | 0.007944 |

Full table: [`09_nausithous_coefficients.csv`](../Tables/09_nausithous_coefficients.csv)

**Table - Random-effect variances**

| model | label | group | term | variance | sd |
|---|---|---|---|---|---|
| nau_null | Null model with year and site random effects | X:Y | (Intercept) | 0.2608 | 0.5107 |
| nau_null | Null model with year and site random effects | YEAR | (Intercept) | 0.2797 | 0.5289 |
| nau_area_null | Null model on records with a positive site area | X:Y | (Intercept) | 0.4004 | 0.6328 |
| nau_area_null | Null model on records with a positive site area | YEAR | (Intercept) | 3.734 | 1.932 |
| nau_area | Site area | X:Y | (Intercept) | 0.3932 | 0.6271 |
| nau_area | Site area | YEAR | (Intercept) | 3.754 | 1.938 |
| nau_area_poly | Site area, quadratic | X:Y | (Intercept) | 0.3927 | 0.6266 |
| nau_area_poly | Site area, quadratic | YEAR | (Intercept) | 3.768 | 1.941 |
| nau_plant | Host plant abundance | X:Y | (Intercept) | 0.4843 | 0.6959 |
| nau_plant | Host plant abundance | YEAR | (Intercept) | 3.531 | 1.879 |
| nau_plant_poly | Host plant abundance, quadratic | X:Y | (Intercept) | 0.3051 | 0.5524 |
| nau_plant_poly | Host plant abundance, quadratic | YEAR | (Intercept) | 0.2525 | 0.5025 |
| nau_resource_density | Site area by host plant abundance | X:Y | (Intercept) | 0.4845 | 0.696 |
| nau_resource_density | Site area by host plant abundance | YEAR | (Intercept) | 3.552 | 1.885 |
| nau_mow_null | Null model on records where mowing was assessed | X:Y | (Intercept) | 0.3223 | 0.5677 |
| nau_mow_null | Null model on records where mowing was assessed | YEAR | (Intercept) | 0.2705 | 0.5201 |
| nau_timing | Mowing timing | X:Y | (Intercept) | 0.3889 | 0.6236 |
| nau_timing | Mowing timing | YEAR | (Intercept) | 0.2719 | 0.5214 |
| nau_method | Mowing method | X:Y | (Intercept) | 0.3283 | 0.573 |
| nau_method | Mowing method | YEAR | (Intercept) | 0.2767 | 0.526 |
| nau_method_timing | Mowing method by timing (selected management model) | X:Y | (Intercept) | 0.4003 | 0.6327 |
| nau_method_timing | Mowing method by timing (selected management model) | YEAR | (Intercept) | 0.2745 | 0.5239 |
| nau_graze_null | Null model on records where grazing was assessed | X:Y | (Intercept) | 0.2752 | 0.5246 |
| nau_graze_null | Null model on records where grazing was assessed | YEAR | (Intercept) | 0 | 0 |
| nau_graze | Grazing present | X:Y | (Intercept) | 0.2616 | 0.5115 |
| nau_graze | Grazing present | YEAR | (Intercept) | 0.2807 | 0.5299 |
| nau_graze_method | Grazing intensity | X:Y | (Intercept) | 0.2598 | 0.5097 |
| nau_graze_method | Grazing intensity | YEAR | (Intercept) | 0.009537 | 0.09766 |
| nau_management_het | Mowing timing by method by within-site heterogeneity | X:Y | (Intercept) | 0.4039 | 0.6356 |
| nau_management_het | Mowing timing by method by within-site heterogeneity | YEAR | (Intercept) | 0.2753 | 0.5247 |
| nau_protect | Any protection | X:Y | (Intercept) | 0.2684 | 0.5181 |
| nau_protect | Any protection | YEAR | (Intercept) | 0.2833 | 0.5322 |
| nau_evl | Natura 2000 membership | X:Y | (Intercept) | 0.2665 | 0.5162 |
| nau_evl | Natura 2000 membership | YEAR | (Intercept) | 0.2824 | 0.5314 |
| nau_evl_target | Natura 2000 designated for Phengaris | X:Y | (Intercept) | 0.2644 | 0.5142 |
| nau_evl_target | Natura 2000 designated for Phengaris | YEAR | (Intercept) | 0.2822 | 0.5312 |
| nau_evl_combined | Natura 2000 membership and designation (selected protection model) | X:Y | (Intercept) | 0.2669 | 0.5166 |
| nau_evl_combined | Natura 2000 membership and designation (selected protection model) | YEAR | (Intercept) | 0.2828 | 0.5318 |
| nau_evl_year | Natura 2000 membership by year | X:Y | (Intercept) | 0.2666 | 0.5163 |
| nau_mzchu | Small-scale protected area | X:Y | (Intercept) | 0.2611 | 0.511 |
| nau_mzchu | Small-scale protected area | YEAR | (Intercept) | 0.2799 | 0.5291 |
| nau_mzchu_evl | Small-scale protected area by Natura 2000 | X:Y | (Intercept) | 0.268 | 0.5177 |
| nau_mzchu_evl | Small-scale protected area by Natura 2000 | YEAR | (Intercept) | 0.2818 | 0.5308 |
| nau_ttp | Regularly managed grassland | X:Y | (Intercept) | 0.264 | 0.5138 |
| nau_ttp | Regularly managed grassland | YEAR | (Intercept) | 0.2797 | 0.5289 |
| nau_management_ttp | Mowing timing by method by grassland type | X:Y | (Intercept) | 0.4007 | 0.633 |
| nau_management_ttp | Mowing timing by method by grassland type | YEAR | (Intercept) | 0.2748 | 0.5242 |
| nau_fsb | Habitat quality evaluation | X:Y | (Intercept) | 0.2647 | 0.5144 |
| nau_fsb | Habitat quality evaluation | YEAR | (Intercept) | 0.2613 | 0.5112 |
| nau_het_inner | Within-site habitat heterogeneity | X.Y | (Intercept) | 0.2616 | 0.5115 |
| nau_het_inner | Within-site habitat heterogeneity | YEAR | (Intercept) | 0.2216 | 0.4707 |
| nau_het_inner | Within-site habitat heterogeneity | YEAR.1 | (Intercept) | 0.05973 | 0.2444 |
| nau_het_outer | Between-habitat heterogeneity | X:Y | (Intercept) | 0.2614 | 0.5113 |
| nau_het_outer | Between-habitat heterogeneity | YEAR | (Intercept) | 0.2802 | 0.5293 |
| nau_threats | Number of threats and pressures | X:Y | (Intercept) | 0.4184 | 0.6468 |
| nau_threats | Number of threats and pressures | YEAR | (Intercept) | 3.58 | 1.892 |

Full table: [`09_nausithous_random_effects.csv`](../Tables/09_nausithous_random_effects.csv)

**Table - Models that failed or warned during fitting**

| model | label | status | message |
|---|---|---|---|
| nau_management_het | Mowing timing by method by within-site heterogeneity | warning | Model failed to converge with max\|grad\| = 0.0263658 (tol = 0.002, component 1)   See ?lme4::convergence and ?lme4::troubleshooting. |
| nau_evl_year | Natura 2000 membership by year | warning | Model failed to converge with max\|grad\| = 0.285738 (tol = 0.002, component 1)   See ?lme4::convergence and ?lme4::troubleshooting. \| Model is nearly unidentifiable: very large eigenvalue  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio  - Rescale variables? |
| nau_management_ttp | Mowing timing by method by grassland type | warning | unable to evaluate scaled gradient \| Model failed to converge: degenerate  Hessian with 1 negative eigenvalues   See ?lme4::convergence and ?lme4::troubleshooting. |

Full table: [`09_nausithous_fitting_issues.csv`](../Tables/09_nausithous_fitting_issues.csv)

**Table - Model specifications as fitted**

| model | label | engine | formula |
|---|---|---|---|
| nau_null | Null model with year and site random effects | glmer | as.factor(POSITIVE) ~ 1 + (1 \| YEAR) + (1 \| X:Y) |
| nau_year_factor | Year as a factor | glm | as.factor(POSITIVE) ~ as.factor(YEAR) |
| nau_year_linear | Year as a linear trend | glm | as.factor(POSITIVE) ~ as.numeric(YEAR) |
| nau_year_poly | Year as a quadratic trend | glm | as.factor(POSITIVE) ~ poly(as.numeric(YEAR), 2) |
| nau_spatial | Spatial position | glm | as.factor(POSITIVE) ~ X:Y |
| nau_spatiotemporal | Year and spatial position | glm | as.factor(POSITIVE) ~ as.factor(YEAR) + X:Y |
| nau_area_null | Null model on records with a positive site area | glmer | as.factor(POSITIVE) ~ 1 + (1 \| YEAR) + (1 \| X:Y) |
| nau_area | Site area | glmer | as.factor(POSITIVE) ~ log10(AREA_SITE) + (1 \| YEAR) + (1 \| X:Y) |
| nau_area_null_glm | Intercept-only model on records with a known site area | glm | as.factor(POSITIVE) ~ 1 |
| nau_area_poly | Site area, quadratic | glmer | as.factor(POSITIVE) ~ poly(log10(AREA_SITE), 2) + (1 \| YEAR) +      (1 \| X:Y) |
| nau_plant | Host plant abundance | glmer | as.factor(POSITIVE) ~ PLANT_QUANT + (1 \| YEAR) + (1 \| X:Y) |
| nau_plant_poly | Host plant abundance, quadratic | glmer | as.factor(POSITIVE) ~ poly(PLANT_QUANT, 2) + (1 \| YEAR) + (1 \|      X:Y) |
| nau_resource_density | Site area by host plant abundance | glmer | as.factor(POSITIVE) ~ log(AREA_SITE) * as.numeric(PLANT_QUANT) +      (1 \| YEAR) + (1 \| X:Y) |
| nau_mow_null | Null model on records where mowing was assessed | glmer | as.factor(POSITIVE) ~ 1 + (1 \| YEAR) + (1 \| X:Y) |
| nau_timing | Mowing timing | glmer | as.factor(POSITIVE) ~ as.factor(TIMING) + (1 \| YEAR) + (1 \| X:Y) |
| nau_method | Mowing method | glmer | as.factor(POSITIVE) ~ as.factor(METHOD) + (1 \| YEAR) + (1 \| X:Y) |
| nau_method_timing | Mowing method by timing (selected management model) | glmer | as.factor(POSITIVE) ~ as.factor(METHOD) * as.factor(TIMING) +      (1 \| YEAR) + (1 \| X:Y) |
| nau_mow_null_glm | Intercept-only model on records where mowing was assessed | glm | as.factor(POSITIVE) ~ 1 |
| nau_graze_null | Null model on records where grazing was assessed | glmer | as.factor(POSITIVE) ~ 1 + (1 \| YEAR) + (1 \| X:Y) |
| nau_graze | Grazing present | glmer | as.factor(POSITIVE) ~ as.factor(GRAZE) + (1 \| YEAR) + (1 \| X:Y) |
| nau_graze_method | Grazing intensity | glmer | as.factor(POSITIVE) ~ as.factor(GRAZE_MET) + (1 \| YEAR) + (1 \|      X:Y) |
| nau_management_het | Mowing timing by method by within-site heterogeneity | glmer | as.factor(POSITIVE) ~ as.factor(TIMING) * as.factor(METHOD) *      HET_INN + (1 \| YEAR) + (1 \| X:Y) |
| nau_protect | Any protection | glmer | as.factor(POSITIVE) ~ as.factor(PROTECT) + (1 \| YEAR) + (1 \|      X:Y) |
| nau_evl | Natura 2000 membership | glmer | as.factor(POSITIVE) ~ as.factor(EVL) + (1 \| YEAR) + (1 \| X:Y) |
| nau_evl_target | Natura 2000 designated for Phengaris | glmer | as.factor(POSITIVE) ~ as.factor(EVL_target) + (1 \| YEAR) + (1 \|      X:Y) |
| nau_evl_combined | Natura 2000 membership and designation (selected protection model) | glmer | as.factor(POSITIVE) ~ as.factor(EVL) + as.factor(EVL_target) +      (1 \| YEAR) + (1 \| X:Y) |
| nau_evl_year | Natura 2000 membership by year | glmer | as.factor(POSITIVE) ~ as.factor(EVL) * as.numeric(YEAR) + (1 \|      X:Y) |
| nau_mzchu | Small-scale protected area | glmer | as.factor(POSITIVE) ~ as.factor(MZCHU) + (1 \| YEAR) + (1 \| X:Y) |
| nau_mzchu_evl | Small-scale protected area by Natura 2000 | glmer | as.factor(POSITIVE) ~ as.factor(MZCHU) * as.factor(EVL) + (1 \|      YEAR) + (1 \| X:Y) |
| nau_ttp | Regularly managed grassland | glmer | as.factor(POSITIVE) ~ as.factor(TTP) + (1 \| YEAR) + (1 \| X:Y) |
| nau_management_ttp | Mowing timing by method by grassland type | glmer | as.factor(POSITIVE) ~ as.factor(TIMING) * as.factor(METHOD) *      as.factor(TTP) + (1 \| YEAR) + (1 \| X:Y) |
| nau_fsb | Habitat quality evaluation | glmer | as.factor(POSITIVE) ~ as.factor(FSB) + (1 \| YEAR) + (1 \| X:Y) |
| nau_het_inner | Within-site habitat heterogeneity | glmer | as.factor(POSITIVE) ~ HET_INN + (1 \| YEAR) + (1 \| YEAR) + (1 \|      X:Y) |
| nau_het_outer | Between-habitat heterogeneity | glmer | as.factor(POSITIVE) ~ as.numeric(HET_OUT) + (1 \| YEAR) + (1 \|      X:Y) |
| nau_threats | Number of threats and pressures | glmer | as.factor(POSITIVE) ~ SUM_THREATS + (1 \| YEAR) + (1 \| X:Y) |

Full table: [`09_nausithous_specifications.csv`](../Tables/09_nausithous_specifications.csv)

## Model comparisons

The comparisons below are the ones the original script printed to the console, now ordered by AIC with the difference from the best model added.

**Table - Management models**

| model | label | n_obs | df | AIC | BIC | logLik | delta_AIC |
|---|---|---|---|---|---|---|---|
| nau_method_timing | Mowing method by timing (selected management model) | 1753 | 6 | 2295 | 2328 | -1141 | 0 |
| nau_timing | Mowing timing | 1753 | 4 | 2296 | 2318 | -1144 | 1.169 |
| nau_management_het | Mowing timing by method by within-site heterogeneity | 1753 | 10 | 2301 | 2355 | -1140 | 5.735 |
| nau_method | Mowing method | 1753 | 4 | 2356 | 2378 | -1174 | 61.21 |
| nau_mow_null | Null model on records where mowing was assessed | 1753 | 3 | 2363 | 2380 | -1179 | 68.56 |

Full table: [`09_nausithous_aic_management.csv`](../Tables/09_nausithous_aic_management.csv)

**Table - Natura 2000 models**

| model | label | n_obs | df | AIC | BIC | logLik | delta_AIC |
|---|---|---|---|---|---|---|---|
| nau_evl | Natura 2000 membership | 3216 | 4 | 4302 | 4326 | -2147 | 0 |
| nau_evl_combined | Natura 2000 membership and designation (selected protection model) | 3216 | 5 | 4303 | 4334 | -2147 | 1.742 |
| nau_evl_target | Natura 2000 designated for Phengaris | 3216 | 4 | 4307 | 4331 | -2149 | 5.037 |

Full table: [`09_nausithous_aic_natura2000.csv`](../Tables/09_nausithous_aic_natura2000.csv)

**Table - Grassland type models**

| model | label | n_obs | df | AIC | BIC | logLik | delta_AIC |
|---|---|---|---|---|---|---|---|
| nau_management_ttp | Mowing timing by method by grassland type | 1752 | 7 | 2292 | 2330 | -1139 | 0 |
| nau_ttp | Regularly managed grassland | 3195 | 4 | 4283 | 4308 | -2138 | 1991 |

Full table: [`09_nausithous_aic_grassland.csv`](../Tables/09_nausithous_aic_grassland.csv)

**Table - Habitat extent and host plant models**

| model | label | n_obs | df | AIC | BIC | logLik | delta_AIC |
|---|---|---|---|---|---|---|---|
| nau_plant | Host plant abundance | 1805 | 4 | 2267 | 2289 | -1130 | 0 |
| nau_resource_density | Site area by host plant abundance | 1805 | 6 | 2269 | 2302 | -1128 | 1.436 |
| nau_area_null | Null model on records with a positive site area | 1805 | 3 | 2318 | 2335 | -1156 | 51.09 |
| nau_area | Site area | 1805 | 4 | 2319 | 2341 | -1155 | 51.26 |
| nau_area_poly | Site area, quadratic | 1805 | 5 | 2320 | 2348 | -1155 | 52.96 |

Full table: [`09_nausithous_aic_habitat.csv`](../Tables/09_nausithous_aic_habitat.csv)


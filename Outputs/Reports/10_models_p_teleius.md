# Step 10 - Models P teleius

_Generated 2026-09-05 11:00:12_

Binomial occupancy models for Phengaris teleius, in the same four groups as for P. nausithous: baseline space and time, habitat extent and host plant, management, and conservation status.

**Table - Model fit statistics**

| model | label | group | engine | n_obs | AIC | BIC | logLik | deviance | df | status |
|---|---|---|---|---|---|---|---|---|---|---|
| tel_null | Null model with year and site random effects | Baseline | glmer | 1324 | 1131 | 1147 | -562.6 | 983.5 | 3 | ok |
| tel_null_glm | Intercept-only model | Baseline | glm | 1324 | 1133 | 1138 | -565.5 | 1131 | 1 | ok |
| tel_year_factor | Year as a factor | Baseline | glm | 1324 | 1124 | 1155 | -556.1 | 1112 | 6 | ok |
| tel_year_linear | Year as a linear trend | Baseline | glm | 1324 | 1126 | 1137 | -561.2 | 1122 | 2 | ok |
| tel_year_poly | Year as a quadratic trend | Baseline | glm | 1324 | 1126 | 1142 | -560 | 1120 | 3 | ok |
| tel_spatial | Spatial position | Baseline | glm | 1324 | 1135 | 1145 | -565.5 | 1131 | 2 | ok |
| tel_spatiotemporal | Year and spatial position, 2018 excluded | Baseline | glm | 1324 | 1128 | 1144 | -561.2 | 1122 | 3 | ok |
| tel_mapping_field | Mapping field as a fixed effect | Baseline | glm | 1324 | 796.6 | 1954 | -175.3 | 350.6 | 223 | ok |
| tel_random_only | Year and site random effects only | Baseline | glmer | 1324 | 1131 | 1147 | -562.6 | 983.5 | 3 | ok |
| tel_area_null | Null model on records with a positive site area | Habitat | glmer | 713 | 583.8 | 597.5 | -288.9 | 480.8 | 3 | ok |
| tel_area | Site area (selected habitat model) | Habitat | glmer | 713 | 585.7 | 604 | -288.9 | 480.9 | 4 | ok |
| tel_area_null_glm | Intercept-only model on records with a positive site area | Habitat | glm | 713 | 587.3 | 591.9 | -292.7 | 585.3 | 1 | ok |
| tel_plant_area_subset | Host plant abundance, records with a positive site area | Habitat | glmer | 713 | 335.9 | 354.1 | -163.9 | 11.36 | 4 | ok |
| tel_plant | Host plant abundance, all records | Habitat | glmer | 1324 | 648.5 | 669.2 | -320.2 | 15.82 | 4 | ok |
| tel_plant_poly | Host plant abundance, quadratic | Habitat | glmer | 1324 | 650.4 | 676.4 | -320.2 | 15.82 | 5 | ok |
| tel_resource_density | Site area by host plant abundance | Habitat | glmer |  |  |  |  |  |  | failed |
| tel_mow_null | Null model on records where mowing was assessed | Management | glmer | 698 | 616.1 | 629.7 | -305 | 533.8 | 3 | ok |
| tel_timing_mow_subset | Mowing timing, records where mowing was assessed | Management | glmer | 698 | 608 | 626.2 | -300 | 517.7 | 4 | ok |
| tel_method_mow_subset | Mowing method, records where mowing was assessed | Management | glmer | 698 | 614.4 | 632.6 | -303.2 | 539.2 | 4 | ok |
| tel_method_timing_mow_subset | Mowing method by timing, records where mowing was assessed | Management | glmer | 698 | 611 | 638.3 | -299.5 | 523 | 6 | ok |
| tel_mow_null_glm | Intercept-only model on records where mowing was assessed | Management | glm | 698 | 616.8 | 621.4 | -307.4 | 614.8 | 1 | ok |
| tel_graze_null | Null model on records where grazing was assessed | Management | glmer | 17 | 29.51 | 32.01 | -11.75 | 23.51 | 3 | ok |
| tel_graze | Grazing present | Management | glmer | 1324 | 648.5 | 669.2 | -320.2 | 15.82 | 4 | ok |
| tel_graze_method | Grazing intensity | Management | glmer | 17 | 31.37 | 34.7 | -11.68 | 23.37 | 4 | ok |
| tel_method | Mowing method, all records | Management | glmer | 706 | 624 | 642.3 | -308 | 547.8 | 4 | ok |
| tel_timing | Mowing timing, all records | Management | glmer | 754 | 689.6 | 708.1 | -340.8 | 607 | 4 | ok |
| tel_method_timing | Mowing method by timing, all records | Management | glmer | 698 | 611 | 638.3 | -299.5 | 523 | 6 | ok |
| tel_protect | Any protection | Conservation | glmer | 1324 | 1048 | 1069 | -520 | 973.4 | 4 | ok |
| tel_evl | Natura 2000 membership | Conservation | glmer | 1324 | 1059 | 1080 | -525.5 | 981.8 | 4 | ok |
| tel_evl_target | Natura 2000 designated for Phengaris | Conservation | glmer | 1324 | 1097 | 1118 | -544.6 | 965.5 | 4 | ok |
| tel_evl_combined | Natura 2000 membership and designation (selected protection model) | Conservation | glmer | 1324 | 1060 | 1086 | -525 | 978.3 | 5 | ok |
| tel_protection_combined | Natura 2000 membership, designation and small-scale protection | Conservation | glmer | 1324 | 1060 | 1091 | -524 | 959.6 | 6 | fitted with warnings |
| tel_mzchu | Small-scale protected area | Conservation | glmer | 1324 | 1111 | 1132 | -551.6 | 993.4 | 4 | ok |
| tel_fsb | Habitat quality evaluation, all levels | Conservation | glmer | 1324 | 1118 | 1175 | -548 | 980.5 | 11 | fitted with warnings |
| tel_fsb_subset | Habitat quality evaluation, T / X / mosaic only | Conservation | glmer | 633 | 633.3 | 655.6 | -311.7 | 569.8 | 5 | ok |
| tel_het_inner | Within-site habitat heterogeneity | Conservation | glmer | 1324 | 1133 | 1154 | -562.6 | 983.5 | 4 | ok |
| tel_het_inner_dup_year | Within-site habitat heterogeneity, year term repeated | Conservation | glmer | 1324 | 1135 | 1161 | -562.6 | 983.5 | 5 | ok |
| tel_het_outer | Between-habitat heterogeneity | Conservation | glmer | 1324 | 1133 | 1154 | -562.6 | 983.4 | 4 | ok |

Full table: [`10_teleius_fit_statistics.csv`](../Tables/10_teleius_fit_statistics.csv)

**Table - Fixed-effect coefficients**

| model | label | term | estimate | std_error | statistic | p_value |
|---|---|---|---|---|---|---|
| tel_null | Null model with year and site random effects | (Intercept) | -1.895 | 0.215 | -8.816 | 1.188e-18 |
| tel_null_glm | Intercept-only model | (Intercept) | -1.715 | 0.07643 | -22.43 | 1.8e-111 |
| tel_year_factor | Year as a factor | (Intercept) | -1.453 | 0.1756 | -8.272 | 1.322e-16 |
| tel_year_factor | Year as a factor | as.factor(YEAR)2020 | -0.2857 | 0.2239 | -1.276 | 0.202 |
| tel_year_factor | Year as a factor | as.factor(YEAR)2021 | 0.2513 | 0.2517 | 0.9984 | 0.3181 |
| tel_year_factor | Year as a factor | as.factor(YEAR)2022 | -0.4376 | 0.2489 | -1.758 | 0.07877 |
| tel_year_factor | Year as a factor | as.factor(YEAR)2023 | -0.5525 | 0.3345 | -1.652 | 0.09856 |
| tel_year_factor | Year as a factor | as.factor(YEAR)2024 | -1.049 | 0.3729 | -2.812 | 0.00492 |
| tel_year_linear | Year as a linear trend | (Intercept) | 299.2 | 104.4 | 2.867 | 0.004148 |
| tel_year_linear | Year as a linear trend | as.numeric(YEAR) | -0.1489 | 0.05164 | -2.883 | 0.00394 |
| tel_year_poly | Year as a quadratic trend | (Intercept) | -1.741 | 0.07847 | -22.19 | 4.509e-109 |
| tel_year_poly | Year as a quadratic trend | poly(as.numeric(YEAR), 2)1 | -9.288 | 3.13 | -2.967 | 0.003007 |
| tel_year_poly | Year as a quadratic trend | poly(as.numeric(YEAR), 2)2 | -4.632 | 3.041 | -1.523 | 0.1277 |
| tel_spatial | Spatial position | (Intercept) | -1.675 | 0.1709 | -9.797 | 1.164e-22 |
| tel_spatial | Spatial position | X:Y | -6.574e-18 | 2.521e-17 | -0.2607 | 0.7943 |
| tel_spatiotemporal | Year and spatial position, 2018 excluded | (Intercept) | 300.3 | 104.4 | 2.876 | 0.004033 |
| tel_spatiotemporal | Year and spatial position, 2018 excluded | as.numeric(YEAR) | -0.1494 | 0.05168 | -2.891 | 0.003835 |
| tel_spatiotemporal | Year and spatial position, 2018 excluded | X:Y | -8.69e-18 | 2.498e-17 | -0.3479 | 0.7279 |
| tel_mapping_field | Mapping field as a fixed effect | (Intercept) | -21.57 | 2.923e+04 | -0.0007377 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5056 | 43.13 | 3.58e+04 | 0.001205 | 0.999 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5152 | 22.26 | 2.923e+04 | 0.0007614 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5153 | 4.654e-05 | 3.158e+04 | 1.474e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5154 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5155 | 4.654e-05 | 3.034e+04 | 1.534e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5156 | 21.57 | 2.923e+04 | 0.0007377 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5250 | 21.23 | 2.923e+04 | 0.0007262 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5252 | 22.26 | 2.923e+04 | 0.0007614 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5253 | 4.654e-05 | 3.053e+04 | 1.524e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5254 | 20.47 | 2.923e+04 | 0.0007001 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5255 | 4.653e-05 | 3.043e+04 | 1.529e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5256 | 4.653e-05 | 3.376e+04 | 1.379e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5257 | 21.57 | 2.923e+04 | 0.0007377 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5351 | 43.13 | 4.134e+04 | 0.001043 | 0.9992 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5352 | 21.85 | 2.923e+04 | 0.0007476 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5353 | 23.65 | 2.923e+04 | 0.0008089 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5354 | 20.47 | 2.923e+04 | 0.0007001 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5355 | 4.653e-05 | 3.034e+04 | 1.534e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5356 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5357 | 4.653e-05 | 3.066e+04 | 1.518e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5361 | 4.653e-05 | 3.101e+04 | 1.501e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5445 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5453 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5454 | 19.49 | 2.923e+04 | 0.0006666 | 0.9995 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5455 | 4.654e-05 | 3.158e+04 | 1.474e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5456 | 20.87 | 2.923e+04 | 0.000714 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5457 | 4.653e-05 | 3.043e+04 | 1.529e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5458 | 4.653e-05 | 3.008e+04 | 1.547e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5459 | 4.653e-05 | 3.202e+04 | 1.453e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5545 | 4.653e-05 | 3.158e+04 | 1.474e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5552 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5553 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5554 | 4.653e-05 | 3.125e+04 | 1.489e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5555 | 4.653e-05 | 3.034e+04 | 1.534e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5556 | 4.653e-05 | 3.268e+04 | 1.424e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5557 | 4.653e-05 | 3.202e+04 | 1.453e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5558 | 4.653e-05 | 3.158e+04 | 1.474e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5642 | 21.28 | 2.923e+04 | 0.0007279 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5643 | 4.653e-05 | 3.043e+04 | 1.529e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5644 | 4.653e-05 | 2.992e+04 | 1.555e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5652 | 4.653e-05 | 3.268e+04 | 1.424e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5653 | 21.57 | 2.923e+04 | 0.0007377 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5655 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5656 | 4.653e-05 | 3.376e+04 | 1.379e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5742 | 21.06 | 2.923e+04 | 0.0007203 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5743 | 19.43 | 2.923e+04 | 0.0006645 | 0.9995 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5744 | 4.653e-05 | 3.376e+04 | 1.379e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5753 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5754 | 4.653e-05 | 3.053e+04 | 1.524e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5755 | 4.654e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5757 | 4.654e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5758 | 4.653e-05 | 3.202e+04 | 1.453e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5843 | 43.13 | 4.134e+04 | 0.001043 | 0.9992 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5844 | 20.87 | 2.923e+04 | 0.000714 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5853 | 4.654e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5854 | 20.87 | 2.923e+04 | 0.000714 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5855 | 4.653e-05 | 3.081e+04 | 1.51e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5856 | 4.653e-05 | 3.376e+04 | 1.379e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5857 | 4.653e-05 | 3.202e+04 | 1.453e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5866 | 4.653e-05 | 3.202e+04 | 1.453e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5867 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5943 | 4.653e-05 | 3.202e+04 | 1.453e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5944 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5952 | 4.653e-05 | 3.376e+04 | 1.379e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5953 | 4.653e-05 | 3.158e+04 | 1.474e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5957 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5960 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)5967 | 43.13 | 4.134e+04 | 0.001043 | 0.9992 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6052 | 4.653e-05 | 3.125e+04 | 1.489e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6058 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6059 | 4.653e-05 | 3.101e+04 | 1.501e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6060 | 4.653e-05 | 3.158e+04 | 1.474e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6065 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6066 | 4.653e-05 | 3.158e+04 | 1.474e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6067 | 43.13 | 4.134e+04 | 0.001043 | 0.9992 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6073 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6074 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6146 | 4.653e-05 | 2.979e+04 | 1.562e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6147 | 4.653e-05 | 3.101e+04 | 1.501e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6148 | 18.35 | 2.923e+04 | 0.0006276 | 0.9995 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6149 | 4.653e-05 | 3.034e+04 | 1.534e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6151 | 20.87 | 2.923e+04 | 0.000714 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6152 | 4.653e-05 | 3.268e+04 | 1.424e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6158 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6159 | 4.653e-05 | 3.066e+04 | 1.518e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6160 | 4.653e-05 | 3.268e+04 | 1.424e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6161 | 4.653e-05 | 3.268e+04 | 1.424e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6165 | 4.653e-05 | 3.376e+04 | 1.379e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6166 | 4.653e-05 | 3.008e+04 | 1.547e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6167 | 4.653e-05 | 3.376e+04 | 1.379e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6171 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6172 | 4.653e-05 | 3.268e+04 | 1.424e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6173 | 4.653e-05 | 3.268e+04 | 1.424e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6174 | 4.653e-05 | 3.158e+04 | 1.474e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6243 | 4.653e-05 | 3.081e+04 | 1.51e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6244 | 4.653e-05 | 2.999e+04 | 1.552e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6245 | 4.653e-05 | 3.013e+04 | 1.544e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6246 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6247 | 4.653e-05 | 3.043e+04 | 1.529e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6249 | 43.13 | 4.134e+04 | 0.001043 | 0.9992 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6250 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6257 | 4.653e-05 | 3.202e+04 | 1.453e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6258 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6260 | 4.653e-05 | 3.158e+04 | 1.474e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6261 | 43.13 | 3.125e+04 | 0.00138 | 0.9989 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6262 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6263 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6264 | 20.87 | 2.923e+04 | 0.000714 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6265 | 4.653e-05 | 3.125e+04 | 1.489e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6266 | 4.653e-05 | 3.053e+04 | 1.524e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6267 | 4.653e-05 | 3.066e+04 | 1.518e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6268 | 4.653e-05 | 3.376e+04 | 1.379e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6269 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6271 | 4.653e-05 | 3.376e+04 | 1.379e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6272 | 4.653e-05 | 3.101e+04 | 1.501e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6273 | 4.653e-05 | 3.101e+04 | 1.501e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6274 | 4.653e-05 | 3.019e+04 | 1.541e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6345 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6347 | 20.87 | 2.923e+04 | 0.000714 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6348 | 20.87 | 2.923e+04 | 0.000714 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6349 | 43.13 | 4.134e+04 | 0.001043 | 0.9992 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6350 | 4.653e-05 | 3.202e+04 | 1.453e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6357 | 4.653e-05 | 3.202e+04 | 1.453e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6358 | 4.653e-05 | 3.043e+04 | 1.529e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6359 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6360 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6362 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6363 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6365 | 4.653e-05 | 3.043e+04 | 1.529e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6366 | 4.653e-05 | 3.125e+04 | 1.489e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6367 | 4.653e-05 | 3.268e+04 | 1.424e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6368 | 4.653e-05 | 3.376e+04 | 1.379e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6369 | 4.653e-05 | 3.268e+04 | 1.424e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6370 | 4.653e-05 | 3.376e+04 | 1.379e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6371 | 43.13 | 3.58e+04 | 0.001205 | 0.999 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6373 | 4.653e-05 | 3.158e+04 | 1.474e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6374 | 4.653e-05 | 3.268e+04 | 1.424e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6444 | 4.653e-05 | 2.963e+04 | 1.571e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6445 | 4.653e-05 | 2.947e+04 | 1.579e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6448 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6449 | 4.653e-05 | 2.996e+04 | 1.553e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6450 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6460 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6464 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6465 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6466 | 20.65 | 2.923e+04 | 0.0007064 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6470 | 4.653e-05 | 3.158e+04 | 1.474e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6472 | 23.18 | 2.923e+04 | 0.0007928 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6473 | 4.653e-05 | 3.158e+04 | 1.474e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6474 | 20.47 | 2.923e+04 | 0.0007001 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6475 | 21.28 | 2.923e+04 | 0.0007279 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6542 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6544 | 4.653e-05 | 3.081e+04 | 1.51e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6545 | 4.653e-05 | 3.125e+04 | 1.489e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6565 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6566 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6567 | 4.653e-05 | 3.081e+04 | 1.51e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6568 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6569 | 4.653e-05 | 3.202e+04 | 1.453e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6571 | 4.653e-05 | 3.202e+04 | 1.453e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6572 | 4.653e-05 | 3.202e+04 | 1.453e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6573 | 4.653e-05 | 3.125e+04 | 1.489e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6574 | 21.57 | 2.923e+04 | 0.0007377 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6575 | 22.26 | 2.923e+04 | 0.0007614 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6576 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6643 | 4.653e-05 | 3.053e+04 | 1.524e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6644 | 4.653e-05 | 3.066e+04 | 1.518e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6645 | 4.653e-05 | 3.053e+04 | 1.524e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6666 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6669 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6670 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6672 | 22.66 | 2.923e+04 | 0.0007753 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6674 | 43.13 | 3.58e+04 | 0.001205 | 0.999 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6675 | 19.62 | 2.923e+04 | 0.0006712 | 0.9995 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6676 | 20.47 | 2.923e+04 | 0.0007001 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6770 | 4.653e-05 | 3.376e+04 | 1.379e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6771 | 4.653e-05 | 3.202e+04 | 1.453e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6772 | 21.16 | 2.923e+04 | 0.0007239 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6773 | 21.57 | 2.923e+04 | 0.0007377 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6774 | 24.27 | 2.923e+04 | 0.0008304 | 0.9993 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6851 | 20.87 | 2.923e+04 | 0.000714 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6853 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6870 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6871 | 20.18 | 2.923e+04 | 0.0006903 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6872 | 43.13 | 3.268e+04 | 0.00132 | 0.9989 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6873 | 43.13 | 4.134e+04 | 0.001043 | 0.9992 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6874 | 43.13 | 4.134e+04 | 0.001043 | 0.9992 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6949 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6950 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6953 | 4.653e-05 | 4.134e+04 | 1.126e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6971 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6972 | 21.57 | 2.923e+04 | 0.0007377 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)6973 | 43.13 | 3.58e+04 | 0.001205 | 0.999 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7049 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7050 | 21.57 | 2.923e+04 | 0.0007377 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7051 | 21.85 | 2.923e+04 | 0.0007476 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7052 | 4.653e-05 | 3.202e+04 | 1.453e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7053 | 22.26 | 2.923e+04 | 0.0007614 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7069 | 18.93 | 2.923e+04 | 0.0006475 | 0.9995 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7070 | 4.653e-05 | 3.034e+04 | 1.534e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7072 | 4.653e-05 | 3.268e+04 | 1.424e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7073 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7151 | 23.24 | 2.923e+04 | 0.000795 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7152 | 43.13 | 3.158e+04 | 0.001366 | 0.9989 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7153 | 22.04 | 2.923e+04 | 0.0007538 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7154 | 23.18 | 2.923e+04 | 0.0007928 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7155 | 43.13 | 4.134e+04 | 0.001043 | 0.9992 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7169 | 21.16 | 2.923e+04 | 0.0007239 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7170 | 21.28 | 2.923e+04 | 0.0007279 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7171 | 4.653e-05 | 3.101e+04 | 1.501e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7249 | 4.653e-05 | 3.58e+04 | 1.3e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7250 | 43.13 | 3.58e+04 | 0.001205 | 0.999 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7251 | 43.13 | 3.158e+04 | 0.001366 | 0.9989 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7252 | 4.653e-05 | 3.125e+04 | 1.489e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7253 | 21.72 | 2.923e+04 | 0.000743 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7254 | 22.66 | 2.923e+04 | 0.0007753 | 0.9994 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7350 | 43.13 | 4.134e+04 | 0.001043 | 0.9992 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7351 | 20.06 | 2.923e+04 | 0.0006863 | 0.9995 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7352 | 43.13 | 3.158e+04 | 0.001366 | 0.9989 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7353 | 43.13 | 3.268e+04 | 0.00132 | 0.9989 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7451 | 4.653e-05 | 3.158e+04 | 1.474e-09 | 1 |
| tel_mapping_field | Mapping field as a fixed effect | as.factor(SITMAP)7452 | 43.13 | 4.134e+04 | 0.001043 | 0.9992 |
| tel_random_only | Year and site random effects only | (Intercept) | -1.895 | 0.215 | -8.816 | 1.188e-18 |
| tel_area_null | Null model on records with a positive site area | (Intercept) | -2.067 | 0.3128 | -6.607 | 3.923e-11 |
| tel_area | Site area (selected habitat model) | (Intercept) | -2.01 | 0.3846 | -5.225 | 1.74e-07 |
| tel_area | Site area (selected habitat model) | log10(AREA_SITE) | -0.02415 | 0.09678 | -0.2495 | 0.803 |
| tel_area_null_glm | Intercept-only model on records with a positive site area | (Intercept) | -1.79 | 0.107 | -16.74 | 7.13e-63 |
| tel_plant_area_subset | Host plant abundance, records with a positive site area | (Intercept) | -12.71 | 10.97 | -1.159 | 0.2463 |
| tel_plant_area_subset | Host plant abundance, records with a positive site area | PLANT_QUANT | 0.6168 | 6.063 | 0.1017 | 0.919 |
| tel_plant | Host plant abundance, all records | (Intercept) | -12.55 | 7.84 | -1.601 | 0.1093 |
| tel_plant | Host plant abundance, all records | PLANT_QUANT | 0.4955 | 4.454 | 0.1112 | 0.9114 |
| tel_plant_poly | Host plant abundance, quadratic | (Intercept) | -11.83 | 3.046 | -3.885 | 0.0001023 |
| tel_plant_poly | Host plant abundance, quadratic | poly(PLANT_QUANT, 2)1 | 11.71 | 101.6 | 0.1152 | 0.9083 |
| tel_plant_poly | Host plant abundance, quadratic | poly(PLANT_QUANT, 2)2 | -2.912 | 92.38 | -0.03152 | 0.9749 |
| tel_mow_null | Null model on records where mowing was assessed | (Intercept) | -1.784 | 0.2685 | -6.645 | 3.038e-11 |
| tel_timing_mow_subset | Mowing timing, records where mowing was assessed | (Intercept) | -2.463 | 0.3837 | -6.418 | 1.381e-10 |
| tel_timing_mow_subset | Mowing timing, records where mowing was assessed | as.factor(TIMING)1 | 0.8657 | 0.2951 | 2.934 | 0.003351 |
| tel_method_mow_subset | Mowing method, records where mowing was assessed | (Intercept) | -1.994 | 0.3029 | -6.583 | 4.602e-11 |
| tel_method_mow_subset | Mowing method, records where mowing was assessed | as.factor(METHOD)1 | 0.4236 | 0.2234 | 1.896 | 0.05793 |
| tel_method_timing_mow_subset | Mowing method by timing, records where mowing was assessed | (Intercept) | -2.492 | 0.4122 | -6.046 | 1.481e-09 |
| tel_method_timing_mow_subset | Mowing method by timing, records where mowing was assessed | as.factor(METHOD)1 | 0.1761 | 0.5831 | 0.3019 | 0.7627 |
| tel_method_timing_mow_subset | Mowing method by timing, records where mowing was assessed | as.factor(TIMING)1 | 0.7571 | 0.3722 | 2.034 | 0.04191 |
| tel_method_timing_mow_subset | Mowing method by timing, records where mowing was assessed | as.factor(METHOD)1:as.factor(TIMING)1 | 0.06544 | 0.6378 | 0.1026 | 0.9183 |
| tel_mow_null_glm | Intercept-only model on records where mowing was assessed | (Intercept) | -1.655 | 0.1031 | -16.05 | 6.043e-58 |
| tel_graze_null | Null model on records where grazing was assessed | (Intercept) | -0.1178 | 0.4859 | -0.2424 | 0.8085 |
| tel_graze | Grazing present | (Intercept) | -11.84 | 3.016 | -3.927 | 8.617e-05 |
| tel_graze | Grazing present | as.factor(GRAZE)1 | 21.62 | 14.13 | 1.53 | 0.1259 |
| tel_graze_method | Grazing intensity | (Intercept) | -0.4055 | 0.9129 | -0.4442 | 0.6569 |
| tel_graze_method | Grazing intensity | as.factor(GRAZE_MET)1 | 0.4055 | 1.08 | 0.3754 | 0.7074 |
| tel_method | Mowing method, all records | (Intercept) | -2.011 | 0.2998 | -6.707 | 1.991e-11 |
| tel_method | Mowing method, all records | as.factor(METHOD)1 | 0.461 | 0.2221 | 2.076 | 0.03792 |
| tel_timing | Mowing timing, all records | (Intercept) | -2.409 | 0.3469 | -6.945 | 3.785e-12 |
| tel_timing | Mowing timing, all records | as.factor(TIMING)1 | 0.941 | 0.2713 | 3.468 | 0.0005239 |
| tel_method_timing | Mowing method by timing, all records | (Intercept) | -2.492 | 0.4122 | -6.046 | 1.481e-09 |
| tel_method_timing | Mowing method by timing, all records | as.factor(METHOD)1 | 0.1761 | 0.5831 | 0.3019 | 0.7627 |
| tel_method_timing | Mowing method by timing, all records | as.factor(TIMING)1 | 0.7571 | 0.3722 | 2.034 | 0.04191 |
| tel_method_timing | Mowing method by timing, all records | as.factor(METHOD)1:as.factor(TIMING)1 | 0.06544 | 0.6378 | 0.1026 | 0.9183 |
| tel_protect | Any protection | (Intercept) | -2.368 | 0.251 | -9.433 | 3.976e-21 |
| tel_protect | Any protection | as.factor(PROTECT)1 | 1.606 | 0.1894 | 8.478 | 2.29e-17 |
| tel_evl | Natura 2000 membership | (Intercept) | -2.303 | 0.2433 | -9.467 | 2.874e-21 |
| tel_evl | Natura 2000 membership | as.factor(EVL)1 | 1.532 | 0.1899 | 8.065 | 7.319e-16 |
| tel_evl_target | Natura 2000 designated for Phengaris | (Intercept) | -2.109 | 0.24 | -8.787 | 1.533e-18 |
| tel_evl_target | Natura 2000 designated for Phengaris | as.factor(EVL_target)1 | 1.479 | 0.2584 | 5.724 | 1.04e-08 |
| tel_evl_combined | Natura 2000 membership and designation (selected protection model) | (Intercept) | -2.315 | 0.248 | -9.337 | 9.878e-21 |
| tel_evl_combined | Natura 2000 membership and designation (selected protection model) | as.factor(EVL)1 | 1.411 | 0.2219 | 6.358 | 2.041e-10 |
| tel_evl_combined | Natura 2000 membership and designation (selected protection model) | as.factor(EVL_target)1 | 0.286 | 0.2818 | 1.015 | 0.3103 |
| tel_protection_combined | Natura 2000 membership, designation and small-scale protection | (Intercept) | -2.35 | 0.001058 | -2221 | 0 |
| tel_protection_combined | Natura 2000 membership, designation and small-scale protection | as.factor(EVL)1 | 1.313 | 0.001058 | 1240 | 0 |
| tel_protection_combined | Natura 2000 membership, designation and small-scale protection | as.factor(EVL_target)1 | 0.3067 | 0.001058 | 289.9 | 0 |
| tel_protection_combined | Natura 2000 membership, designation and small-scale protection | as.factor(MZCHU)1 | 0.376 | 0.001058 | 355.4 | 0 |
| tel_mzchu | Small-scale protected area | (Intercept) | -2.003 | 0.2172 | -9.226 | 2.822e-20 |
| tel_mzchu | Small-scale protected area | as.factor(MZCHU)1 | 1.223 | 0.2619 | 4.668 | 3.038e-06 |
| tel_fsb | Habitat quality evaluation, all levels | (Intercept) | -2.264 | 0.1843 | -12.28 | 1.107e-34 |
| tel_fsb | Habitat quality evaluation, all levels | as.factor(FSB)K | 0.6908 | 0.4614 | 1.497 | 0.1343 |
| tel_fsb | Habitat quality evaluation, all levels | as.factor(FSB)L | -0.2091 | 0.7888 | -0.2651 | 0.791 |
| tel_fsb | Habitat quality evaluation, all levels | as.factor(FSB)M | -21.86 | 3.657e+04 | -0.0005978 | 0.9995 |
| tel_fsb | Habitat quality evaluation, all levels | as.factor(FSB)moz. | 0.5288 | 0.3346 | 1.581 | 0.114 |
| tel_fsb | Habitat quality evaluation, all levels | as.factor(FSB)R | 1.501 | 1.276 | 1.176 | 0.2395 |
| tel_fsb | Habitat quality evaluation, all levels | as.factor(FSB)T | 0.9103 | 0.2067 | 4.403 | 1.067e-05 |
| tel_fsb | Habitat quality evaluation, all levels | as.factor(FSB)V | 0.1834 | 1.131 | 0.1622 | 0.8711 |
| tel_fsb | Habitat quality evaluation, all levels | as.factor(FSB)X | 0.6074 | 0.2156 | 2.817 | 0.004848 |
| tel_fsb_subset | Habitat quality evaluation, T / X / mosaic only | (Intercept) | -1.736 | 0.3752 | -4.627 | 3.707e-06 |
| tel_fsb_subset | Habitat quality evaluation, T / X / mosaic only | as.factor(FSB)T | 0.3672 | 0.3379 | 1.087 | 0.2771 |
| tel_fsb_subset | Habitat quality evaluation, T / X / mosaic only | as.factor(FSB)X | 0.04948 | 0.3447 | 0.1436 | 0.8858 |
| tel_het_inner | Within-site habitat heterogeneity | (Intercept) | -1.892 | 0.2644 | -7.157 | 8.219e-13 |
| tel_het_inner | Within-site habitat heterogeneity | HET_INN | -0.002579 | 0.1408 | -0.01832 | 0.9854 |
| tel_het_inner_dup_year | Within-site habitat heterogeneity, year term repeated | (Intercept) | -1.892 | 0.2644 | -7.157 | 8.231e-13 |
| tel_het_inner_dup_year | Within-site habitat heterogeneity, year term repeated | HET_INN | -0.002576 | 0.1408 | -0.0183 | 0.9854 |
| tel_het_outer | Between-habitat heterogeneity | (Intercept) | -1.852 | 0.3687 | -5.024 | 5.055e-07 |
| tel_het_outer | Between-habitat heterogeneity | HET_OUT | -0.04017 | 0.2818 | -0.1426 | 0.8866 |

Full table: [`10_teleius_coefficients.csv`](../Tables/10_teleius_coefficients.csv)

**Table - Random-effect variances**

| model | label | group | term | variance | sd |
|---|---|---|---|---|---|
| tel_null | Null model with year and site random effects | X:Y | (Intercept) | 0.4137 | 0.6432 |
| tel_null | Null model with year and site random effects | YEAR | (Intercept) | 0.0768 | 0.2771 |
| tel_random_only | Year and site random effects only | X:Y | (Intercept) | 0.4137 | 0.6432 |
| tel_random_only | Year and site random effects only | YEAR | (Intercept) | 0.0768 | 0.2771 |
| tel_area_null | Null model on records with a positive site area | X:Y | (Intercept) | 0.5681 | 0.7537 |
| tel_area_null | Null model on records with a positive site area | YEAR | (Intercept) | 0.1326 | 0.3641 |
| tel_area | Site area (selected habitat model) | X:Y | (Intercept) | 0.5673 | 0.7532 |
| tel_area | Site area (selected habitat model) | YEAR | (Intercept) | 0.1328 | 0.3645 |
| tel_plant_area_subset | Host plant abundance, records with a positive site area | X:Y | (Intercept) | 1576 | 39.7 |
| tel_plant_area_subset | Host plant abundance, records with a positive site area | YEAR | (Intercept) | 0 | 0 |
| tel_plant | Host plant abundance, all records | X:Y | (Intercept) | 1745 | 41.77 |
| tel_plant | Host plant abundance, all records | YEAR | (Intercept) | 0 | 0 |
| tel_plant_poly | Host plant abundance, quadratic | X:Y | (Intercept) | 1743 | 41.75 |
| tel_plant_poly | Host plant abundance, quadratic | YEAR | (Intercept) | 0 | 0 |
| tel_mow_null | Null model on records where mowing was assessed | X:Y | (Intercept) | 0.3756 | 0.6129 |
| tel_mow_null | Null model on records where mowing was assessed | YEAR | (Intercept) | 0.1611 | 0.4013 |
| tel_timing_mow_subset | Mowing timing, records where mowing was assessed | X:Y | (Intercept) | 0.4243 | 0.6514 |
| tel_timing_mow_subset | Mowing timing, records where mowing was assessed | YEAR | (Intercept) | 0.1455 | 0.3814 |
| tel_method_mow_subset | Mowing method, records where mowing was assessed | X:Y | (Intercept) | 0.3191 | 0.5649 |
| tel_method_mow_subset | Mowing method, records where mowing was assessed | YEAR | (Intercept) | 0.1791 | 0.4232 |
| tel_method_timing_mow_subset | Mowing method by timing, records where mowing was assessed | X:Y | (Intercept) | 0.3821 | 0.6182 |
| tel_method_timing_mow_subset | Mowing method by timing, records where mowing was assessed | YEAR | (Intercept) | 0.1595 | 0.3994 |
| tel_graze_null | Null model on records where grazing was assessed | X:Y | (Intercept) | 2.637e-08 | 0.0001624 |
| tel_graze_null | Null model on records where grazing was assessed | YEAR | (Intercept) | 0 | 0 |
| tel_graze | Grazing present | X:Y | (Intercept) | 1745 | 41.78 |
| tel_graze | Grazing present | YEAR | (Intercept) | 0 | 0 |
| tel_graze_method | Grazing intensity | X:Y | (Intercept) | 4e-14 | 2e-07 |
| tel_graze_method | Grazing intensity | YEAR | (Intercept) | 0 | 0 |
| tel_method | Mowing method, all records | X:Y | (Intercept) | 0.3228 | 0.5681 |
| tel_method | Mowing method, all records | YEAR | (Intercept) | 0.1591 | 0.3989 |
| tel_timing | Mowing timing, all records | X:Y | (Intercept) | 0.3066 | 0.5537 |
| tel_timing | Mowing timing, all records | YEAR | (Intercept) | 0.2142 | 0.4628 |
| tel_method_timing | Mowing method by timing, all records | X:Y | (Intercept) | 0.3821 | 0.6182 |
| tel_method_timing | Mowing method by timing, all records | YEAR | (Intercept) | 0.1595 | 0.3994 |
| tel_protect | Any protection | X:Y | (Intercept) | 0.1721 | 0.4149 |
| tel_protect | Any protection | YEAR | (Intercept) | 0.1771 | 0.4208 |
| tel_evl | Natura 2000 membership | X:Y | (Intercept) | 0.18 | 0.4242 |
| tel_evl | Natura 2000 membership | YEAR | (Intercept) | 0.1648 | 0.406 |
| tel_evl_target | Natura 2000 designated for Phengaris | X:Y | (Intercept) | 0.3549 | 0.5957 |
| tel_evl_target | Natura 2000 designated for Phengaris | YEAR | (Intercept) | 0.1608 | 0.4009 |
| tel_evl_combined | Natura 2000 membership and designation (selected protection model) | X:Y | (Intercept) | 0.1873 | 0.4328 |
| tel_evl_combined | Natura 2000 membership and designation (selected protection model) | YEAR | (Intercept) | 0.1792 | 0.4234 |
| tel_protection_combined | Natura 2000 membership, designation and small-scale protection | X:Y | (Intercept) | 0.2464 | 0.4964 |
| tel_protection_combined | Natura 2000 membership, designation and small-scale protection | YEAR | (Intercept) | 0.1738 | 0.4169 |
| tel_mzchu | Small-scale protected area | X:Y | (Intercept) | 0.3107 | 0.5574 |
| tel_mzchu | Small-scale protected area | YEAR | (Intercept) | 0.1046 | 0.3234 |
| tel_fsb | Habitat quality evaluation, all levels | X:Y | (Intercept) | 0.3332 | 0.5772 |
| tel_fsb | Habitat quality evaluation, all levels | YEAR | (Intercept) | 0.08431 | 0.2904 |
| tel_fsb_subset | Habitat quality evaluation, T / X / mosaic only | X:Y | (Intercept) | 0.2202 | 0.4692 |
| tel_fsb_subset | Habitat quality evaluation, T / X / mosaic only | YEAR | (Intercept) | 0.1847 | 0.4297 |
| tel_het_inner | Within-site habitat heterogeneity | X:Y | (Intercept) | 0.4137 | 0.6432 |
| tel_het_inner | Within-site habitat heterogeneity | YEAR | (Intercept) | 0.07683 | 0.2772 |
| tel_het_inner_dup_year | Within-site habitat heterogeneity, year term repeated | X.Y | (Intercept) | 0.4137 | 0.6432 |
| tel_het_inner_dup_year | Within-site habitat heterogeneity, year term repeated | YEAR | (Intercept) | 0.07476 | 0.2734 |
| tel_het_inner_dup_year | Within-site habitat heterogeneity, year term repeated | YEAR.1 | (Intercept) | 0.002069 | 0.04549 |
| tel_het_outer | Between-habitat heterogeneity | X:Y | (Intercept) | 0.4139 | 0.6434 |
| tel_het_outer | Between-habitat heterogeneity | YEAR | (Intercept) | 0.0768 | 0.2771 |

Full table: [`10_teleius_random_effects.csv`](../Tables/10_teleius_random_effects.csv)

**Table - Models that failed or warned during fitting**

| model | label | status | message |
|---|---|---|---|
| tel_resource_density | Site area by host plant abundance | failed | NA/NaN/Inf in foreign function call (arg 1) |
| tel_protection_combined | Natura 2000 membership, designation and small-scale protection | warning | Model failed to converge with max\|grad\| = 0.0669092 (tol = 0.002, component 1)   See ?lme4::convergence and ?lme4::troubleshooting. \| Model is nearly unidentifiable: very large eigenvalue  - Rescale variables? |
| tel_fsb | Habitat quality evaluation, all levels | warning | unable to evaluate scaled gradient \| Model failed to converge: degenerate  Hessian with 1 negative eigenvalues   See ?lme4::convergence and ?lme4::troubleshooting. |

Full table: [`10_teleius_fitting_issues.csv`](../Tables/10_teleius_fitting_issues.csv)

**Table - Model specifications as fitted**

| model | label | engine | formula |
|---|---|---|---|
| tel_null | Null model with year and site random effects | glmer | as.factor(POSITIVE) ~ 1 + (1 \| YEAR) + (1 \| X:Y) |
| tel_null_glm | Intercept-only model | glm | as.factor(POSITIVE) ~ 1 |
| tel_year_factor | Year as a factor | glm | as.factor(POSITIVE) ~ as.factor(YEAR) |
| tel_year_linear | Year as a linear trend | glm | as.factor(POSITIVE) ~ as.numeric(YEAR) |
| tel_year_poly | Year as a quadratic trend | glm | as.factor(POSITIVE) ~ poly(as.numeric(YEAR), 2) |
| tel_spatial | Spatial position | glm | as.factor(POSITIVE) ~ X:Y |
| tel_spatiotemporal | Year and spatial position, 2018 excluded | glm | as.factor(POSITIVE) ~ as.numeric(YEAR) + X:Y |
| tel_mapping_field | Mapping field as a fixed effect | glm | as.factor(POSITIVE) ~ as.factor(SITMAP) |
| tel_random_only | Year and site random effects only | glmer | as.factor(POSITIVE) ~ (1 \| YEAR) + (1 \| X:Y) |
| tel_area_null | Null model on records with a positive site area | glmer | as.factor(POSITIVE) ~ 1 + (1 \| YEAR) + (1 \| X:Y) |
| tel_area | Site area (selected habitat model) | glmer | as.factor(POSITIVE) ~ log10(AREA_SITE) + (1 \| YEAR) + (1 \| X:Y) |
| tel_area_null_glm | Intercept-only model on records with a positive site area | glm | as.factor(POSITIVE) ~ 1 |
| tel_plant_area_subset | Host plant abundance, records with a positive site area | glmer | as.factor(POSITIVE) ~ PLANT_QUANT + (1 \| YEAR) + (1 \| X:Y) |
| tel_plant | Host plant abundance, all records | glmer | as.factor(POSITIVE) ~ PLANT_QUANT + (1 \| YEAR) + (1 \| X:Y) |
| tel_plant_poly | Host plant abundance, quadratic | glmer | as.factor(POSITIVE) ~ poly(PLANT_QUANT, 2) + (1 \| YEAR) + (1 \|      X:Y) |
| tel_resource_density | Site area by host plant abundance | glmer | as.factor(POSITIVE) ~ log(AREA_SITE) * as.numeric(PLANT_QUANT) +      (1 \| YEAR) + (1 \| X:Y) |
| tel_mow_null | Null model on records where mowing was assessed | glmer | as.factor(POSITIVE) ~ 1 + (1 \| YEAR) + (1 \| X:Y) |
| tel_timing_mow_subset | Mowing timing, records where mowing was assessed | glmer | as.factor(POSITIVE) ~ as.factor(TIMING) + (1 \| YEAR) + (1 \| X:Y) |
| tel_method_mow_subset | Mowing method, records where mowing was assessed | glmer | as.factor(POSITIVE) ~ as.factor(METHOD) + (1 \| YEAR) + (1 \| X:Y) |
| tel_method_timing_mow_subset | Mowing method by timing, records where mowing was assessed | glmer | as.factor(POSITIVE) ~ as.factor(METHOD) * as.factor(TIMING) +      (1 \| YEAR) + (1 \| X:Y) |
| tel_mow_null_glm | Intercept-only model on records where mowing was assessed | glm | as.factor(POSITIVE) ~ 1 |
| tel_graze_null | Null model on records where grazing was assessed | glmer | as.factor(POSITIVE) ~ 1 + (1 \| YEAR) + (1 \| X:Y) |
| tel_graze | Grazing present | glmer | as.factor(POSITIVE) ~ as.factor(GRAZE) + (1 \| YEAR) + (1 \| X:Y) |
| tel_graze_method | Grazing intensity | glmer | as.factor(POSITIVE) ~ as.factor(GRAZE_MET) + (1 \| YEAR) + (1 \|      X:Y) |
| tel_method | Mowing method, all records | glmer | as.factor(POSITIVE) ~ as.factor(METHOD) + (1 \| YEAR) + (1 \| X:Y) |
| tel_timing | Mowing timing, all records | glmer | as.factor(POSITIVE) ~ as.factor(TIMING) + (1 \| YEAR) + (1 \| X:Y) |
| tel_method_timing | Mowing method by timing, all records | glmer | as.factor(POSITIVE) ~ as.factor(METHOD) * as.factor(TIMING) +      (1 \| YEAR) + (1 \| X:Y) |
| tel_protect | Any protection | glmer | as.factor(POSITIVE) ~ as.factor(PROTECT) + (1 \| YEAR) + (1 \|      X:Y) |
| tel_evl | Natura 2000 membership | glmer | as.factor(POSITIVE) ~ as.factor(EVL) + (1 \| YEAR) + (1 \| X:Y) |
| tel_evl_target | Natura 2000 designated for Phengaris | glmer | as.factor(POSITIVE) ~ as.factor(EVL_target) + (1 \| YEAR) + (1 \|      X:Y) |
| tel_evl_combined | Natura 2000 membership and designation (selected protection model) | glmer | as.factor(POSITIVE) ~ as.factor(EVL) + as.factor(EVL_target) +      (1 \| YEAR) + (1 \| X:Y) |
| tel_protection_combined | Natura 2000 membership, designation and small-scale protection | glmer | as.factor(POSITIVE) ~ as.factor(EVL) + as.factor(EVL_target) +      as.factor(MZCHU) + (1 \| YEAR) + (1 \| X:Y) |
| tel_mzchu | Small-scale protected area | glmer | as.factor(POSITIVE) ~ as.factor(MZCHU) + (1 \| YEAR) + (1 \| X:Y) |
| tel_fsb | Habitat quality evaluation, all levels | glmer | as.factor(POSITIVE) ~ as.factor(FSB) + (1 \| YEAR) + (1 \| X:Y) |
| tel_fsb_subset | Habitat quality evaluation, T / X / mosaic only | glmer | as.factor(POSITIVE) ~ as.factor(FSB) + (1 \| YEAR) + (1 \| X:Y) |
| tel_het_inner | Within-site habitat heterogeneity | glmer | as.factor(POSITIVE) ~ HET_INN + (1 \| YEAR) + (1 \| X:Y) |
| tel_het_inner_dup_year | Within-site habitat heterogeneity, year term repeated | glmer | as.factor(POSITIVE) ~ HET_INN + (1 \| YEAR) + (1 \| YEAR) + (1 \|      X:Y) |
| tel_het_outer | Between-habitat heterogeneity | glmer | as.factor(POSITIVE) ~ HET_OUT + (1 \| YEAR) + (1 \| X:Y) |

Full table: [`10_teleius_specifications.csv`](../Tables/10_teleius_specifications.csv)

> **Note.** `tel_resource_density` cannot be fitted. 611 of the 1324 P. teleius records have AREA_SITE = 0, and the formula takes log(AREA_SITE), so the model matrix contains -Inf. The equivalent P. nausithous model (`nau_resource_density`) runs only because the original restricted it to AREA_SITE > 0 while leaving the P. teleius version unrestricted. Applying the same filter here would make the two species comparable and the model fittable, but it changes the specification, so it is left as written for you to decide.

## Model comparisons

**Table - Management models, all records**

| model | label | n_obs | df | AIC | BIC | logLik | delta_AIC |
|---|---|---|---|---|---|---|---|
| tel_method_timing | Mowing method by timing, all records | 698 | 6 | 611 | 638.3 | -299.5 | 0 |
| tel_method | Mowing method, all records | 706 | 4 | 624 | 642.3 | -308 | 13.01 |
| tel_timing | Mowing timing, all records | 754 | 4 | 689.6 | 708.1 | -340.8 | 78.53 |

Full table: [`10_teleius_aic_management.csv`](../Tables/10_teleius_aic_management.csv)

**Table - Management models, records where mowing was assessed**

| model | label | n_obs | df | AIC | BIC | logLik | delta_AIC |
|---|---|---|---|---|---|---|---|
| tel_timing_mow_subset | Mowing timing, records where mowing was assessed | 698 | 4 | 608 | 626.2 | -300 | 0 |
| tel_method_timing_mow_subset | Mowing method by timing, records where mowing was assessed | 698 | 6 | 611 | 638.3 | -299.5 | 3.025 |
| tel_method_mow_subset | Mowing method, records where mowing was assessed | 698 | 4 | 614.4 | 632.6 | -303.2 | 6.422 |
| tel_mow_null | Null model on records where mowing was assessed | 698 | 3 | 616.1 | 629.7 | -305 | 8.079 |

Full table: [`10_teleius_aic_management_subset.csv`](../Tables/10_teleius_aic_management_subset.csv)

**Table - Natura 2000 and protection models**

| model | label | n_obs | df | AIC | BIC | logLik | delta_AIC |
|---|---|---|---|---|---|---|---|
| tel_evl | Natura 2000 membership | 1324 | 4 | 1059 | 1080 | -525.5 | 0 |
| tel_evl_combined | Natura 2000 membership and designation (selected protection model) | 1324 | 5 | 1060 | 1086 | -525 | 0.9749 |
| tel_protection_combined | Natura 2000 membership, designation and small-scale protection | 1324 | 6 | 1060 | 1091 | -524 | 1.15 |
| tel_evl_target | Natura 2000 designated for Phengaris | 1324 | 4 | 1097 | 1118 | -544.6 | 38.32 |

Full table: [`10_teleius_aic_protection.csv`](../Tables/10_teleius_aic_protection.csv)

**Table - Habitat extent and host plant models**

| model | label | n_obs | df | AIC | BIC | logLik | delta_AIC |
|---|---|---|---|---|---|---|---|
| tel_area_null | Null model on records with a positive site area | 713 | 3 | 583.8 | 597.5 | -288.9 | 0 |
| tel_area | Site area (selected habitat model) | 713 | 4 | 585.7 | 604 | -288.9 | 1.938 |
| tel_plant | Host plant abundance, all records | 1324 | 4 | 648.5 | 669.2 | -320.2 | 64.69 |
| tel_plant_poly | Host plant abundance, quadratic | 1324 | 5 | 650.4 | 676.4 | -320.2 | 66.63 |

Full table: [`10_teleius_aic_habitat.csv`](../Tables/10_teleius_aic_habitat.csv)

29 occupied P. teleius records combine appropriate mowing method with appropriate timing inside a Natura 2000 site.


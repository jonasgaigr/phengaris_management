# c04_flight_curve

Snapshot `22_phase4_base_all` compared with `23_flight_curve_all`.

254 files compared: 179 identical, 1 numerically identical (differences below 1e-9 relative), 50 changed, 24 added, 0 removed.

## Files that differ

| file | kind | status | rows_before | rows_after | rows_removed | rows_added | cols_removed | cols_added | cells_changed |
|---|---|---|---|---|---|---|---|---|---|
| revised__data_analysis.csv | processed | changed | 6576 | 6576 | 0 | 0 |  | ELEVATION, FLIGHT_ACTIVITY, LOG_ACTIVITY, IN_WINDOW_28, IN_WINDOW_45 | 12 |
| revised__data_clean.csv | processed | changed | 6576 | 6576 | 0 | 0 |  | ELEVATION, FLIGHT_ACTIVITY, LOG_ACTIVITY, IN_WINDOW_28, IN_WINDOW_45 | 12 |
| site_design.csv | processed | changed | 8678 | 8678 | 0 | 0 |  | ELEVATION | 0 |
| flight_curve.csv | processed | added |  |  |  |  |  |  |  |
| 16_site_geometries.csv | tables | changed | 3 | 3 | 0 | 0 |  | min_elevation, median_elevation, max_elevation | 0 |
| revised__04_design_by_era.csv | tables | changed | 4 | 4 | 0 | 0 |  | positive_outside, outside_28_days, outside_45_days, median_activity | 2 |
| revised__04_outside_flight_window.csv | tables | changed | 6 | 18 | 0 | 12 |  | ELEVATION, FLIGHT_ACTIVITY | 0 |
| revised__17_both_species_coefficients.csv | tables | changed | 10 | 10 | 0 | 0 |  |  | 87 |
| revised__17_both_species_diagnostics.csv | tables | changed | 2 | 2 | 0 | 0 |  |  | 6 |
| revised__17_both_species_fit_statistics.csv | tables | changed | 2 | 2 | 0 | 0 |  |  | 8 |
| revised__17_both_species_gvif.csv | tables | changed | 6 | 6 | 0 | 0 |  |  | 12 |
| revised__17_both_species_random_effects.csv | tables | changed | 6 | 6 | 0 | 0 |  |  | 12 |
| revised__17_cooccurrence_coefficients.csv | tables | changed | 10 | 10 | 0 | 0 |  |  | 90 |
| revised__17_cooccurrence_counts.csv | tables | changed | 8 | 8 | 0 | 0 |  |  | 2 |
| revised__17_cooccurrence_diagnostics.csv | tables | changed | 2 | 2 | 0 | 0 |  |  | 6 |
| revised__17_cooccurrence_estimates.csv | tables | changed | 2 | 2 | 2 | 2 |  |  | 0 |
| revised__17_cooccurrence_fit_statistics.csv | tables | changed | 2 | 2 | 0 | 0 |  |  | 8 |
| revised__17_cooccurrence_gvif.csv | tables | changed | 6 | 6 | 0 | 0 |  |  | 12 |
| revised__17_cooccurrence_random_effects.csv | tables | changed | 6 | 6 | 0 | 0 |  |  | 12 |
| revised__17_nausithous_coefficients.csv | tables | changed | 184 | 190 | 0 | 6 |  |  | 0 |
| revised__17_nausithous_diagnostics.csv | tables | changed | 20 | 22 | 0 | 2 |  |  | 0 |
| revised__17_nausithous_fit_statistics.csv | tables | changed | 20 | 22 | 0 | 2 |  |  | 0 |
| revised__17_nausithous_gvif.csv | tables | changed | 112 | 116 | 0 | 4 |  |  | 0 |
| revised__17_nausithous_random_effects.csv | tables | changed | 66 | 72 | 0 | 6 |  |  | 0 |
| revised__17_nausithous_specifications.csv | tables | changed | 20 | 22 | 0 | 2 |  |  | 0 |
| revised__17_records.csv | tables | changed | 4 | 4 | 0 | 0 |  |  | 7 |
| revised__17_records_by_year.csv | tables | changed | 12 | 12 | 0 | 0 |  |  | 6 |
| revised__17_species_estimates.csv | tables | numerically identical | 2 | 2 | 0 | 0 |  |  | 0 |
| revised__17_species_proportions.csv | tables | changed | 2 | 2 | 0 | 0 |  |  | 6 |
| revised__17_teleius_aic_management.csv | tables | changed | 5 | 5 | 0 | 0 |  |  | 19 |
| revised__17_teleius_aic_management_obs.csv | tables | changed | 5 | 5 | 0 | 0 |  |  | 19 |
| revised__17_teleius_aic_site.csv | tables | changed | 4 | 4 | 0 | 0 |  |  | 15 |
| revised__17_teleius_aic_site_obs.csv | tables | changed | 4 | 4 | 1 | 1 |  |  | 12 |
| revised__17_teleius_bias_reduced.csv | tables | changed | 28 | 28 | 0 | 0 |  |  | 168 |
| revised__17_teleius_coefficients.csv | tables | changed | 184 | 190 | 0 | 6 |  |  | 1641 |
| revised__17_teleius_diagnostics.csv | tables | changed | 20 | 22 | 3 | 5 |  |  | 48 |
| revised__17_teleius_fit_statistics.csv | tables | changed | 20 | 22 | 1 | 3 |  |  | 74 |
| revised__17_teleius_gvif.csv | tables | changed | 112 | 116 | 0 | 4 |  |  | 224 |
| revised__17_teleius_imputation_sensitivity.csv | tables | changed | 6 | 6 | 6 | 6 |  |  | 0 |
| revised__17_teleius_key_estimates.csv | tables | changed | 42 | 42 | 40 | 40 |  |  | 2 |
| revised__17_teleius_predictions_management.csv | tables | changed | 8 | 8 | 0 | 0 |  |  | 24 |
| revised__17_teleius_predictions_plant_protection.csv | tables | changed | 10 | 10 | 0 | 0 |  |  | 30 |
| revised__17_teleius_random_effects.csv | tables | changed | 66 | 72 | 0 | 6 |  |  | 153 |
| revised__17_teleius_specifications.csv | tables | changed | 20 | 22 | 0 | 2 |  |  | 0 |
| revised__18_moran.csv | tables | changed | 8 | 8 | 0 | 0 |  |  | 20 |
| revised__19_teleius_coefficients.csv | tables | changed | 75 | 75 | 0 | 0 |  |  | 667 |
| revised__19_teleius_diagnostics.csv | tables | changed | 15 | 15 | 3 | 3 |  |  | 36 |
| revised__19_teleius_fit_statistics.csv | tables | changed | 15 | 15 | 0 | 0 |  |  | 60 |
| revised__19_teleius_gvif.csv | tables | changed | 45 | 45 | 0 | 0 |  |  | 90 |
| revised__19_teleius_random_effects.csv | tables | changed | 45 | 45 | 0 | 0 |  |  | 103 |
| revised__19_threat_counts.csv | tables | changed | 52 | 52 | 0 | 0 |  |  | 7 |
| revised__19_threat_estimates.csv | tables | changed | 41 | 41 | 7 | 7 |  |  | 12 |
| 16_flight_curve_models.csv | tables | added |  |  |  |  |  |  |  |
| 16_flight_curve_visits.csv | tables | added |  |  |  |  |  |  |  |
| 16_flight_season.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_nausithous_aic_season.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_nausithous_aic_season_obs.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_nausithous_window_coefficients.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_nausithous_window_diagnostics.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_nausithous_window_fit_statistics.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_nausithous_window_gvif.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_nausithous_window_random_effects.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_nausithous_window_sensitivity.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_nausithous_window_specifications.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_teleius_aic_season.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_teleius_aic_season_obs.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_teleius_fitting_issues.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_teleius_window_bias_reduced.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_teleius_window_coefficients.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_teleius_window_diagnostics.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_teleius_window_fit_statistics.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_teleius_window_gvif.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_teleius_window_random_effects.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_teleius_window_sensitivity.csv | tables | added |  |  |  |  |  |  |  |
| revised__17_teleius_window_specifications.csv | tables | added |  |  |  |  |  |  |  |

## Changed numbers

The full list of 3724 changed cells is in `c04_flight_curve.csv`.

### revised__data_analysis.csv

| row | column | before | after |
|---|---|---|---|
| 50258153 \| Phengaris teleius \| FALSE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 54720520 \| Phengaris teleius \| FALSE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 48053850 \| Phengaris teleius \| TRUE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 48053851 \| Phengaris teleius \| TRUE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 48054049 \| Phengaris teleius \| TRUE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 48054059 \| Phengaris teleius \| TRUE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 48054060 \| Phengaris teleius \| TRUE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 54720508 \| Phengaris teleius \| TRUE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 54788826 \| Phengaris teleius \| TRUE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 57615863 \| Phengaris teleius \| TRUE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 65296630 \| Phengaris teleius \| TRUE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 50257822 \| Phengaris teleius \| FALSE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |

### revised__data_clean.csv

| row | column | before | after |
|---|---|---|---|
| 50258153 \| Phengaris teleius \| FALSE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 54720520 \| Phengaris teleius \| FALSE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 48053850 \| Phengaris teleius \| TRUE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 48053851 \| Phengaris teleius \| TRUE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 48054049 \| Phengaris teleius \| TRUE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 48054059 \| Phengaris teleius \| TRUE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 48054060 \| Phengaris teleius \| TRUE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 54720508 \| Phengaris teleius \| TRUE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 54788826 \| Phengaris teleius \| TRUE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 57615863 \| Phengaris teleius \| TRUE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 65296630 \| Phengaris teleius \| TRUE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |
| 50257822 \| Phengaris teleius \| FALSE #1 | IN_FLIGHT_WINDOW | TRUE | FALSE |

### revised__04_design_by_era.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris teleius \| 2019-2020 #1 | outside_flight_window | 0 | 7 |
| Phengaris teleius \| 2021-2024 #1 | outside_flight_window | 2 | 7 |

### revised__17_both_species_coefficients.csv

| row | column | before | after |
|---|---|---|---|
| both_species \| with imputed \| (Intercept) #1 | estimate | 0.297655048944376 | 0.297143426405472 |
| both_species \| with imputed \| DRUHPhengaris teleius #1 | estimate | -2.77141327526845 | -2.77074493376983 |
| both_species \| with imputed \| ERA2021-2024 #1 | estimate | -0.553066214596305 | -0.553121568305117 |
| both_species \| with imputed \| DOY_STD #1 | estimate | 0.023201269342459 | 0.0224555224660513 |
| both_species \| with imputed \| I(DOY_STD^2) #1 | estimate | -0.271874426661732 | -0.270655809481441 |
| both_species_obs \| observed only \| (Intercept) #1 | estimate | 0.54311723440772 | 0.542188393675108 |
| both_species_obs \| observed only \| DRUHPhengaris teleius #1 | estimate | -1.25509491659895 | -1.25432261382872 |
| both_species_obs \| observed only \| ERA2021-2024 #1 | estimate | -0.263151798054372 | -0.262976658787712 |
| both_species_obs \| observed only \| DOY_STD #1 | estimate | -0.111326504652549 | -0.112062286615075 |
| both_species_obs \| observed only \| I(DOY_STD^2) #1 | estimate | -0.211466994382426 | -0.210250569438541 |
| both_species \| with imputed \| (Intercept) #1 | std_error | 0.371747386541634 | 0.3718344520598 |
| both_species \| with imputed \| DRUHPhengaris teleius #1 | std_error | 0.101164841062013 | 0.101170291594379 |
| both_species \| with imputed \| ERA2021-2024 #1 | std_error | 0.414647548565157 | 0.414730197263594 |
| both_species \| with imputed \| DOY_STD #1 | std_error | 0.0710193039158083 | 0.071052516197893 |
| both_species \| with imputed \| I(DOY_STD^2) #1 | std_error | 0.0437713667256334 | 0.0439184370643103 |
| both_species_obs \| observed only \| (Intercept) #1 | std_error | 0.520059784288367 | 0.520049440756114 |
| both_species_obs \| observed only \| DRUHPhengaris teleius #1 | std_error | 0.130319226752736 | 0.130341631301914 |
| both_species_obs \| observed only \| ERA2021-2024 #1 | std_error | 0.566979438880353 | 0.566946632656419 |
| both_species_obs \| observed only \| DOY_STD #1 | std_error | 0.0842067232866663 | 0.0842551543617298 |
| both_species_obs \| observed only \| I(DOY_STD^2) #1 | std_error | 0.0515440526145058 | 0.0517737464987685 |
| both_species \| with imputed \| (Intercept) #1 | z_value | 0.800691705497813 | 0.799128280769648 |
| both_species \| with imputed \| DRUHPhengaris teleius #1 | z_value | -27.3950242611423 | -27.386942254536 |
| both_species \| with imputed \| ERA2021-2024 #1 | z_value | -1.33382246322239 | -1.33369012421723 |
| both_species \| with imputed \| DOY_STD #1 | z_value | 0.326689619064185 | 0.3160411997727 |
| both_species \| with imputed \| I(DOY_STD^2) #1 | z_value | -6.21123915014782 | -6.16269219883933 |
| both_species_obs \| observed only \| (Intercept) #1 | z_value | 1.04433615291154 | 1.04257086189114 |
| both_species_obs \| observed only \| DRUHPhengaris teleius #1 | z_value | -9.63092667040092 | -9.62334598163271 |
| both_species_obs \| observed only \| ERA2021-2024 #1 | z_value | -0.464129349335901 | -0.463847289392194 |
| both_species_obs \| observed only \| DOY_STD #1 | z_value | -1.32206194835011 | -1.33003479091572 |
| both_species_obs \| observed only \| I(DOY_STD^2) #1 | z_value | -4.10264586612877 | -4.06094948998026 |
| both_species \| with imputed \| (Intercept) #1 | p_value | 0.423310145582701 | 0.42421603264656 |
| both_species \| with imputed \| ERA2021-2024 #1 | p_value | 0.182262047306283 | 0.182305432688701 |
| both_species \| with imputed \| DOY_STD #1 | p_value | 0.743902652616014 | 0.75197123664163 |
| both_species_obs \| observed only \| (Intercept) #1 | p_value | 0.296329891874125 | 0.297147090259947 |
| both_species_obs \| observed only \| ERA2021-2024 #1 | p_value | 0.642555075389093 | 0.642757159839062 |
| both_species_obs \| observed only \| DOY_STD #1 | p_value | 0.186147523384988 | 0.18350680867466 |
| both_species_obs \| observed only \| I(DOY_STD^2) #1 | p_value | 4.08452183737394e-05 | 4.88735453000748e-05 |
| both_species \| with imputed \| (Intercept) #1 | conf_low | -0.430956440024116 | -0.431638707842921 |
| both_species \| with imputed \| DRUHPhengaris teleius #1 | conf_low | -2.96969272025171 | -2.96903506160023 |
| both_species \| with imputed \| ERA2021-2024 #1 | conf_low | -1.36576047606184 | -1.36597781824295 |
| both_species \| with imputed \| DOY_STD #1 | conf_low | -0.11599400853963 | -0.116804850292768 |
| both_species \| with imputed \| I(DOY_STD^2) #1 | conf_low | -0.357664728998068 | -0.356734364384778 |
| both_species_obs \| observed only \| (Intercept) #1 | conf_low | -0.476181212605148 | -0.477089780387072 |
| both_species_obs \| observed only \| DRUHPhengaris teleius #1 | conf_low | -1.51051590752742 | -1.50978751686667 |
| both_species_obs \| observed only \| ERA2021-2024 #1 | conf_low | -1.37441107823459 | -1.37417163995055 |
| both_species_obs \| observed only \| DOY_STD #1 | conf_low | -0.276368649550545 | -0.277199354675928 |
| both_species_obs \| observed only \| I(DOY_STD^2) #1 | conf_low | -0.312491481124095 | -0.311725247920834 |
| both_species \| with imputed \| (Intercept) #1 | conf_high | 1.02626653791287 | 1.02592556065387 |
| both_species \| with imputed \| DRUHPhengaris teleius #1 | conf_high | -2.57313383028519 | -2.57245480593944 |
| both_species \| with imputed \| ERA2021-2024 #1 | conf_high | 0.259628046869226 | 0.259734681632719 |
| both_species \| with imputed \| DOY_STD #1 | conf_high | 0.162396547224548 | 0.16171589522487 |
| both_species \| with imputed \| I(DOY_STD^2) #1 | conf_high | -0.186084124325395 | -0.184577254578104 |
| both_species_obs \| observed only \| (Intercept) #1 | conf_high | 1.56241568142059 | 1.56146656773729 |
| both_species_obs \| observed only \| DRUHPhengaris teleius #1 | conf_high | -0.99967392567048 | -0.998857710790775 |
| both_species_obs \| observed only \| ERA2021-2024 #1 | conf_high | 0.848107482125847 | 0.848218322375129 |
| both_species_obs \| observed only \| DOY_STD #1 | conf_high | 0.0537156402454476 | 0.0530747814457785 |
| both_species_obs \| observed only \| I(DOY_STD^2) #1 | conf_high | -0.110442507640757 | -0.108775890956248 |
| both_species \| with imputed \| (Intercept) #1 | ratio | 1.34669716313973 | 1.34600833874206 |
| both_species \| with imputed \| DRUHPhengaris teleius #1 | ratio | 0.0625735086299927 | 0.0626153430808166 |
| both_species \| with imputed \| ERA2021-2024 #1 | ratio | 0.575183467827103 | 0.575151630170086 |

_Showing the first 60 of 87 rows._

### revised__17_both_species_diagnostics.csv

| row | column | before | after |
|---|---|---|---|
| both_species \| with imputed \| TRUE \| FALSE #1 | max_gvif_adjusted | 1.04225248970724 | 1.0410258426497 |
| both_species_obs \| observed only \| TRUE \| FALSE #1 | max_gvif_adjusted | 1.00822375764272 | 1.00776444857451 |
| both_species \| with imputed \| TRUE \| FALSE #1 | dharma_uniformity_p | 0.38799675020277 | 0.497599076165653 |
| both_species_obs \| observed only \| TRUE \| FALSE #1 | dharma_uniformity_p | 0.334659099616972 | 0.0947546623314297 |
| both_species \| with imputed \| TRUE \| FALSE #1 | dharma_dispersion_p | 0.352 | 0.344 |
| both_species_obs \| observed only \| TRUE \| FALSE #1 | dharma_dispersion_p | 0.536 | 0.56 |

### revised__17_both_species_fit_statistics.csv

| row | column | before | after |
|---|---|---|---|
| both_species \| Species as a fixed effect \| Species \| with imputed \| ok #1 | n_obs | 6570 | 6558 |
| both_species_obs \| Species as a fixed effect \| Species \| observed only \| ok #1 | n_obs | 4430 | 4427 |
| both_species \| Species as a fixed effect \| Species \| with imputed \| ok #1 | AIC | 5542.72430544136 | 5542.52777614575 |
| both_species_obs \| Species as a fixed effect \| Species \| observed only \| ok #1 | AIC | 4335.8459562149 | 4335.73828669801 |
| both_species \| Species as a fixed effect \| Species \| with imputed \| ok #1 | BIC | 5597.04645833319 | 5596.83530380499 |
| both_species_obs \| Species as a fixed effect \| Species \| observed only \| ok #1 | BIC | 4387.01519511921 | 4386.90210615987 |
| both_species \| Species as a fixed effect \| Species \| with imputed \| ok #1 | logLik | -2763.36215272068 | -2763.26388807288 |
| both_species_obs \| Species as a fixed effect \| Species \| observed only \| ok #1 | logLik | -2159.92297810745 | -2159.86914334901 |

### revised__17_both_species_gvif.csv

| row | column | before | after |
|---|---|---|---|
| both_species \| with imputed \| DRUH #1 | GVIF | 1.08629025230094 | 1.08373480506451 |
| both_species \| with imputed \| ERA #1 | GVIF | 1.00284598904455 | 1.00274712322785 |
| both_species \| with imputed \| DOY_STD #1 | GVIF | 1.08543978420615 | 1.08284861892554 |
| both_species_obs \| observed only \| DRUH #1 | GVIF | 1.01651514547521 | 1.01558918381069 |
| both_species_obs \| observed only \| ERA #1 | GVIF | 1.00758920363991 | 1.00749498589226 |
| both_species_obs \| observed only \| DOY_STD #1 | GVIF | 1.01288278982559 | 1.01196321761056 |
| both_species \| with imputed \| DRUH #1 | GVIF_adjusted | 1.04225248970724 | 1.0410258426497 |
| both_species \| with imputed \| ERA #1 | GVIF_adjusted | 1.00142198350373 | 1.00137261957168 |
| both_species \| with imputed \| DOY_STD #1 | GVIF_adjusted | 1.04184441458701 | 1.04060012441165 |
| both_species_obs \| observed only \| DRUH #1 | GVIF_adjusted | 1.00822375764272 | 1.00776444857451 |
| both_species_obs \| observed only \| ERA #1 | GVIF_adjusted | 1.00378742950881 | 1.00374049728615 |
| both_species_obs \| observed only \| DOY_STD #1 | GVIF_adjusted | 1.00642078169401 | 1.00596382519977 |

### revised__17_both_species_random_effects.csv

| row | column | before | after |
|---|---|---|---|
| both_species \| with imputed \| SITMAP #1 | variance | 0.88628628557219 | 0.886116797099873 |
| both_species \| with imputed \| YEAR #1 | variance | 0.20900865864383 | 0.209098881432347 |
| both_species \| with imputed \| OBSERVER #1 | variance | 0.972872338151862 | 0.973393448141129 |
| both_species_obs \| observed only \| SITMAP #1 | variance | 1.66201244141968 | 1.66141303032139 |
| both_species_obs \| observed only \| YEAR #1 | variance | 0.399309782186017 | 0.399265129367437 |
| both_species_obs \| observed only \| OBSERVER #1 | variance | 2.3520704781701 | 2.35222041669987 |
| both_species \| with imputed \| SITMAP #1 | sd | 0.941427790949571 | 0.94133776993164 |
| both_species \| with imputed \| YEAR #1 | sd | 0.45717464785772 | 0.457273311524243 |
| both_species \| with imputed \| OBSERVER #1 | sd | 0.986342911036452 | 0.986607038359817 |
| both_species_obs \| observed only \| SITMAP #1 | sd | 1.28919061485092 | 1.28895811814092 |
| both_species_obs \| observed only \| YEAR #1 | sd | 0.631909631344559 | 0.631874298707771 |
| both_species_obs \| observed only \| OBSERVER #1 | sd | 1.53364613851113 | 1.53369502075865 |

### revised__17_cooccurrence_coefficients.csv

| row | column | before | after |
|---|---|---|---|
| nau_cooccurrence \| observed only \| (Intercept) #1 | estimate | 0.0266142066288547 | 0.0221597519573345 |
| nau_cooccurrence \| observed only \| OTHER_POSITIVE #1 | estimate | 1.45294153848851 | 1.45200965343134 |
| nau_cooccurrence \| observed only \| ERA2021-2024 #1 | estimate | -1.59327884696324 | -1.59084792021746 |
| nau_cooccurrence \| observed only \| DOY_STD #1 | estimate | 0.108092094211457 | 0.106651871778949 |
| nau_cooccurrence \| observed only \| I(DOY_STD^2) #1 | estimate | -0.422119833284584 | -0.413058172346729 |
| tel_cooccurrence \| observed only \| (Intercept) #1 | estimate | -2.31964611258576 | -2.3171419996426 |
| tel_cooccurrence \| observed only \| OTHER_POSITIVE #1 | estimate | 1.20018166922638 | 1.19980257941039 |
| tel_cooccurrence \| observed only \| ERA2021-2024 #1 | estimate | -1.2178637778166 | -1.21705678945153 |
| tel_cooccurrence \| observed only \| DOY_STD #1 | estimate | -0.770384059593194 | -0.77516300884542 |
| tel_cooccurrence \| observed only \| I(DOY_STD^2) #1 | estimate | 0.0406743842012828 | 0.0459853952778564 |
| nau_cooccurrence \| observed only \| (Intercept) #1 | std_error | 0.865687382576799 | 0.865275974274931 |
| nau_cooccurrence \| observed only \| OTHER_POSITIVE #1 | std_error | 0.463724460244024 | 0.463477057170839 |
| nau_cooccurrence \| observed only \| ERA2021-2024 #1 | std_error | 1.10183466178118 | 1.10111149233029 |
| nau_cooccurrence \| observed only \| DOY_STD #1 | std_error | 0.244527220597458 | 0.244386517907639 |
| nau_cooccurrence \| observed only \| I(DOY_STD^2) #1 | std_error | 0.195703730733565 | 0.201082703905352 |
| tel_cooccurrence \| observed only \| (Intercept) #1 | std_error | 1.70233420563411 | 1.70156691116171 |
| tel_cooccurrence \| observed only \| OTHER_POSITIVE #1 | std_error | 0.474649943620997 | 0.474419857568699 |
| tel_cooccurrence \| observed only \| ERA2021-2024 #1 | std_error | 2.2648263812865 | 2.26540886838715 |
| tel_cooccurrence \| observed only \| DOY_STD #1 | std_error | 0.54371374667351 | 0.546188296104165 |
| tel_cooccurrence \| observed only \| I(DOY_STD^2) #1 | std_error | 0.308064012681336 | 0.313857951218814 |
| nau_cooccurrence \| observed only \| (Intercept) #1 | z_value | 0.0307434382948207 | 0.0256100395898587 |
| nau_cooccurrence \| observed only \| OTHER_POSITIVE #1 | z_value | 3.13320012863658 | 3.13286198521824 |
| nau_cooccurrence \| observed only \| ERA2021-2024 #1 | z_value | -1.44602352987117 | -1.44476552220042 |
| nau_cooccurrence \| observed only \| DOY_STD #1 | z_value | 0.442045241210177 | 0.436406528036281 |
| nau_cooccurrence \| observed only \| I(DOY_STD^2) #1 | z_value | -2.15693299101827 | -2.05417056924574 |
| tel_cooccurrence \| observed only \| (Intercept) #1 | z_value | -1.36262674209833 | -1.36176954573042 |
| tel_cooccurrence \| observed only \| OTHER_POSITIVE #1 | z_value | 2.52856170185225 | 2.52898895412835 |
| tel_cooccurrence \| observed only \| ERA2021-2024 #1 | z_value | -0.537729420621112 | -0.537234936454545 |
| tel_cooccurrence \| observed only \| DOY_STD #1 | z_value | -1.41689273870023 | -1.41922303054547 |
| tel_cooccurrence \| observed only \| I(DOY_STD^2) #1 | z_value | 0.132032248256653 | 0.146516585287325 |
| nau_cooccurrence \| observed only \| (Intercept) #1 | p_value | 0.975474148770908 | 0.979568378262793 |
| nau_cooccurrence \| observed only \| OTHER_POSITIVE #1 | p_value | 0.00172911501561771 | 0.00173110820688795 |
| nau_cooccurrence \| observed only \| ERA2021-2024 #1 | p_value | 0.148170602246399 | 0.148523758493745 |
| nau_cooccurrence \| observed only \| DOY_STD #1 | p_value | 0.658456468301356 | 0.66254180344763 |
| nau_cooccurrence \| observed only \| I(DOY_STD^2) #1 | p_value | 0.03101088593538 | 0.0399591858277801 |
| tel_cooccurrence \| observed only \| (Intercept) #1 | p_value | 0.17300017637815 | 0.173270625450516 |
| tel_cooccurrence \| observed only \| OTHER_POSITIVE #1 | p_value | 0.0114530953177865 | 0.011439162723498 |
| tel_cooccurrence \| observed only \| ERA2021-2024 #1 | p_value | 0.590763864732904 | 0.591105341698294 |
| tel_cooccurrence \| observed only \| DOY_STD #1 | p_value | 0.156514289820687 | 0.155834003623262 |
| tel_cooccurrence \| observed only \| I(DOY_STD^2) #1 | p_value | 0.894958784606503 | 0.883513598037046 |
| nau_cooccurrence \| observed only \| (Intercept) #1 | conf_low | -1.67010188509242 | -1.67374999430934 |
| nau_cooccurrence \| observed only \| OTHER_POSITIVE #1 | conf_low | 0.544058297659943 | 0.543611313715884 |
| nau_cooccurrence \| observed only \| ERA2021-2024 #1 | conf_low | -3.75283510097222 | -3.74898678814799 |
| nau_cooccurrence \| observed only \| DOY_STD #1 | conf_low | -0.37117245139924 | -0.372336901627176 |
| nau_cooccurrence \| observed only \| I(DOY_STD^2) #1 | conf_low | -0.805692097162495 | -0.807173029915152 |
| tel_cooccurrence \| observed only \| (Intercept) #1 | conf_low | -5.65615984527921 | -5.65215186280461 |
| tel_cooccurrence \| observed only \| OTHER_POSITIVE #1 | conf_low | 0.269884874465263 | 0.269956745025113 |
| tel_cooccurrence \| observed only \| ERA2021-2024 #1 | conf_low | -5.65684191637433 | -5.65717658174799 |
| tel_cooccurrence \| observed only \| DOY_STD #1 | conf_low | -1.83604342097261 | -1.84567239798688 |
| tel_cooccurrence \| observed only \| I(DOY_STD^2) #1 | conf_low | -0.563119985587027 | -0.569164885372549 |
| nau_cooccurrence \| observed only \| (Intercept) #1 | conf_high | 1.72333029835013 | 1.71806949822401 |
| nau_cooccurrence \| observed only \| OTHER_POSITIVE #1 | conf_high | 2.36182477931707 | 2.3604079931468 |
| nau_cooccurrence \| observed only \| ERA2021-2024 #1 | conf_high | 0.566277407045755 | 0.567290947713062 |
| nau_cooccurrence \| observed only \| DOY_STD #1 | conf_high | 0.587356639822155 | 0.585640645185074 |
| nau_cooccurrence \| observed only \| I(DOY_STD^2) #1 | conf_high | -0.0385475694066716 | -0.0189433147783072 |
| tel_cooccurrence \| observed only \| (Intercept) #1 | conf_high | 1.0168676201077 | 1.01786786351942 |
| tel_cooccurrence \| observed only \| OTHER_POSITIVE #1 | conf_high | 2.13047846398751 | 2.12964841379566 |
| tel_cooccurrence \| observed only \| ERA2021-2024 #1 | conf_high | 3.22111436074113 | 3.22306300284492 |
| tel_cooccurrence \| observed only \| DOY_STD #1 | conf_high | 0.29527530178622 | 0.295346380296042 |
| tel_cooccurrence \| observed only \| I(DOY_STD^2) #1 | conf_high | 0.644468753989592 | 0.661135675928262 |

_Showing the first 60 of 90 rows._

### revised__17_cooccurrence_counts.csv

| row | column | before | after |
|---|---|---|---|
| nau_cooccurrence #1 | Freq | 316 | 314 |
| tel_cooccurrence #1 | Freq | 316 | 314 |

### revised__17_cooccurrence_diagnostics.csv

| row | column | before | after |
|---|---|---|---|
| nau_cooccurrence \| observed only \| TRUE \| FALSE #1 | max_gvif_adjusted | 1.07095508298949 | 1.07043785509506 |
| tel_cooccurrence \| observed only \| TRUE \| FALSE #1 | max_gvif_adjusted | 1.05537759531497 | 1.05481428519075 |
| nau_cooccurrence \| observed only \| TRUE \| FALSE #1 | dharma_uniformity_p | 0.861466233225162 | 0.636518112043751 |
| tel_cooccurrence \| observed only \| TRUE \| FALSE #1 | dharma_uniformity_p | 0.965213630747337 | 0.0960496654385736 |
| nau_cooccurrence \| observed only \| TRUE \| FALSE #1 | dharma_dispersion_p | 0.656 | 0.792 |
| tel_cooccurrence \| observed only \| TRUE \| FALSE #1 | dharma_dispersion_p | 0.752 | 0.72 |

### revised__17_cooccurrence_fit_statistics.csv

| row | column | before | after |
|---|---|---|---|
| nau_cooccurrence \| P. nausithous against the other species at the same visit \| Co-occurrence \| observed only \| ok #1 | n_obs | 586 | 584 |
| tel_cooccurrence \| P. teleius against the other species at the same visit \| Co-occurrence \| observed only \| ok #1 | n_obs | 586 | 584 |
| nau_cooccurrence \| P. nausithous against the other species at the same visit \| Co-occurrence \| observed only \| ok #1 | AIC | 632.04990872931 | 631.984887337356 |
| tel_cooccurrence \| P. teleius against the other species at the same visit \| Co-occurrence \| observed only \| ok #1 | AIC | 293.468509653486 | 293.454981711831 |
| nau_cooccurrence \| P. nausithous against the other species at the same visit \| Co-occurrence \| observed only \| ok #1 | BIC | 667.036467045927 | 666.944095199982 |
| tel_cooccurrence \| P. teleius against the other species at the same visit \| Co-occurrence \| observed only \| ok #1 | BIC | 328.455067970102 | 328.414189574457 |
| nau_cooccurrence \| P. nausithous against the other species at the same visit \| Co-occurrence \| observed only \| ok #1 | logLik | -308.024954364655 | -307.992443668678 |
| tel_cooccurrence \| P. teleius against the other species at the same visit \| Co-occurrence \| observed only \| ok #1 | logLik | -138.734254826743 | -138.727490855916 |

### revised__17_cooccurrence_gvif.csv

| row | column | before | after |
|---|---|---|---|
| nau_cooccurrence \| observed only \| OTHER_POSITIVE #1 | GVIF | 1.0774431613753 | 1.07729123443508 |
| nau_cooccurrence \| observed only \| ERA #1 | GVIF | 1.11895156988493 | 1.11844712795931 |
| nau_cooccurrence \| observed only \| DOY_STD #1 | GVIF | 1.14694478978103 | 1.14583720162052 |
| tel_cooccurrence \| observed only \| OTHER_POSITIVE #1 | GVIF | 1.07395480471319 | 1.07790974546701 |
| tel_cooccurrence \| observed only \| ERA #1 | GVIF | 1.1138218686928 | 1.11263317624248 |
| tel_cooccurrence \| observed only \| DOY_STD #1 | GVIF | 1.0656667341209 | 1.0674518866103 |
| nau_cooccurrence \| observed only \| OTHER_POSITIVE #1 | GVIF_adjusted | 1.03799959603812 | 1.03792641089582 |
| nau_cooccurrence \| observed only \| ERA #1 | GVIF_adjusted | 1.05780507178068 | 1.05756660686659 |
| nau_cooccurrence \| observed only \| DOY_STD #1 | GVIF_adjusted | 1.07095508298949 | 1.07043785509506 |
| tel_cooccurrence \| observed only \| OTHER_POSITIVE #1 | GVIF_adjusted | 1.03631790716613 | 1.03822432328809 |
| tel_cooccurrence \| observed only \| ERA #1 | GVIF_adjusted | 1.05537759531497 | 1.05481428519075 |
| tel_cooccurrence \| observed only \| DOY_STD #1 | GVIF_adjusted | 1.0323113552223 | 1.03317563202502 |

### revised__17_cooccurrence_random_effects.csv

| row | column | before | after |
|---|---|---|---|
| nau_cooccurrence \| observed only \| SITMAP #1 | variance | 1.0838382256964 | 1.07846173368053 |
| nau_cooccurrence \| observed only \| YEAR #1 | variance | 0.536206801938639 | 0.535852173842181 |
| nau_cooccurrence \| observed only \| OBSERVER #1 | variance | 2.7655818241671 | 2.76236531954379 |
| tel_cooccurrence \| observed only \| SITMAP #1 | variance | 2.02674084139006 | 2.01803198179086 |
| tel_cooccurrence \| observed only \| YEAR #1 | variance | 2.85091920157667 | 2.85365796253709 |
| tel_cooccurrence \| observed only \| OBSERVER #1 | variance | 9.99931363729082 | 9.98238356732567 |
| nau_cooccurrence \| observed only \| SITMAP #1 | sd | 1.04107551392606 | 1.03849012209097 |
| nau_cooccurrence \| observed only \| YEAR #1 | sd | 0.732261430049842 | 0.732019244174756 |
| nau_cooccurrence \| observed only \| OBSERVER #1 | sd | 1.66300385572827 | 1.66203649765695 |
| tel_cooccurrence \| observed only \| SITMAP #1 | sd | 1.42363648498838 | 1.42057452525056 |
| tel_cooccurrence \| observed only \| YEAR #1 | sd | 1.68846652367664 | 1.689277349205 |
| tel_cooccurrence \| observed only \| OBSERVER #1 | sd | 3.16216913483305 | 3.15949102979035 |

### revised__17_records.csv

| row | column | before | after |
|---|---|---|---|
| with imputed \| Phengaris teleius #1 | records | 2728 | 2716 |
| observed only \| Phengaris teleius #1 | records | 898 | 895 |
| with imputed \| Phengaris teleius #1 | imputed | 1830 | 1821 |
| with imputed \| Phengaris teleius #1 | management_recorded | 1348 | 1339 |
| observed only \| Phengaris teleius #1 | management_recorded | 400 | 398 |
| with imputed \| Phengaris teleius #1 | polygon_2021_2024_with_grazing | 727 | 723 |
| observed only \| Phengaris teleius #1 | polygon_2021_2024_with_grazing | 186 | 185 |

### revised__17_records_by_year.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris teleius #1 | imputed negative | 517 | 512 |
| Phengaris teleius #4 | imputed negative | 403 | 401 |
| Phengaris teleius #5 | imputed negative | 153 | 152 |
| Phengaris teleius #6 | imputed negative | 165 | 164 |
| Phengaris teleius #2 | observed negative | 369 | 367 |
| Phengaris teleius #4 | observed negative | 64 | 63 |

### revised__17_species_proportions.csv

| row | column | before | after |
|---|---|---|---|
| with imputed #1 | teleius_records | 2728 | 2716 |
| observed only #1 | teleius_records | 898 | 895 |
| with imputed #1 | teleius_share | 0.0780791788856305 | 0.0784241531664212 |
| observed only #1 | teleius_share | 0.237193763919822 | 0.237988826815642 |
| with imputed #1 | chi_squared | 900.468895108893 | 895.278315114114 |
| observed only #1 | chi_squared | 133.343033402775 | 131.961587001341 |

### revised__17_teleius_aic_management.csv

| row | column | before | after |
|---|---|---|---|
| tel_management_conn_a0.5 \| Management model + connectivity, alpha = 0.5 per km \| with imputed \| ok #1 | n_obs | 1348 | 1339 |
| tel_management_conn \| Management model + connectivity \| with imputed \| ok #1 | n_obs | 1348 | 1339 |
| tel_management_conn_a2 \| Management model + connectivity, alpha = 2 per km \| with imputed \| ok #1 | n_obs | 1348 | 1339 |
| tel_management \| Mowing timing x method, host plant, protection for the species \| with imputed \| ok #1 | n_obs | 1348 | 1339 |
| tel_management_null \| Detection covariates only, records with the management recorded \| with imputed \| ok #1 | n_obs | 1348 | 1339 |
| tel_management_conn_a0.5 \| Management model + connectivity, alpha = 0.5 per km \| with imputed \| ok #1 | AIC | 554.824439212817 | 553.395916570547 |
| tel_management_conn \| Management model + connectivity \| with imputed \| ok #1 | AIC | 560.834599150751 | 559.358724469026 |
| tel_management_conn_a2 \| Management model + connectivity, alpha = 2 per km \| with imputed \| ok #1 | AIC | 566.284061929278 | 564.692302486444 |
| tel_management \| Mowing timing x method, host plant, protection for the species \| with imputed \| ok #1 | AIC | 620.990300319805 | 620.134687283459 |
| tel_management_null \| Detection covariates only, records with the management recorded \| with imputed \| ok #1 | AIC | 642.094647056731 | 641.308275408972 |
| tel_management_conn_a0.5 \| Management model + connectivity, alpha = 0.5 per km \| with imputed \| ok #1 | logLik | -263.412219606409 | -262.697958285274 |
| tel_management_conn \| Management model + connectivity \| with imputed \| ok #1 | logLik | -266.417299575375 | -265.679362234513 |
| tel_management_conn_a2 \| Management model + connectivity, alpha = 2 per km \| with imputed \| ok #1 | logLik | -269.142030964639 | -268.346151243222 |
| tel_management \| Mowing timing x method, host plant, protection for the species \| with imputed \| ok #1 | logLik | -297.495150159903 | -297.06734364173 |
| tel_management_null \| Detection covariates only, records with the management recorded \| with imputed \| ok #1 | logLik | -314.047323528366 | -313.654137704486 |
| tel_management_conn \| Management model + connectivity \| with imputed \| ok #1 | delta_AIC | 6.01015993793339 | 5.96280789847856 |
| tel_management_conn_a2 \| Management model + connectivity, alpha = 2 per km \| with imputed \| ok #1 | delta_AIC | 11.4596227164611 | 11.2963859158972 |
| tel_management \| Mowing timing x method, host plant, protection for the species \| with imputed \| ok #1 | delta_AIC | 66.1658611069879 | 66.7387707129121 |
| tel_management_null \| Detection covariates only, records with the management recorded \| with imputed \| ok #1 | delta_AIC | 87.270207843914 | 87.9123588384255 |

### revised__17_teleius_aic_management_obs.csv

| row | column | before | after |
|---|---|---|---|
| tel_management_conn_a0.5_obs \| Management model + connectivity, alpha = 0.5 per km \| observed only \| ok #1 | n_obs | 400 | 398 |
| tel_management_conn_obs \| Management model + connectivity \| observed only \| ok #1 | n_obs | 400 | 398 |
| tel_management_conn_a2_obs \| Management model + connectivity, alpha = 2 per km \| observed only \| ok #1 | n_obs | 400 | 398 |
| tel_management_obs \| Mowing timing x method, host plant, protection for the species \| observed only \| ok #1 | n_obs | 400 | 398 |
| tel_management_null_obs \| Detection covariates only, records with the management recorded \| observed only \| ok #1 | n_obs | 400 | 398 |
| tel_management_conn_a0.5_obs \| Management model + connectivity, alpha = 0.5 per km \| observed only \| ok #1 | AIC | 212.044102224289 | 210.903528834659 |
| tel_management_conn_obs \| Management model + connectivity \| observed only \| ok #1 | AIC | 217.163062226104 | 215.806156660291 |
| tel_management_conn_a2_obs \| Management model + connectivity, alpha = 2 per km \| observed only \| ok #1 | AIC | 222.303175321827 | 220.758236302292 |
| tel_management_obs \| Mowing timing x method, host plant, protection for the species \| observed only \| ok #1 | AIC | 241.866283814464 | 240.918368957607 |
| tel_management_null_obs \| Detection covariates only, records with the management recorded \| observed only \| ok #1 | AIC | 257.326303471888 | 257.204305934025 |
| tel_management_conn_a0.5_obs \| Management model + connectivity, alpha = 0.5 per km \| observed only \| ok #1 | logLik | -92.0220511121443 | -91.4517644173296 |
| tel_management_conn_obs \| Management model + connectivity \| observed only \| ok #1 | logLik | -94.5815311130522 | -93.9030783301453 |
| tel_management_conn_a2_obs \| Management model + connectivity, alpha = 2 per km \| observed only \| ok #1 | logLik | -97.1515876609133 | -96.3791181511458 |
| tel_management_obs \| Mowing timing x method, host plant, protection for the species \| observed only \| ok #1 | logLik | -107.933141907232 | -107.459184478804 |
| tel_management_null_obs \| Detection covariates only, records with the management recorded \| observed only \| ok #1 | logLik | -121.663151735944 | -121.602152967012 |
| tel_management_conn_obs \| Management model + connectivity \| observed only \| ok #1 | delta_AIC | 5.11896000181582 | 4.90262782563138 |
| tel_management_conn_a2_obs \| Management model + connectivity, alpha = 2 per km \| observed only \| ok #1 | delta_AIC | 10.2590730975381 | 9.85470746763252 |
| tel_management_obs \| Mowing timing x method, host plant, protection for the species \| observed only \| ok #1 | delta_AIC | 29.8221815901759 | 30.014840122948 |
| tel_management_null_obs \| Detection covariates only, records with the management recorded \| observed only \| ok #1 | delta_AIC | 45.2822012475999 | 46.3007770993655 |

### revised__17_teleius_aic_site.csv

| row | column | before | after |
|---|---|---|---|
| tel_management \| Mowing timing x method, host plant, protection for the species \| with imputed \| ok #1 | n_obs | 1348 | 1339 |
| tel_management_site0 \| Management model + persistent site (0 m) \| with imputed \| ok #1 | n_obs | 1348 | 1339 |
| tel_management_site200 \| Management model + persistent site (200 m) \| with imputed \| ok #1 | n_obs | 1348 | 1339 |
| tel_management_site \| Management model + persistent site (50 m) \| with imputed \| ok #1 | n_obs | 1348 | 1339 |
| tel_management \| Mowing timing x method, host plant, protection for the species \| with imputed \| ok #1 | AIC | 620.990300319805 | 620.134687283459 |
| tel_management_site0 \| Management model + persistent site (0 m) \| with imputed \| ok #1 | AIC | 622.990300310052 | 622.134687253397 |
| tel_management_site200 \| Management model + persistent site (200 m) \| with imputed \| ok #1 | AIC | 622.990300322609 | 622.134687265199 |
| tel_management_site \| Management model + persistent site (50 m) \| with imputed \| ok #1 | AIC | 622.99030034125 | 622.134687282907 |
| tel_management \| Mowing timing x method, host plant, protection for the species \| with imputed \| ok #1 | logLik | -297.495150159903 | -297.06734364173 |
| tel_management_site0 \| Management model + persistent site (0 m) \| with imputed \| ok #1 | logLik | -297.495150155026 | -297.067343626699 |
| tel_management_site200 \| Management model + persistent site (200 m) \| with imputed \| ok #1 | logLik | -297.495150161304 | -297.067343632599 |
| tel_management_site \| Management model + persistent site (50 m) \| with imputed \| ok #1 | logLik | -297.495150170625 | -297.067343641454 |
| tel_management_site0 \| Management model + persistent site (0 m) \| with imputed \| ok #1 | delta_AIC | 1.99999999024647 | 1.99999996993813 |
| tel_management_site200 \| Management model + persistent site (200 m) \| with imputed \| ok #1 | delta_AIC | 2.0000000028034 | 1.99999998173973 |
| tel_management_site \| Management model + persistent site (50 m) \| with imputed \| ok #1 | delta_AIC | 2.00000002144486 | 1.99999999944805 |

### revised__17_teleius_aic_site_obs.csv

| row | column | before | after |
|---|---|---|---|
| tel_management_obs \| Mowing timing x method, host plant, protection for the species \| observed only \| ok #1 | n_obs | 400 | 398 |
| tel_management_site200_obs \| Management model + persistent site (200 m) \| observed only \| ok #1 | n_obs | 400 | 398 |
| tel_management_site_obs \| Management model + persistent site (50 m) \| observed only \| ok #1 | n_obs | 400 | 398 |
| tel_management_obs \| Mowing timing x method, host plant, protection for the species \| observed only \| ok #1 | AIC | 241.866283814464 | 240.918368957607 |
| tel_management_site200_obs \| Management model + persistent site (200 m) \| observed only \| ok #1 | AIC | 243.866283861068 | 228.417829570146 |
| tel_management_site_obs \| Management model + persistent site (50 m) \| observed only \| ok #1 | AIC | 243.866283885819 | 242.918368930767 |
| tel_management_obs \| Mowing timing x method, host plant, protection for the species \| observed only \| ok #1 | logLik | -107.933141907232 | -107.459184478804 |
| tel_management_site200_obs \| Management model + persistent site (200 m) \| observed only \| ok #1 | logLik | -107.933141930534 | -100.208914785073 |
| tel_management_site_obs \| Management model + persistent site (50 m) \| observed only \| ok #1 | logLik | -107.93314194291 | -107.459184465384 |
| tel_management_obs \| Mowing timing x method, host plant, protection for the species \| observed only \| ok #1 | delta_AIC | 0 | 17.0657552133219 |
| tel_management_site200_obs \| Management model + persistent site (200 m) \| observed only \| ok #1 | delta_AIC | 2.00000004660376 | 4.56521582586095 |
| tel_management_site_obs \| Management model + persistent site (50 m) \| observed only \| ok #1 | delta_AIC | 2.00000007135463 | 19.0657551864821 |

### revised__17_teleius_bias_reduced.csv

| row | column | before | after |
|---|---|---|---|
| tel_management_obs_brglm \| observed only \| (Intercept) #1 | estimate | 2.92695231821738 | 2.93500122995593 |
| tel_management_obs_brglm \| observed only \| TIMING #1 | estimate | -0.290973747484095 | -0.318969687109173 |
| tel_management_obs_brglm \| observed only \| METHOD #1 | estimate | 0.156170732464551 | 0.34740772209737 |
| tel_management_obs_brglm \| observed only \| PLANTabundant #1 | estimate | 1.11511462994189 | 1.16755001356299 |
| tel_management_obs_brglm \| observed only \| PLANTdominant #1 | estimate | 1.60526068446334 | 1.70288280797517 |
| tel_management_obs_brglm \| observed only \| EVL_species #1 | estimate | 1.72514389778548 | 1.67177141133501 |
| tel_management_obs_brglm \| observed only \| ERA2021-2024 #1 | estimate | -5.34609173354392 | -5.33593260789791 |
| tel_management_obs_brglm \| observed only \| DOY_STD #1 | estimate | -0.967591237446928 | -1.29926102449602 |
| tel_management_obs_brglm \| observed only \| I(DOY_STD^2) #1 | estimate | 0.321652113585211 | 0.584065984783982 |
| tel_management_obs_brglm \| observed only \| factor(YEAR)2020 #1 | estimate | -5.68242528609597 | -5.86284766556913 |
| tel_management_obs_brglm \| observed only \| factor(YEAR)2021 #1 | estimate | 1.84748801419321 | 1.64780644805349 |
| tel_management_obs_brglm \| observed only \| factor(YEAR)2022 #1 | estimate | 1.31674138121744 | 1.30952574247596 |
| tel_management_obs_brglm \| observed only \| factor(YEAR)2023 #1 | estimate | 0.623948643463102 | 0.438049991657263 |
| tel_management_obs_brglm \| observed only \| TIMING:METHOD #1 | estimate | 1.03982399608521 | 0.899874992805034 |
| tel_management_2021_obs_brglm \| observed only \| (Intercept) #1 | estimate | -1.35072027641086 | -1.62773740344951 |
| tel_management_2021_obs_brglm \| observed only \| TIMING #1 | estimate | -0.721907898773882 | -0.639739314672862 |
| tel_management_2021_obs_brglm \| observed only \| METHOD #1 | estimate | 1.05283011296847 | 1.15522687798064 |
| tel_management_2021_obs_brglm \| observed only \| PLANTabundant #1 | estimate | 2.94331333863149 | 2.84377945557692 |
| tel_management_2021_obs_brglm \| observed only \| PLANTdominant #1 | estimate | 2.63688351279104 | 2.58583559354263 |
| tel_management_2021_obs_brglm \| observed only \| EVL_species #1 | estimate | 1.2865528484988 | 1.49953822840282 |
| tel_management_2021_obs_brglm \| observed only \| GRAZE #1 | estimate | 1.43848987969059 | 1.01068853424597 |
| tel_management_2021_obs_brglm \| observed only \| LOG_AREA #1 | estimate | -0.476575563780116 | -0.548402273841385 |
| tel_management_2021_obs_brglm \| observed only \| DOY_STD #1 | estimate | -0.770252664842088 | -1.0892667278582 |
| tel_management_2021_obs_brglm \| observed only \| I(DOY_STD^2) #1 | estimate | 0.176457633082588 | 0.521097703557606 |
| tel_management_2021_obs_brglm \| observed only \| factor(YEAR)2022 #1 | estimate | -0.22217043801299 | 0.0331740309403252 |
| tel_management_2021_obs_brglm \| observed only \| factor(YEAR)2023 #1 | estimate | -0.813947039969574 | -0.8285714161037 |
| tel_management_2021_obs_brglm \| observed only \| factor(YEAR)2024 #1 | estimate | -1.30228933602577 | -1.09933107333532 |
| tel_management_2021_obs_brglm \| observed only \| TIMING:METHOD #1 | estimate | 0.707166621999022 | 0.532104279875248 |
| tel_management_obs_brglm \| observed only \| (Intercept) #1 | std_error | 1.50224905729349 | 1.50961142733003 |
| tel_management_obs_brglm \| observed only \| TIMING #1 | std_error | 0.486945300482147 | 0.489635241849233 |
| tel_management_obs_brglm \| observed only \| METHOD #1 | std_error | 0.669495503802131 | 0.669990512614355 |
| tel_management_obs_brglm \| observed only \| PLANTabundant #1 | std_error | 0.322763549636387 | 0.325408481984279 |
| tel_management_obs_brglm \| observed only \| PLANTdominant #1 | std_error | 0.487074622604528 | 0.497159742729368 |
| tel_management_obs_brglm \| observed only \| EVL_species #1 | std_error | 0.519134288254296 | 0.509058587111413 |
| tel_management_obs_brglm \| observed only \| ERA2021-2024 #1 | std_error | 1.58294611145251 | 1.58942167071951 |
| tel_management_obs_brglm \| observed only \| DOY_STD #1 | std_error | 0.291959176340441 | 0.339334470351386 |
| tel_management_obs_brglm \| observed only \| I(DOY_STD^2) #1 | std_error | 0.124474729932375 | 0.171903475372981 |
| tel_management_obs_brglm \| observed only \| factor(YEAR)2020 #1 | std_error | 1.49525968531893 | 1.50771875425558 |
| tel_management_obs_brglm \| observed only \| factor(YEAR)2021 #1 | std_error | 0.701515856255395 | 0.70974914986331 |
| tel_management_obs_brglm \| observed only \| factor(YEAR)2022 #1 | std_error | 0.638511224931378 | 0.637933699942073 |
| tel_management_obs_brglm \| observed only \| factor(YEAR)2023 #1 | std_error | 0.699165414533036 | 0.70910970873289 |
| tel_management_obs_brglm \| observed only \| TIMING:METHOD #1 | std_error | 0.771291789064413 | 0.771338949551256 |
| tel_management_2021_obs_brglm \| observed only \| (Intercept) #1 | std_error | 0.847401786268843 | 0.875647445774356 |
| tel_management_2021_obs_brglm \| observed only \| TIMING #1 | std_error | 0.812093260750535 | 0.815279005986961 |
| tel_management_2021_obs_brglm \| observed only \| METHOD #1 | std_error | 0.980510805115459 | 0.9789606492076 |
| tel_management_2021_obs_brglm \| observed only \| PLANTabundant #1 | std_error | 0.527341331964812 | 0.525122255660588 |
| tel_management_2021_obs_brglm \| observed only \| PLANTdominant #1 | std_error | 0.751826714984031 | 0.763662751820186 |
| tel_management_2021_obs_brglm \| observed only \| EVL_species #1 | std_error | 0.809849421856575 | 0.822455258463225 |
| tel_management_2021_obs_brglm \| observed only \| GRAZE #1 | std_error | 1.34357569679125 | 1.43923410613894 |
| tel_management_2021_obs_brglm \| observed only \| LOG_AREA #1 | std_error | 0.341259965052318 | 0.346374252278718 |
| tel_management_2021_obs_brglm \| observed only \| DOY_STD #1 | std_error | 0.381376884819276 | 0.461511037889327 |
| tel_management_2021_obs_brglm \| observed only \| I(DOY_STD^2) #1 | std_error | 0.160834608135695 | 0.279275346781955 |
| tel_management_2021_obs_brglm \| observed only \| factor(YEAR)2022 #1 | std_error | 0.547779967386781 | 0.5725085994191 |
| tel_management_2021_obs_brglm \| observed only \| factor(YEAR)2023 #1 | std_error | 0.674007062233631 | 0.689046784553332 |
| tel_management_2021_obs_brglm \| observed only \| factor(YEAR)2024 #1 | std_error | 0.791218672281403 | 0.806394634771875 |
| tel_management_2021_obs_brglm \| observed only \| TIMING:METHOD #1 | std_error | 1.12035494553358 | 1.12809380715989 |
| tel_management_obs_brglm \| observed only \| (Intercept) #1 | p_value | 0.0513694872334461 | 0.051870164572087 |
| tel_management_obs_brglm \| observed only \| TIMING #1 | p_value | 0.550140806425436 | 0.514760249317838 |
| tel_management_obs_brglm \| observed only \| METHOD #1 | p_value | 0.815554633149013 | 0.604091093817271 |
| tel_management_obs_brglm \| observed only \| PLANTabundant #1 | p_value | 0.000550504283424327 | 0.000333285627603197 |

_Showing the first 60 of 168 rows._

### revised__17_teleius_coefficients.csv

| row | column | before | after |
|---|---|---|---|
| tel_baseline \| with imputed \| (Intercept) #1 | estimate | -3.17612014497983 | -3.17883512446129 |
| tel_baseline \| with imputed \| ERA2021-2024 #1 | estimate | 0.181675661666284 | 0.180283773726241 |
| tel_baseline \| with imputed \| DOY_STD #1 | estimate | -0.857170452577936 | -0.870969715335656 |
| tel_baseline \| with imputed \| I(DOY_STD^2) #1 | estimate | 0.0423737039406203 | 0.0571954636207551 |
| tel_management_null \| with imputed \| (Intercept) #1 | estimate | -2.90458874086171 | -2.92535021946849 |
| tel_management_null \| with imputed \| ERA2021-2024 #1 | estimate | -0.0457023967306758 | -0.0438256060953012 |
| tel_management_null \| with imputed \| DOY_STD #1 | estimate | -1.05253319863791 | -1.09951436530772 |
| tel_management_null \| with imputed \| I(DOY_STD^2) #1 | estimate | 0.0977388223868448 | 0.143014071644955 |
| tel_management \| with imputed \| (Intercept) #1 | estimate | -3.7069179010968 | -3.72679626559334 |
| tel_management \| with imputed \| TIMING #1 | estimate | 0.221891218963707 | 0.214723974927872 |
| tel_management \| with imputed \| METHOD #1 | estimate | 0.409518933900761 | 0.405987270818609 |
| tel_management \| with imputed \| PLANTabundant #1 | estimate | 0.529451267672917 | 0.546737244777581 |
| tel_management \| with imputed \| PLANTdominant #1 | estimate | 0.938305256023097 | 0.961654451306236 |
| tel_management \| with imputed \| EVL_species #1 | estimate | 1.80838695843859 | 1.81130352409287 |
| tel_management \| with imputed \| ERA2021-2024 #1 | estimate | -0.0874895781828278 | -0.0770811185857124 |
| tel_management \| with imputed \| DOY_STD #1 | estimate | -1.16176788150263 | -1.21052415143561 |
| tel_management \| with imputed \| I(DOY_STD^2) #1 | estimate | 0.171491127390181 | 0.218421511253085 |
| tel_management \| with imputed \| TIMING:METHOD #1 | estimate | 0.418215283308686 | 0.408317793638776 |
| tel_management_conn \| with imputed \| (Intercept) #1 | estimate | -1.48407479425156 | -1.49713582161575 |
| tel_management_conn \| with imputed \| TIMING #1 | estimate | 0.332913875493998 | 0.328227067428282 |
| tel_management_conn \| with imputed \| METHOD #1 | estimate | 1.00816607773908 | 1.00259806937614 |
| tel_management_conn \| with imputed \| PLANTabundant #1 | estimate | 0.533143010690291 | 0.558015950168377 |
| tel_management_conn \| with imputed \| PLANTdominant #1 | estimate | 0.736084764770908 | 0.760325378626287 |
| tel_management_conn \| with imputed \| EVL_species #1 | estimate | 1.20366694591314 | 1.19749166441626 |
| tel_management_conn \| with imputed \| LOG_CONN #1 | estimate | 0.996040650167866 | 1.00021712208155 |
| tel_management_conn \| with imputed \| ERA2021-2024 #1 | estimate | -0.309676212034104 | -0.307770147952555 |
| tel_management_conn \| with imputed \| DOY_STD #1 | estimate | -1.00538654698329 | -1.06589031473927 |
| tel_management_conn \| with imputed \| I(DOY_STD^2) #1 | estimate | 0.1690136489258 | 0.232900393745447 |
| tel_management_conn \| with imputed \| TIMING:METHOD #1 | estimate | -0.310029821960521 | -0.327198646741509 |
| tel_management_conn_a0.5 \| with imputed \| (Intercept) #1 | estimate | -1.89435191636515 | -1.91523495082406 |
| tel_management_conn_a0.5 \| with imputed \| TIMING #1 | estimate | 0.316925233117868 | 0.316530935561349 |
| tel_management_conn_a0.5 \| with imputed \| METHOD #1 | estimate | 1.02106588329092 | 1.01850439756766 |
| tel_management_conn_a0.5 \| with imputed \| PLANTabundant #1 | estimate | 0.56636671255076 | 0.591472273724385 |
| tel_management_conn_a0.5 \| with imputed \| PLANTdominant #1 | estimate | 0.746242772870331 | 0.773944554538592 |
| tel_management_conn_a0.5 \| with imputed \| EVL_species #1 | estimate | 0.880515182287007 | 0.873450962428284 |
| tel_management_conn_a0.5 \| with imputed \| LOG_CONN_0.5 #1 | estimate | 1.23447889327015 | 1.23738026126143 |
| tel_management_conn_a0.5 \| with imputed \| ERA2021-2024 #1 | estimate | -0.393338427187423 | -0.395322786495239 |
| tel_management_conn_a0.5 \| with imputed \| DOY_STD #1 | estimate | -1.03252702480711 | -1.09218625741013 |
| tel_management_conn_a0.5 \| with imputed \| I(DOY_STD^2) #1 | estimate | 0.184785583772862 | 0.247088757467518 |
| tel_management_conn_a0.5 \| with imputed \| TIMING:METHOD #1 | estimate | -0.310712323569772 | -0.331834417136232 |
| tel_management_conn_a2 \| with imputed \| (Intercept) #1 | estimate | -1.11713394410364 | -1.12217106327824 |
| tel_management_conn_a2 \| with imputed \| TIMING #1 | estimate | 0.406344747044431 | 0.396892906901761 |
| tel_management_conn_a2 \| with imputed \| METHOD #1 | estimate | 0.987020652927064 | 0.979358037811833 |
| tel_management_conn_a2 \| with imputed \| PLANTabundant #1 | estimate | 0.480605670611775 | 0.50919885949251 |
| tel_management_conn_a2 \| with imputed \| PLANTdominant #1 | estimate | 0.731513674412511 | 0.758106544090622 |
| tel_management_conn_a2 \| with imputed \| EVL_species #1 | estimate | 1.63720946714676 | 1.63230280429021 |
| tel_management_conn_a2 \| with imputed \| LOG_CONN_2 #1 | estimate | 0.978479470351153 | 0.984416208938472 |
| tel_management_conn_a2 \| with imputed \| ERA2021-2024 #1 | estimate | -0.246813707634376 | -0.242927426775578 |
| tel_management_conn_a2 \| with imputed \| DOY_STD #1 | estimate | -0.977958087535146 | -1.04311298494713 |
| tel_management_conn_a2 \| with imputed \| I(DOY_STD^2) #1 | estimate | 0.146204701318128 | 0.21497648917801 |
| tel_management_conn_a2 \| with imputed \| TIMING:METHOD #1 | estimate | -0.331871499975892 | -0.349707288801398 |
| tel_management_site \| with imputed \| (Intercept) #1 | estimate | -3.70691744819885 | -3.72679619295722 |
| tel_management_site \| with imputed \| TIMING #1 | estimate | 0.221891005155404 | 0.214723852705146 |
| tel_management_site \| with imputed \| METHOD #1 | estimate | 0.40951911063027 | 0.40598726445413 |
| tel_management_site \| with imputed \| PLANTabundant #1 | estimate | 0.529451157676007 | 0.546737254030063 |
| tel_management_site \| with imputed \| PLANTdominant #1 | estimate | 0.938304938029744 | 0.961654455340576 |
| tel_management_site \| with imputed \| EVL_species #1 | estimate | 1.80838677773081 | 1.81130342855727 |
| tel_management_site \| with imputed \| ERA2021-2024 #1 | estimate | -0.0874899984464714 | -0.0770810980098996 |
| tel_management_site \| with imputed \| DOY_STD #1 | estimate | -1.16176779164656 | -1.21052411010427 |
| tel_management_site \| with imputed \| I(DOY_STD^2) #1 | estimate | 0.171491159236107 | 0.218421477914986 |

_Showing the first 60 of 1641 rows._

### revised__17_teleius_diagnostics.csv

| row | column | before | after |
|---|---|---|---|
| tel_baseline \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | max_gvif_adjusted | 1.00003153199828 | 1.00001439384343 |
| tel_management_null \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | max_gvif_adjusted | 1.00007192533015 | 1.0000485103501 |
| tel_management \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | max_gvif_adjusted | 1.10192574822089 | 1.10156128091236 |
| tel_management_conn \| with imputed \| TRUE \| FALSE \| <NA> #1 | max_gvif_adjusted | 1.1041748315881 | 1.10382383176974 |
| tel_management_conn_a0.5 \| with imputed \| TRUE \| FALSE \| <NA> #1 | max_gvif_adjusted | 1.10476603084997 | 1.1045456190796 |
| tel_management_site \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | max_gvif_adjusted | 1.10192574822089 | 1.10156128091236 |
| tel_management_site0 \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | max_gvif_adjusted | 1.10192574822089 | 1.10156128091236 |
| tel_management_site200 \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | max_gvif_adjusted | 1.10192574822089 | 1.10156128091236 |
| tel_management_2021 \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | max_gvif_adjusted | 1.07777807048561 | 1.07696049206417 |
| tel_baseline_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | max_gvif_adjusted | 1.00302079421651 | 1.0029622655162 |
| tel_management_null_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | max_gvif_adjusted | 1.000175544315 | 1.00023935756488 |
| tel_management_obs \| observed only \| TRUE \| FALSE \| possible separation; random-effect variance inflated #1 | max_gvif_adjusted | 1.11178673258274 | 1.11417842005519 |
| tel_management_conn_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | max_gvif_adjusted | 1.11344194219925 | 1.11559035102639 |
| tel_management_conn_a0.5_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | max_gvif_adjusted | 1.11275912468034 | 1.11488953226887 |
| tel_management_conn_a2_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | max_gvif_adjusted | 1.11375901814224 | 1.11596434831413 |
| tel_management_site200_obs \| observed only \| TRUE \| FALSE \| possible separation; random-effect variance inflated #1 | max_gvif_adjusted | 1.11178673258274 | 1.11417842005519 |
| tel_management_2021_obs \| observed only \| TRUE \| FALSE \| possible separation; random-effect variance inflated #1 | max_gvif_adjusted | 1.088132791627 | 1.07972285599799 |
| tel_baseline \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_uniformity_p | 0.47790795210707 | 0.00939201645305573 |
| tel_management_null \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_uniformity_p | 0.323450537938356 | 0.226540516593613 |
| tel_management \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_uniformity_p | 0.298804406270564 | 0.448493018623965 |
| tel_management_conn \| with imputed \| TRUE \| FALSE \| <NA> #1 | dharma_uniformity_p | 0.146115135177042 | 0.383013931040793 |
| tel_management_conn_a0.5 \| with imputed \| TRUE \| FALSE \| <NA> #1 | dharma_uniformity_p | 0.147737351973217 | 0.486058879701781 |
| tel_management_site \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_uniformity_p | 0.298804406270564 | 0.448493018623965 |
| tel_management_site0 \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_uniformity_p | 0.298804406270564 | 0.448493018623965 |
| tel_management_site200 \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_uniformity_p | 0.298804406270564 | 0.448493018623965 |
| tel_management_2021 \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_uniformity_p | 0.228997554632252 | 0.857690030208387 |
| tel_baseline_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_uniformity_p | 0.78472090824182 | 0.102586315265848 |
| tel_management_null_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_uniformity_p | 0.324804869776282 | 0.86795986721185 |
| tel_management_obs \| observed only \| TRUE \| FALSE \| possible separation; random-effect variance inflated #1 | dharma_uniformity_p | 0.650140705170435 | 0.665380493785063 |
| tel_management_conn_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_uniformity_p | 0.669946634374503 | 0.887070205362626 |
| tel_management_conn_a0.5_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_uniformity_p | 0.620324969179158 | 0.806893492652627 |
| tel_management_conn_a2_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_uniformity_p | 0.515801790145709 | 0.810143793242179 |
| tel_management_site200_obs \| observed only \| TRUE \| FALSE \| possible separation; random-effect variance inflated #1 | dharma_uniformity_p | 0.650140705170435 | 0.710428297559448 |
| tel_management_2021_obs \| observed only \| TRUE \| FALSE \| possible separation; random-effect variance inflated #1 | dharma_uniformity_p | 0.755555319986496 | 0.85025793871425 |
| tel_baseline \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_dispersion_p | 0.4 | 0.344 |
| tel_management_null \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_dispersion_p | 0.272 | 0.264 |
| tel_management \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_dispersion_p | 0.36 | 0.312 |
| tel_management_conn \| with imputed \| TRUE \| FALSE \| <NA> #1 | dharma_dispersion_p | 0.424 | 0.36 |
| tel_management_conn_a0.5 \| with imputed \| TRUE \| FALSE \| <NA> #1 | dharma_dispersion_p | 0.52 | 0.416 |
| tel_management_site \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_dispersion_p | 0.36 | 0.312 |
| tel_management_site0 \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_dispersion_p | 0.36 | 0.312 |
| tel_management_site200 \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_dispersion_p | 0.36 | 0.312 |
| tel_baseline_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_dispersion_p | 0.648 | 0.768 |
| tel_management_null_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_dispersion_p | 0.688 | 0.704 |
| tel_management_obs \| observed only \| TRUE \| FALSE \| possible separation; random-effect variance inflated #1 | dharma_dispersion_p | 0.512 | 0.528 |
| tel_management_conn_a2_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_dispersion_p | 0.104 | 0.144 |
| tel_management_site200_obs \| observed only \| TRUE \| FALSE \| possible separation; random-effect variance inflated #1 | dharma_dispersion_p | 0.512 | 0.96 |
| tel_management_2021_obs \| observed only \| TRUE \| FALSE \| possible separation; random-effect variance inflated #1 | dharma_dispersion_p | 0.992 | 0.936 |

### revised__17_teleius_fit_statistics.csv

| row | column | before | after |
|---|---|---|---|
| tel_baseline \| Detection covariates and random effects, every in-season record \| Baseline \| with imputed \| ok #1 | n_obs | 2728 | 2716 |
| tel_management_null \| Detection covariates only, records with the management recorded \| Management \| with imputed \| ok #1 | n_obs | 1348 | 1339 |
| tel_management \| Mowing timing x method, host plant, protection for the species \| Management \| with imputed \| ok #1 | n_obs | 1348 | 1339 |
| tel_management_conn \| Management model + connectivity \| Connectivity \| with imputed \| ok #1 | n_obs | 1348 | 1339 |
| tel_management_conn_a0.5 \| Management model + connectivity, alpha = 0.5 per km \| Connectivity \| with imputed \| ok #1 | n_obs | 1348 | 1339 |
| tel_management_conn_a2 \| Management model + connectivity, alpha = 2 per km \| Connectivity \| with imputed \| ok #1 | n_obs | 1348 | 1339 |
| tel_management_site \| Management model + persistent site (50 m) \| Persistent site \| with imputed \| ok #1 | n_obs | 1348 | 1339 |
| tel_management_site0 \| Management model + persistent site (0 m) \| Persistent site \| with imputed \| ok #1 | n_obs | 1348 | 1339 |
| tel_management_site200 \| Management model + persistent site (200 m) \| Persistent site \| with imputed \| ok #1 | n_obs | 1348 | 1339 |
| tel_management_2021 \| 2021-2024 polygon sites: management + grazing + site area \| 2021-2024 \| with imputed \| ok #1 | n_obs | 727 | 723 |
| tel_baseline_obs \| Detection covariates and random effects, every in-season record \| Baseline \| observed only \| ok #1 | n_obs | 898 | 895 |
| tel_management_null_obs \| Detection covariates only, records with the management recorded \| Management \| observed only \| ok #1 | n_obs | 400 | 398 |
| tel_management_obs \| Mowing timing x method, host plant, protection for the species \| Management \| observed only \| ok #1 | n_obs | 400 | 398 |
| tel_management_conn_obs \| Management model + connectivity \| Connectivity \| observed only \| ok #1 | n_obs | 400 | 398 |
| tel_management_conn_a0.5_obs \| Management model + connectivity, alpha = 0.5 per km \| Connectivity \| observed only \| ok #1 | n_obs | 400 | 398 |
| tel_management_conn_a2_obs \| Management model + connectivity, alpha = 2 per km \| Connectivity \| observed only \| ok #1 | n_obs | 400 | 398 |
| tel_management_site_obs \| Management model + persistent site (50 m) \| Persistent site \| observed only \| ok #1 | n_obs | 400 | 398 |
| tel_management_site200_obs \| Management model + persistent site (200 m) \| Persistent site \| observed only \| ok #1 | n_obs | 400 | 398 |
| tel_management_2021_obs \| 2021-2024 polygon sites: management + grazing + site area \| 2021-2024 \| observed only \| ok #1 | n_obs | 186 | 185 |
| tel_baseline \| Detection covariates and random effects, every in-season record \| Baseline \| with imputed \| ok #1 | AIC | 1181.22057781924 | 1180.83221598233 |
| tel_management_null \| Detection covariates only, records with the management recorded \| Management \| with imputed \| ok #1 | AIC | 642.094647056731 | 641.308275408972 |
| tel_management \| Mowing timing x method, host plant, protection for the species \| Management \| with imputed \| ok #1 | AIC | 620.990300319805 | 620.134687283459 |
| tel_management_conn \| Management model + connectivity \| Connectivity \| with imputed \| ok #1 | AIC | 560.834599150751 | 559.358724469026 |
| tel_management_conn_a0.5 \| Management model + connectivity, alpha = 0.5 per km \| Connectivity \| with imputed \| ok #1 | AIC | 554.824439212817 | 553.395916570547 |
| tel_management_conn_a2 \| Management model + connectivity, alpha = 2 per km \| Connectivity \| with imputed \| ok #1 | AIC | 566.284061929278 | 564.692302486444 |
| tel_management_site \| Management model + persistent site (50 m) \| Persistent site \| with imputed \| ok #1 | AIC | 622.99030034125 | 622.134687282907 |
| tel_management_site0 \| Management model + persistent site (0 m) \| Persistent site \| with imputed \| ok #1 | AIC | 622.990300310052 | 622.134687253397 |
| tel_management_site200 \| Management model + persistent site (200 m) \| Persistent site \| with imputed \| ok #1 | AIC | 622.990300322609 | 622.134687265199 |
| tel_management_2021 \| 2021-2024 polygon sites: management + grazing + site area \| 2021-2024 \| with imputed \| ok #1 | AIC | 374.932737296056 | 374.198302036736 |
| tel_baseline_obs \| Detection covariates and random effects, every in-season record \| Baseline \| observed only \| ok #1 | AIC | 531.867160721892 | 531.853712328314 |
| tel_management_null_obs \| Detection covariates only, records with the management recorded \| Management \| observed only \| ok #1 | AIC | 257.326303471888 | 257.204305934025 |
| tel_management_obs \| Mowing timing x method, host plant, protection for the species \| Management \| observed only \| ok #1 | AIC | 241.866283814464 | 240.918368957607 |
| tel_management_conn_obs \| Management model + connectivity \| Connectivity \| observed only \| ok #1 | AIC | 217.163062226104 | 215.806156660291 |
| tel_management_conn_a0.5_obs \| Management model + connectivity, alpha = 0.5 per km \| Connectivity \| observed only \| ok #1 | AIC | 212.044102224289 | 210.903528834659 |
| tel_management_conn_a2_obs \| Management model + connectivity, alpha = 2 per km \| Connectivity \| observed only \| ok #1 | AIC | 222.303175321827 | 220.758236302292 |
| tel_management_site_obs \| Management model + persistent site (50 m) \| Persistent site \| observed only \| ok #1 | AIC | 243.866283885819 | 242.918368930767 |
| tel_management_site200_obs \| Management model + persistent site (200 m) \| Persistent site \| observed only \| ok #1 | AIC | 243.866283861068 | 228.417829570146 |
| tel_baseline \| Detection covariates and random effects, every in-season record \| Baseline \| with imputed \| ok #1 | BIC | 1222.59984595198 | 1222.18062440308 |
| tel_management_null \| Detection covariates only, records with the management recorded \| Management \| with imputed \| ok #1 | BIC | 678.539288097037 | 677.706023828811 |
| tel_management \| Mowing timing x method, host plant, protection for the species \| Management \| with imputed \| ok #1 | BIC | 688.673205108945 | 687.730505777444 |
| tel_management_conn \| Management model + connectivity \| Connectivity \| with imputed \| ok #1 | BIC | 633.723881231362 | 632.154221308702 |
| tel_management_conn_a0.5 \| Management model + connectivity, alpha = 0.5 per km \| Connectivity \| with imputed \| ok #1 | BIC | 627.713721293429 | 626.191413410223 |
| tel_management_conn_a2 \| Management model + connectivity, alpha = 2 per km \| Connectivity \| with imputed \| ok #1 | BIC | 639.17334400989 | 637.487799326121 |
| tel_management_site \| Management model + persistent site (50 m) \| Persistent site \| with imputed \| ok #1 | BIC | 695.879582421862 | 694.930184122584 |
| tel_management_site0 \| Management model + persistent site (0 m) \| Persistent site \| with imputed \| ok #1 | BIC | 695.879582390663 | 694.930184093074 |
| tel_management_site200 \| Management model + persistent site (200 m) \| Persistent site \| with imputed \| ok #1 | BIC | 695.87958240322 | 694.930184104875 |
| tel_management_2021 \| 2021-2024 polygon sites: management + grazing + site area \| 2021-2024 \| with imputed \| ok #1 | BIC | 439.177707981525 | 438.366031146959 |
| tel_baseline_obs \| Detection covariates and random effects, every in-season record \| Baseline \| observed only \| ok #1 | BIC | 565.468351200008 | 565.431478356238 |
| tel_management_null_obs \| Detection covariates only, records with the management recorded \| Management \| observed only \| ok #1 | BIC | 285.266555301644 | 285.109469971016 |
| tel_management_obs \| Mowing timing x method, host plant, protection for the species \| Management \| observed only \| ok #1 | BIC | 293.755322926868 | 292.742245026305 |
| tel_management_conn_obs \| Management model + connectivity \| Connectivity \| observed only \| ok #1 | BIC | 273.043565885616 | 271.616484734273 |
| tel_management_conn_a0.5_obs \| Management model + connectivity, alpha = 0.5 per km \| Connectivity \| observed only \| ok #1 | BIC | 267.9246058838 | 266.713856908641 |
| tel_management_conn_a2_obs \| Management model + connectivity, alpha = 2 per km \| Connectivity \| observed only \| ok #1 | BIC | 278.183678981338 | 276.568564376274 |
| tel_management_site_obs \| Management model + persistent site (50 m) \| Persistent site \| observed only \| ok #1 | BIC | 299.746787545331 | 298.728697004749 |
| tel_management_site200_obs \| Management model + persistent site (200 m) \| Persistent site \| observed only \| ok #1 | BIC | 299.74678752058 | 284.228157644128 |
| tel_management_2021_obs \| 2021-2024 polygon sites: management + grazing + site area \| 2021-2024 \| observed only \| ok #1 | BIC | 165.973111187013 | 165.897639304142 |
| tel_baseline \| Detection covariates and random effects, every in-season record \| Baseline \| with imputed \| ok #1 | logLik | -583.610288909619 | -583.416107991164 |
| tel_management_null \| Detection covariates only, records with the management recorded \| Management \| with imputed \| ok #1 | logLik | -314.047323528366 | -313.654137704486 |
| tel_management \| Mowing timing x method, host plant, protection for the species \| Management \| with imputed \| ok #1 | logLik | -297.495150159903 | -297.06734364173 |
| tel_management_conn \| Management model + connectivity \| Connectivity \| with imputed \| ok #1 | logLik | -266.417299575375 | -265.679362234513 |

_Showing the first 60 of 74 rows._

### revised__17_teleius_gvif.csv

| row | column | before | after |
|---|---|---|---|
| tel_baseline \| with imputed \| ERA #1 | GVIF | 1.00006306499084 | 1.00002878789405 |
| tel_baseline \| with imputed \| DOY_STD #1 | GVIF | 1.00006306499084 | 1.00002878789405 |
| tel_management_null \| with imputed \| ERA #1 | GVIF | 1.00014385583355 | 1.00009702305346 |
| tel_management_null \| with imputed \| DOY_STD #1 | GVIF | 1.00014385583355 | 1.00009702305346 |
| tel_management \| with imputed \| TIMING #1 | GVIF | 1.21424035459216 | 1.21343725560529 |
| tel_management \| with imputed \| METHOD #1 | GVIF | 1.11228352730818 | 1.11320603280014 |
| tel_management \| with imputed \| PLANT #1 | GVIF | 1.15877734242184 | 1.1586660909902 |
| tel_management \| with imputed \| EVL_species #1 | GVIF | 1.02926650015722 | 1.02903015083726 |
| tel_management \| with imputed \| ERA #1 | GVIF | 1.04201246185766 | 1.04154044269077 |
| tel_management \| with imputed \| DOY_STD #1 | GVIF | 1.01670270727411 | 1.01471903250867 |
| tel_management_conn \| with imputed \| TIMING #1 | GVIF | 1.21920205871261 | 1.21842705158283 |
| tel_management_conn \| with imputed \| METHOD #1 | GVIF | 1.11452489804484 | 1.11524235269408 |
| tel_management_conn \| with imputed \| PLANT #1 | GVIF | 1.16788228339329 | 1.16811785458412 |
| tel_management_conn \| with imputed \| EVL_species #1 | GVIF | 1.06597248869186 | 1.06623942158577 |
| tel_management_conn \| with imputed \| LOG_CONN #1 | GVIF | 1.07597690129559 | 1.07927823879522 |
| tel_management_conn \| with imputed \| ERA #1 | GVIF | 1.05371992794445 | 1.0539856511342 |
| tel_management_conn \| with imputed \| DOY_STD #1 | GVIF | 1.0224165808478 | 1.0220007422641 |
| tel_management_conn_a0.5 \| with imputed \| TIMING #1 | GVIF | 1.22050798291999 | 1.22002102462794 |
| tel_management_conn_a0.5 \| with imputed \| METHOD #1 | GVIF | 1.11591357386504 | 1.1167534388329 |
| tel_management_conn_a0.5 \| with imputed \| PLANT #1 | GVIF | 1.16408745134061 | 1.16398137620102 |
| tel_management_conn_a0.5 \| with imputed \| EVL_species #1 | GVIF | 1.09570523204585 | 1.09614229331614 |
| tel_management_conn_a0.5 \| with imputed \| LOG_CONN_0.5 #1 | GVIF | 1.09858766637709 | 1.10224955161944 |
| tel_management_conn_a0.5 \| with imputed \| ERA #1 | GVIF | 1.05100356513178 | 1.05123794252321 |
| tel_management_conn_a0.5 \| with imputed \| DOY_STD #1 | GVIF | 1.02143815505138 | 1.02116645942275 |
| tel_management_conn_a2 \| with imputed \| TIMING #1 | GVIF | 1.21786562773403 | 1.21706466802563 |
| tel_management_conn_a2 \| with imputed \| METHOD #1 | GVIF | 1.1129728365226 | 1.11374227772366 |
| tel_management_conn_a2 \| with imputed \| PLANT #1 | GVIF | 1.17088753932972 | 1.17111566681867 |
| tel_management_conn_a2 \| with imputed \| EVL_species #1 | GVIF | 1.0342026178463 | 1.03409204111612 |
| tel_management_conn_a2 \| with imputed \| LOG_CONN_2 #1 | GVIF | 1.03557001474175 | 1.0384491145423 |
| tel_management_conn_a2 \| with imputed \| ERA #1 | GVIF | 1.04761448946432 | 1.0475992134176 |
| tel_management_conn_a2 \| with imputed \| DOY_STD #1 | GVIF | 1.02098981868055 | 1.0207976384735 |
| tel_management_site \| with imputed \| TIMING #1 | GVIF | 1.21424035459216 | 1.21343725560529 |
| tel_management_site \| with imputed \| METHOD #1 | GVIF | 1.11228352730818 | 1.11320603280014 |
| tel_management_site \| with imputed \| PLANT #1 | GVIF | 1.15877734242184 | 1.1586660909902 |
| tel_management_site \| with imputed \| EVL_species #1 | GVIF | 1.02926650015722 | 1.02903015083726 |
| tel_management_site \| with imputed \| ERA #1 | GVIF | 1.04201246185766 | 1.04154044269077 |
| tel_management_site \| with imputed \| DOY_STD #1 | GVIF | 1.01670270727411 | 1.01471903250867 |
| tel_management_site0 \| with imputed \| TIMING #1 | GVIF | 1.21424035459216 | 1.21343725560529 |
| tel_management_site0 \| with imputed \| METHOD #1 | GVIF | 1.11228352730818 | 1.11320603280014 |
| tel_management_site0 \| with imputed \| PLANT #1 | GVIF | 1.15877734242184 | 1.1586660909902 |
| tel_management_site0 \| with imputed \| EVL_species #1 | GVIF | 1.02926650015722 | 1.02903015083726 |
| tel_management_site0 \| with imputed \| ERA #1 | GVIF | 1.04201246185766 | 1.04154044269077 |
| tel_management_site0 \| with imputed \| DOY_STD #1 | GVIF | 1.01670270727411 | 1.01471903250867 |
| tel_management_site200 \| with imputed \| TIMING #1 | GVIF | 1.21424035459216 | 1.21343725560529 |
| tel_management_site200 \| with imputed \| METHOD #1 | GVIF | 1.11228352730818 | 1.11320603280014 |
| tel_management_site200 \| with imputed \| PLANT #1 | GVIF | 1.15877734242184 | 1.1586660909902 |
| tel_management_site200 \| with imputed \| EVL_species #1 | GVIF | 1.02926650015722 | 1.02903015083726 |
| tel_management_site200 \| with imputed \| ERA #1 | GVIF | 1.04201246185766 | 1.04154044269077 |
| tel_management_site200 \| with imputed \| DOY_STD #1 | GVIF | 1.01670270727411 | 1.01471903250867 |
| tel_management_2021 \| with imputed \| TIMING #1 | GVIF | 1.16160556921968 | 1.1598439014671 |
| tel_management_2021 \| with imputed \| METHOD #1 | GVIF | 1.08362327450956 | 1.08477569020554 |
| tel_management_2021 \| with imputed \| PLANT #1 | GVIF | 1.17911363755598 | 1.17703050042127 |
| tel_management_2021 \| with imputed \| EVL_species #1 | GVIF | 1.04917481484639 | 1.04929674122 |
| tel_management_2021 \| with imputed \| GRAZE #1 | GVIF | 1.00435029250823 | 1.00417287258009 |
| tel_management_2021 \| with imputed \| LOG_AREA #1 | GVIF | 1.03147322955402 | 1.02888441397818 |
| tel_management_2021 \| with imputed \| DOY_STD #1 | GVIF | 1.05324580358702 | 1.04987107146406 |
| tel_baseline_obs \| observed only \| ERA #1 | GVIF | 1.00605071363071 | 1.0059333060494 |
| tel_baseline_obs \| observed only \| DOY_STD #1 | GVIF | 1.00605071363071 | 1.0059333060494 |
| tel_management_null_obs \| observed only \| ERA #1 | GVIF | 1.0003511194458 | 1.0004787724218 |
| tel_management_null_obs \| observed only \| DOY_STD #1 | GVIF | 1.0003511194458 | 1.0004787724218 |

_Showing the first 60 of 224 rows._

### revised__17_teleius_key_estimates.csv

| row | column | before | after |
|---|---|---|---|
| tel_management_2021_obs \| observed only \| GRAZE \| 10.14 (0.00-57202.41) #1 | p_value | 0.599188312931344 | 0.59918983148977 |
| tel_management_2021_obs \| observed only \| TIMING:METHOD \| 0.00 (0.00-0.00) #1 | p_value | 0.00404228594274602 | 0.00404227403314147 |

### revised__17_teleius_predictions_management.csv

| row | column | before | after |
|---|---|---|---|
| tel_management \| with imputed #1 | estimate | 0.016197637549582 | 0.0167865814514551 |
| tel_management \| with imputed #2 | estimate | 0.0241967962987845 | 0.0249830054110756 |
| tel_management \| with imputed #3 | estimate | 0.0201406593680062 | 0.0207239733582306 |
| tel_management \| with imputed #4 | estimate | 0.0449190470888177 | 0.0455981648476805 |
| tel_management_obs \| observed only #1 | estimate | 0.562735073860918 | 0.564104099163582 |
| tel_management_obs \| observed only #2 | estimate | 0.288306652513378 | 0.299573027676365 |
| tel_management_obs \| observed only #3 | estimate | 0.804748461881775 | 0.823869094873556 |
| tel_management_obs \| observed only #4 | estimate | 0.887573352735755 | 0.894437232571296 |
| tel_management \| with imputed #1 | conf_low | 0.00643302319745758 | 0.00668461276025648 |
| tel_management \| with imputed #2 | conf_low | 0.00721502481706396 | 0.00745476122785772 |
| tel_management \| with imputed #3 | conf_low | 0.00871497577581051 | 0.0089957063140877 |
| tel_management \| with imputed #4 | conf_low | 0.0220251817588352 | 0.022381174271325 |
| tel_management_obs \| observed only #1 | conf_low | 0.00575112426555683 | 0.00649034662995765 |
| tel_management_obs \| observed only #2 | conf_low | 0.0011064479792985 | 0.00119053397196957 |
| tel_management_obs \| observed only #3 | conf_low | 0.0146036903979016 | 0.0182697060867334 |
| tel_management_obs \| observed only #4 | conf_low | 0.0325043427384619 | 0.0386762588990741 |
| tel_management \| with imputed #1 | conf_high | 0.0401843980560412 | 0.0415168433168606 |
| tel_management \| with imputed #2 | conf_high | 0.0780074424830166 | 0.0803871440519539 |
| tel_management \| with imputed #3 | conf_high | 0.0458529788172305 | 0.0470176511534714 |
| tel_management \| with imputed #4 | conf_high | 0.0894335587833201 | 0.0906655933757493 |
| tel_management_obs \| observed only #1 | conf_high | 0.99651963477597 | 0.996114453587778 |
| tel_management_obs \| observed only #2 | conf_high | 0.993295494655936 | 0.993526207480358 |
| tel_management_obs \| observed only #3 | conf_high | 0.999128351102208 | 0.99915018410264 |
| tel_management_obs \| observed only #4 | conf_high | 0.999461248697061 | 0.999439915857092 |

### revised__17_teleius_predictions_plant_protection.csv

| row | column | before | after |
|---|---|---|---|
| tel_management \| with imputed \| single #1 | estimate | 0.0209998350974783 | 0.0213882759584694 |
| tel_management \| with imputed \| abundant #1 | estimate | 0.0351426098507015 | 0.0363843142284073 |
| tel_management \| with imputed \| dominant #1 | estimate | 0.0519703719115675 | 0.0540828636836853 |
| tel_management_obs \| observed only \| single #1 | estimate | 0.711841907830947 | 0.728601480050032 |
| tel_management_obs \| observed only \| abundant #1 | estimate | 0.794096817432554 | 0.801170642064977 |
| tel_management_obs \| observed only \| dominant #1 | estimate | 0.97148389464877 | 0.975004916480458 |
| tel_management \| with imputed \| <NA> #1 | estimate | 0.0241142867599486 | 0.0247045825224835 |
| tel_management \| with imputed \| <NA> #2 | estimate | 0.130999061344017 | 0.13418542700772 |
| tel_management_obs \| observed only \| <NA> #1 | estimate | 0.676213773982843 | 0.684782950821873 |
| tel_management_obs \| observed only \| <NA> #2 | estimate | 0.999160779461369 | 0.999477410684704 |
| tel_management \| with imputed \| single #1 | conf_low | 0.0100370493228385 | 0.0102433141108778 |
| tel_management \| with imputed \| abundant #1 | conf_low | 0.0162910924197957 | 0.0169015932358735 |
| tel_management \| with imputed \| dominant #1 | conf_low | 0.0197493908994272 | 0.0205700701531295 |
| tel_management_obs \| observed only \| single #1 | conf_low | 0.0106927911701583 | 0.0131043138984494 |
| tel_management_obs \| observed only \| abundant #1 | conf_low | 0.0177129146286263 | 0.0200476584249395 |
| tel_management_obs \| observed only \| dominant #1 | conf_low | 0.0664448865129219 | 0.0829709404569775 |
| tel_management \| with imputed \| <NA> #1 | conf_low | 0.0119727127802163 | 0.0122902411484016 |
| tel_management \| with imputed \| <NA> #2 | conf_low | 0.0457100939812611 | 0.0471567077775546 |
| tel_management_obs \| observed only \| <NA> #1 | conf_low | 0.0103136982154061 | 0.0119258975034106 |
| tel_management_obs \| observed only \| <NA> #2 | conf_low | 0.283462249962635 | 0.284877421977694 |
| tel_management \| with imputed \| single #1 | conf_high | 0.0434114507846566 | 0.0441186942681534 |
| tel_management \| with imputed \| abundant #1 | conf_high | 0.0741638548597141 | 0.0765757546995488 |
| tel_management \| with imputed \| dominant #1 | conf_high | 0.129798863562272 | 0.134686549017201 |
| tel_management_obs \| observed only \| single #1 | conf_high | 0.998231985069345 | 0.998161013708963 |
| tel_management_obs \| observed only \| abundant #1 | conf_high | 0.998789110253455 | 0.998741588146597 |
| tel_management_obs \| observed only \| dominant #1 | conf_high | 0.99993867971275 | 0.999940541675807 |
| tel_management \| with imputed \| <NA> #1 | conf_high | 0.047970863601848 | 0.0490360699741963 |
| tel_management \| with imputed \| <NA> #2 | conf_high | 0.321767757726394 | 0.326749843844491 |
| tel_management_obs \| observed only \| <NA> #1 | conf_high | 0.997616423119613 | 0.997449027438311 |
| tel_management_obs \| observed only \| <NA> #2 | conf_high | 0.999999720914364 | 0.999999891093806 |

### revised__17_teleius_random_effects.csv

| row | column | before | after |
|---|---|---|---|
| tel_baseline \| with imputed \| SITMAP #1 | levels | 282 | 281 |
| tel_baseline \| with imputed \| OBSERVER #1 | levels | 51 | 50 |
| tel_management_null \| with imputed \| SITMAP #1 | levels | 243 | 242 |
| tel_management_null \| with imputed \| OBSERVER #1 | levels | 48 | 47 |
| tel_management \| with imputed \| SITMAP #1 | levels | 243 | 242 |
| tel_management \| with imputed \| OBSERVER #1 | levels | 48 | 47 |
| tel_management_conn \| with imputed \| SITMAP #1 | levels | 243 | 242 |
| tel_management_conn \| with imputed \| OBSERVER #1 | levels | 48 | 47 |
| tel_management_conn_a0.5 \| with imputed \| SITMAP #1 | levels | 243 | 242 |
| tel_management_conn_a0.5 \| with imputed \| OBSERVER #1 | levels | 48 | 47 |
| tel_management_conn_a2 \| with imputed \| SITMAP #1 | levels | 243 | 242 |
| tel_management_conn_a2 \| with imputed \| OBSERVER #1 | levels | 48 | 47 |
| tel_management_site \| with imputed \| SITMAP #1 | levels | 243 | 242 |
| tel_management_site \| with imputed \| OBSERVER #1 | levels | 48 | 47 |
| tel_management_site \| with imputed \| SITE_ID #1 | levels | 1071 | 1063 |
| tel_management_site0 \| with imputed \| SITMAP #1 | levels | 243 | 242 |
| tel_management_site0 \| with imputed \| OBSERVER #1 | levels | 48 | 47 |
| tel_management_site0 \| with imputed \| SITE_ID_0M #1 | levels | 1135 | 1127 |
| tel_management_site200 \| with imputed \| SITMAP #1 | levels | 243 | 242 |
| tel_management_site200 \| with imputed \| OBSERVER #1 | levels | 48 | 47 |
| tel_management_site200 \| with imputed \| SITE_ID_200M #1 | levels | 990 | 983 |
| tel_management_2021 \| with imputed \| OBSERVER #1 | levels | 34 | 33 |
| tel_management_site_obs \| observed only \| SITE_ID #1 | levels | 358 | 356 |
| tel_management_site0_obs \| observed only \| SITE_ID_0M #1 | levels | 375 | 373 |
| tel_management_site200_obs \| observed only \| SITE_ID_200M #1 | levels | 340 | 338 |
| tel_baseline \| with imputed \| SITMAP #1 | variance | 1.92493657790452 | 1.9164836021735 |
| tel_baseline \| with imputed \| YEAR #1 | variance | 4.20305911353259e-09 | 3.12082035516689e-09 |
| tel_baseline \| with imputed \| OBSERVER #1 | variance | 1.06243117852195 | 1.06325557618253 |
| tel_management_null \| with imputed \| SITMAP #1 | variance | 2.51428223609939 | 2.5188089131237 |
| tel_management_null \| with imputed \| OBSERVER #1 | variance | 0.910025129659257 | 0.895403448695195 |
| tel_management \| with imputed \| SITMAP #1 | variance | 1.61700748433805 | 1.61139484722706 |
| tel_management \| with imputed \| YEAR #1 | variance | 5.59413973070603e-09 | 8.1368874095011e-09 |
| tel_management \| with imputed \| OBSERVER #1 | variance | 1.105123977047 | 1.09982561546442 |
| tel_management_conn \| with imputed \| SITMAP #1 | variance | 0.263380689313682 | 0.24898381761012 |
| tel_management_conn \| with imputed \| YEAR #1 | variance | 1.24081359619987e-08 | 1.45224735786134e-08 |
| tel_management_conn \| with imputed \| OBSERVER #1 | variance | 0.239551814061608 | 0.234658255800544 |
| tel_management_conn_a0.5 \| with imputed \| SITMAP #1 | variance | 0.272535821518104 | 0.253442036255714 |
| tel_management_conn_a0.5 \| with imputed \| YEAR #1 | variance | 0.0121690995342495 | 0.0176346622824274 |
| tel_management_conn_a0.5 \| with imputed \| OBSERVER #1 | variance | 0.180576130669757 | 0.18336431988691 |
| tel_management_conn_a2 \| with imputed \| SITMAP #1 | variance | 0.254353950188976 | 0.234878696104773 |
| tel_management_conn_a2 \| with imputed \| OBSERVER #1 | variance | 0.364179328009423 | 0.356772282056518 |
| tel_management_site \| with imputed \| SITMAP #1 | variance | 1.61700659869011 | 1.61139482740354 |
| tel_management_site \| with imputed \| YEAR #1 | variance | 2.18358647428716e-10 | 1.482782499445e-09 |
| tel_management_site \| with imputed \| OBSERVER #1 | variance | 1.10512405537361 | 1.09982545261059 |
| tel_management_site \| with imputed \| SITE_ID #1 | variance | 9.31812217985725e-09 | 8.14290294245889e-09 |
| tel_management_site0 \| with imputed \| SITMAP #1 | variance | 1.61700743033952 | 1.6113947967855 |
| tel_management_site0 \| with imputed \| YEAR #1 | variance | 1.12059904591188e-09 | 2.71027273937503e-09 |
| tel_management_site0 \| with imputed \| OBSERVER #1 | variance | 1.10512415304753 | 1.09982549936879 |
| tel_management_site0 \| with imputed \| SITE_ID_0M #1 | variance | 3.5355665325579e-09 | 2.48333014382489e-09 |
| tel_management_site200 \| with imputed \| SITMAP #1 | variance | 1.61700776740987 | 1.61139432756898 |
| tel_management_site200 \| with imputed \| OBSERVER #1 | variance | 1.10512369411788 | 1.09982556422322 |
| tel_management_site200 \| with imputed \| SITE_ID_200M #1 | variance | 1.04544639716192e-08 | 1.33510465281231e-08 |
| tel_management_2021 \| with imputed \| SITMAP #1 | variance | 2.3813562699956 | 2.4187113705735 |
| tel_management_2021 \| with imputed \| OBSERVER #1 | variance | 1.08777758060845e-08 | 1.37383092866827e-08 |
| tel_baseline_obs \| observed only \| SITMAP #1 | variance | 4.6927955274013 | 4.6872439464028 |
| tel_baseline_obs \| observed only \| YEAR #1 | variance | 3.60146177080618 | 3.59985816089852 |
| tel_baseline_obs \| observed only \| OBSERVER #1 | variance | 9.94859026256369 | 9.94276229061283 |
| tel_management_null_obs \| observed only \| SITMAP #1 | variance | 10.1811787995597 | 9.95619435006959 |
| tel_management_null_obs \| observed only \| YEAR #1 | variance | 10.9352165561045 | 10.9676884880422 |
| tel_management_null_obs \| observed only \| OBSERVER #1 | variance | 18.3643783852424 | 18.0159407384097 |

_Showing the first 60 of 153 rows._

### revised__18_moran.csv

| row | column | before | after |
|---|---|---|---|
| tel_baseline \| with imputed \| FALSE #1 | sites | 2173 | 2162 |
| tel_management \| with imputed \| FALSE #1 | sites | 1071 | 1063 |
| tel_baseline_obs \| observed only \| FALSE #1 | sites | 797 | 795 |
| tel_management_obs \| observed only \| FALSE #1 | sites | 358 | 356 |
| tel_baseline \| with imputed \| FALSE #1 | moran_observed | -0.00496043040826419 | -0.00292718348461395 |
| tel_management \| with imputed \| FALSE #1 | moran_observed | -0.00594081801758711 | -0.0097870068272199 |
| tel_baseline_obs \| observed only \| FALSE #1 | moran_observed | -0.00144952105310588 | -0.0183897336812706 |
| tel_management_obs \| observed only \| FALSE #1 | moran_observed | -0.00546769336830013 | -0.00609640566476176 |
| tel_baseline \| with imputed \| FALSE #1 | moran_expected | -0.000460405156537753 | -0.000462748727441 |
| tel_management \| with imputed \| FALSE #1 | moran_expected | -0.000934579439252336 | -0.000941619585687382 |
| tel_baseline_obs \| observed only \| FALSE #1 | moran_expected | -0.00125628140703518 | -0.00125944584382872 |
| tel_management_obs \| observed only \| FALSE #1 | moran_expected | -0.00280112044817927 | -0.0028169014084507 |
| tel_baseline \| with imputed \| FALSE #1 | p_value | 0.19863606913786 | 0.484127974660246 |
| tel_management \| with imputed \| FALSE #1 | p_value | 0.441004091949713 | 0.176487517900316 |
| tel_baseline_obs \| observed only \| FALSE #1 | p_value | 0.984086827292166 | 0.0779461132441264 |
| tel_management_obs \| observed only \| FALSE #1 | p_value | 0.864548049242354 | 0.834822183241975 |
| tel_baseline \| with imputed \| FALSE #1 | AIC_grid | 1181.22057781924 | 1180.83221598233 |
| tel_management \| with imputed \| FALSE #1 | AIC_grid | 620.990300319805 | 620.134687283459 |
| tel_baseline_obs \| observed only \| FALSE #1 | AIC_grid | 531.867160721892 | 531.853712328314 |
| tel_management_obs \| observed only \| FALSE #1 | AIC_grid | 241.866283814464 | 240.918368957607 |

### revised__19_teleius_coefficients.csv

| row | column | before | after |
|---|---|---|---|
| tel_threat_HarmfulMow \| with imputed \| (Intercept) #1 | estimate | -2.77543872689733 | -2.78022200491418 |
| tel_threat_HarmfulMow \| with imputed \| HarmfulMow #1 | estimate | -0.148526055357785 | -0.146435830380588 |
| tel_threat_HarmfulMow \| with imputed \| ERA2021-2024 #1 | estimate | -0.014702081327185 | -0.0147845901726354 |
| tel_threat_HarmfulMow \| with imputed \| DOY_STD #1 | estimate | -0.851329376020609 | -0.867059315480103 |
| tel_threat_HarmfulMow \| with imputed \| I(DOY_STD^2) #1 | estimate | 0.0377823562500055 | 0.0534262255967698 |
| tel_threat_HarmfulGrazing \| with imputed \| (Intercept) #1 | estimate | -2.8631330654671 | -2.86628249122472 |
| tel_threat_HarmfulGrazing \| with imputed \| HarmfulGrazing #1 | estimate | 0.207826281022066 | 0.201838979598606 |
| tel_threat_HarmfulGrazing \| with imputed \| ERA2021-2024 #1 | estimate | 0.0303451170217785 | 0.0295143500089557 |
| tel_threat_HarmfulGrazing \| with imputed \| DOY_STD #1 | estimate | -0.851404939857696 | -0.86672333134909 |
| tel_threat_HarmfulGrazing \| with imputed \| I(DOY_STD^2) #1 | estimate | 0.0349162223991823 | 0.050131630322094 |
| tel_threat_GrazingByeffects \| with imputed \| (Intercept) #1 | estimate | -2.86025994790964 | -2.86331422609861 |
| tel_threat_GrazingByeffects \| with imputed \| GrazingByeffects #1 | estimate | -0.235526392379436 | -0.222078310007286 |
| tel_threat_GrazingByeffects \| with imputed \| ERA2021-2024 #1 | estimate | 0.0428164957349931 | 0.0412603351096877 |
| tel_threat_GrazingByeffects \| with imputed \| DOY_STD #1 | estimate | -0.862476840398039 | -0.877446371000886 |
| tel_threat_GrazingByeffects \| with imputed \| I(DOY_STD^2) #1 | estimate | 0.0401130503904195 | 0.0552216892595854 |
| tel_threat_NativeDominants \| with imputed \| (Intercept) #1 | estimate | -2.85492796295055 | -2.85873998344524 |
| tel_threat_NativeDominants \| with imputed \| NativeDominants #1 | estimate | -0.512862225648722 | -0.512239237392174 |
| tel_threat_NativeDominants \| with imputed \| ERA2021-2024 #1 | estimate | 0.0876692474189513 | 0.0868181728711596 |
| tel_threat_NativeDominants \| with imputed \| DOY_STD #1 | estimate | -0.855677177552972 | -0.871802849465332 |
| tel_threat_NativeDominants \| with imputed \| I(DOY_STD^2) #1 | estimate | 0.0369159808022345 | 0.0530214142946175 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| (Intercept) #1 | estimate | -2.79217681024995 | -2.79584350388677 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| AbioticNaturalProcesses #1 | estimate | -0.501381598448105 | -0.504864845789512 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| ERA2021-2024 #1 | estimate | 0.0198544482519179 | 0.0187492858327109 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| DOY_STD #1 | estimate | -0.848524310166664 | -0.865518590791704 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| I(DOY_STD^2) #1 | estimate | 0.0377448018043514 | 0.054649269275939 |
| tel_threat_Encroachment \| with imputed \| (Intercept) #1 | estimate | -2.86761764417202 | -2.87058776139762 |
| tel_threat_Encroachment \| with imputed \| Encroachment #1 | estimate | 0.0383476284981924 | 0.0359148218611867 |
| tel_threat_Encroachment \| with imputed \| ERA2021-2024 #1 | estimate | 0.0394619022337384 | 0.0382185516482435 |
| tel_threat_Encroachment \| with imputed \| DOY_STD #1 | estimate | -0.86292340664004 | -0.878851852852608 |
| tel_threat_Encroachment \| with imputed \| I(DOY_STD^2) #1 | estimate | 0.0403009878010453 | 0.0563349695887675 |
| tel_threat_BiomassAccumulation \| with imputed \| (Intercept) #1 | estimate | -2.89557869146706 | -2.89860328554251 |
| tel_threat_BiomassAccumulation \| with imputed \| BiomassAccumulation #1 | estimate | 0.139234862280936 | 0.136973665127591 |
| tel_threat_BiomassAccumulation \| with imputed \| ERA2021-2024 #1 | estimate | 0.0591837794533168 | 0.0578162468484343 |
| tel_threat_BiomassAccumulation \| with imputed \| DOY_STD #1 | estimate | -0.863422338985802 | -0.879316190382292 |
| tel_threat_BiomassAccumulation \| with imputed \| I(DOY_STD^2) #1 | estimate | 0.0403442754128926 | 0.0562498479881842 |
| tel_threat_Eutrophization \| with imputed \| (Intercept) #1 | estimate | -2.77934804947653 | -2.78267073545472 |
| tel_threat_Eutrophization \| with imputed \| Eutrophization #1 | estimate | -0.567979819581963 | -0.568095451286648 |
| tel_threat_Eutrophization \| with imputed \| ERA2021-2024 #1 | estimate | 0.0625521346313914 | 0.0610624474759471 |
| tel_threat_Eutrophization \| with imputed \| DOY_STD #1 | estimate | -0.856851706494687 | -0.87177668811246 |
| tel_threat_Eutrophization \| with imputed \| I(DOY_STD^2) #1 | estimate | 0.0452333570293169 | 0.0604257542061409 |
| tel_threat_HarmfulMow_obs \| observed only \| (Intercept) #1 | estimate | 2.82168165436077 | 2.81939381556224 |
| tel_threat_HarmfulMow_obs \| observed only \| HarmfulMow #1 | estimate | -0.228023515998031 | -0.226688084943775 |
| tel_threat_HarmfulMow_obs \| observed only \| ERA2021-2024 #1 | estimate | -2.04157171141959 | -2.04308672322173 |
| tel_threat_HarmfulMow_obs \| observed only \| DOY_STD #1 | estimate | -1.3848510866372 | -1.39214230047203 |
| tel_threat_HarmfulMow_obs \| observed only \| I(DOY_STD^2) #1 | estimate | 0.191542655559911 | 0.198118706739901 |
| tel_threat_HarmfulGrazing_obs \| observed only \| (Intercept) #1 | estimate | 2.61477769379697 | 2.6130488887138 |
| tel_threat_HarmfulGrazing_obs \| observed only \| HarmfulGrazing #1 | estimate | 0.36776679147824 | 0.36735652386329 |
| tel_threat_HarmfulGrazing_obs \| observed only \| ERA2021-2024 #1 | estimate | -1.92157641602586 | -1.92380306847216 |
| tel_threat_HarmfulGrazing_obs \| observed only \| DOY_STD #1 | estimate | -1.39604869288827 | -1.4040765033473 |
| tel_threat_HarmfulGrazing_obs \| observed only \| I(DOY_STD^2) #1 | estimate | 0.196085433620808 | 0.203437037639133 |
| tel_threat_NativeDominants_obs \| observed only \| (Intercept) #1 | estimate | 2.71024823631877 | 2.70844622828007 |
| tel_threat_NativeDominants_obs \| observed only \| NativeDominants #1 | estimate | -0.889279207379659 | -0.886580377297311 |
| tel_threat_NativeDominants_obs \| observed only \| ERA2021-2024 #1 | estimate | -1.89000143384973 | -1.8919795949029 |
| tel_threat_NativeDominants_obs \| observed only \| DOY_STD #1 | estimate | -1.39083376132571 | -1.39796076124508 |
| tel_threat_NativeDominants_obs \| observed only \| I(DOY_STD^2) #1 | estimate | 0.186608276989577 | 0.193147663128164 |
| tel_threat_AbioticNaturalProcesses_obs \| observed only \| (Intercept) #1 | estimate | 3.10644942277195 | 3.10348931710672 |
| tel_threat_AbioticNaturalProcesses_obs \| observed only \| AbioticNaturalProcesses #1 | estimate | -2.28242181717308 | -2.28081079243914 |
| tel_threat_AbioticNaturalProcesses_obs \| observed only \| ERA2021-2024 #1 | estimate | -2.28048932391549 | -2.28151417485565 |
| tel_threat_AbioticNaturalProcesses_obs \| observed only \| DOY_STD #1 | estimate | -1.424013820249 | -1.43196062116931 |
| tel_threat_AbioticNaturalProcesses_obs \| observed only \| I(DOY_STD^2) #1 | estimate | 0.195697882154102 | 0.202873584591034 |

_Showing the first 60 of 667 rows._

### revised__19_teleius_diagnostics.csv

| row | column | before | after |
|---|---|---|---|
| tel_threat_HarmfulMow \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | max_gvif_adjusted | 1.10063493269877 | 1.09990375203031 |
| tel_threat_HarmfulGrazing \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | max_gvif_adjusted | 1.00493988347274 | 1.00531137428781 |
| tel_threat_GrazingByeffects \| with imputed \| TRUE \| FALSE \| <NA> #1 | max_gvif_adjusted | 1.00598328562344 | 1.00530421235328 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| TRUE \| FALSE \| <NA> #1 | max_gvif_adjusted | 1.00762003727394 | 1.00746678243522 |
| tel_threat_Eutrophization \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | max_gvif_adjusted | 1.00906635965176 | 1.00964726977763 |
| tel_threat_HarmfulMow_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | max_gvif_adjusted | 1.09296278118917 | 1.09356886168808 |
| tel_threat_HarmfulGrazing_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | max_gvif_adjusted | 1.00356734675848 | 1.00400509986164 |
| tel_threat_NativeDominants_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | max_gvif_adjusted | 1.02251995400247 | 1.02329166107075 |
| tel_threat_AbioticNaturalProcesses_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | max_gvif_adjusted | 1.01557621207762 | 1.01549118073645 |
| tel_threat_Encroachment_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | max_gvif_adjusted | 1.00679075793497 | 1.0069874831741 |
| tel_threat_BiomassAccumulation_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | max_gvif_adjusted | 1.01054958831107 | 1.01097529403558 |
| tel_threat_Eutrophization_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | max_gvif_adjusted | 1.04176516097386 | 1.04310729966939 |
| tel_threat_HarmfulMow \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_uniformity_p | 0.800591992229358 | 0.0688909180409079 |
| tel_threat_HarmfulGrazing \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_uniformity_p | 0.807074197791208 | 0.071013628190082 |
| tel_threat_GrazingByeffects \| with imputed \| TRUE \| FALSE \| <NA> #1 | dharma_uniformity_p | 0.760447538650811 | 0.0612958498724313 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| TRUE \| FALSE \| <NA> #1 | dharma_uniformity_p | 0.738220710100428 | 0.071013628190082 |
| tel_threat_Eutrophization \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_uniformity_p | 0.744406865447766 | 0.0559148753926293 |
| tel_threat_HarmfulMow_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_uniformity_p | 0.259138037434981 | 0.124566020012271 |
| tel_threat_HarmfulGrazing_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_uniformity_p | 0.22404158422156 | 0.165699504287445 |
| tel_threat_NativeDominants_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_uniformity_p | 0.277033386998052 | 0.136195091485658 |
| tel_threat_AbioticNaturalProcesses_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_uniformity_p | 0.167066962048572 | 0.174681771113401 |
| tel_threat_Encroachment_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_uniformity_p | 0.253764410079068 | 0.151081168995889 |
| tel_threat_BiomassAccumulation_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_uniformity_p | 0.188836354451769 | 0.0767135959564909 |
| tel_threat_Eutrophization_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_uniformity_p | 0.294585836962362 | 0.106734927687589 |
| tel_threat_HarmfulMow \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_dispersion_p | 0.4 | 0.464 |
| tel_threat_HarmfulGrazing \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_dispersion_p | 0.4 | 0.456 |
| tel_threat_GrazingByeffects \| with imputed \| TRUE \| FALSE \| <NA> #1 | dharma_dispersion_p | 0.4 | 0.464 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| TRUE \| FALSE \| <NA> #1 | dharma_dispersion_p | 0.408 | 0.456 |
| tel_threat_Eutrophization \| with imputed \| TRUE \| FALSE \| random-effect variance at zero #1 | dharma_dispersion_p | 0.408 | 0.464 |
| tel_threat_HarmfulMow_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_dispersion_p | 0.76 | 0.704 |
| tel_threat_HarmfulGrazing_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_dispersion_p | 0.76 | 0.704 |
| tel_threat_NativeDominants_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_dispersion_p | 0.672 | 0.648 |
| tel_threat_AbioticNaturalProcesses_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_dispersion_p | 0.76 | 0.72 |
| tel_threat_Encroachment_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_dispersion_p | 0.776 | 0.728 |
| tel_threat_BiomassAccumulation_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_dispersion_p | 0.608 | 0.664 |
| tel_threat_Eutrophization_obs \| observed only \| TRUE \| FALSE \| <NA> #1 | dharma_dispersion_p | 0.744 | 0.72 |

### revised__19_teleius_fit_statistics.csv

| row | column | before | after |
|---|---|---|---|
| tel_threat_HarmfulMow \| HarmfulMow \| Threat \| with imputed \| ok #1 | n_obs | 2117 | 2109 |
| tel_threat_HarmfulGrazing \| HarmfulGrazing \| Threat \| with imputed \| ok #1 | n_obs | 2117 | 2109 |
| tel_threat_GrazingByeffects \| GrazingByeffects \| Threat \| with imputed \| ok #1 | n_obs | 2117 | 2109 |
| tel_threat_NativeDominants \| NativeDominants \| Threat \| with imputed \| ok #1 | n_obs | 2117 | 2109 |
| tel_threat_AbioticNaturalProcesses \| AbioticNaturalProcesses \| Threat \| with imputed \| ok #1 | n_obs | 2117 | 2109 |
| tel_threat_Encroachment \| Encroachment \| Threat \| with imputed \| ok #1 | n_obs | 2117 | 2109 |
| tel_threat_BiomassAccumulation \| BiomassAccumulation \| Threat \| with imputed \| ok #1 | n_obs | 2117 | 2109 |
| tel_threat_Eutrophization \| Eutrophization \| Threat \| with imputed \| ok #1 | n_obs | 2117 | 2109 |
| tel_threat_HarmfulMow_obs \| HarmfulMow \| Threat \| observed only \| ok #1 | n_obs | 618 | 617 |
| tel_threat_HarmfulGrazing_obs \| HarmfulGrazing \| Threat \| observed only \| ok #1 | n_obs | 618 | 617 |
| tel_threat_NativeDominants_obs \| NativeDominants \| Threat \| observed only \| ok #1 | n_obs | 618 | 617 |
| tel_threat_AbioticNaturalProcesses_obs \| AbioticNaturalProcesses \| Threat \| observed only \| ok #1 | n_obs | 618 | 617 |
| tel_threat_Encroachment_obs \| Encroachment \| Threat \| observed only \| ok #1 | n_obs | 618 | 617 |
| tel_threat_BiomassAccumulation_obs \| BiomassAccumulation \| Threat \| observed only \| ok #1 | n_obs | 618 | 617 |
| tel_threat_Eutrophization_obs \| Eutrophization \| Threat \| observed only \| ok #1 | n_obs | 618 | 617 |
| tel_threat_HarmfulMow \| HarmfulMow \| Threat \| with imputed \| ok #1 | AIC | 980.92947795413 | 980.563075336402 |
| tel_threat_HarmfulGrazing \| HarmfulGrazing \| Threat \| with imputed \| ok #1 | AIC | 980.901038897866 | 980.548424946486 |
| tel_threat_GrazingByeffects \| GrazingByeffects \| Threat \| with imputed \| ok #1 | AIC | 981.187529858886 | 980.827427240433 |
| tel_threat_NativeDominants \| NativeDominants \| Threat \| with imputed \| ok #1 | AIC | 980.26360178428 | 979.886816975461 |
| tel_threat_AbioticNaturalProcesses \| AbioticNaturalProcesses \| Threat \| with imputed \| ok #1 | AIC | 980.121056059741 | 979.725763762347 |
| tel_threat_Encroachment \| Encroachment \| Threat \| with imputed \| ok #1 | AIC | 981.315865151007 | 980.940870125833 |
| tel_threat_BiomassAccumulation \| BiomassAccumulation \| Threat \| with imputed \| ok #1 | AIC | 981.086536767269 | 980.717115698258 |
| tel_threat_Eutrophization \| Eutrophization \| Threat \| with imputed \| ok #1 | AIC | 978.247003715431 | 977.867576152083 |
| tel_threat_HarmfulMow_obs \| HarmfulMow \| Threat \| observed only \| ok #1 | AIC | 380.361767470567 | 380.344975154029 |
| tel_threat_HarmfulGrazing_obs \| HarmfulGrazing \| Threat \| observed only \| ok #1 | AIC | 380.30224675187 | 380.283055734722 |
| tel_threat_NativeDominants_obs \| NativeDominants \| Threat \| observed only \| ok #1 | AIC | 379.970426217373 | 379.953797331048 |
| tel_threat_AbioticNaturalProcesses_obs \| AbioticNaturalProcesses \| Threat \| observed only \| ok #1 | AIC | 374.14115399082 | 374.122794251876 |
| tel_threat_Encroachment_obs \| Encroachment \| Threat \| observed only \| ok #1 | AIC | 380.590658639278 | 380.570612866742 |
| tel_threat_BiomassAccumulation_obs \| BiomassAccumulation \| Threat \| observed only \| ok #1 | AIC | 376.964925321311 | 376.94770699454 |
| tel_threat_Eutrophization_obs \| Eutrophization \| Threat \| observed only \| ok #1 | AIC | 377.976216401966 | 377.959399656632 |
| tel_threat_HarmfulMow \| HarmfulMow \| Threat \| with imputed \| ok #1 | BIC | 1026.19152012321 | 1025.79482878023 |
| tel_threat_HarmfulGrazing \| HarmfulGrazing \| Threat \| with imputed \| ok #1 | BIC | 1026.16308106694 | 1025.78017839032 |
| tel_threat_GrazingByeffects \| GrazingByeffects \| Threat \| with imputed \| ok #1 | BIC | 1026.44957202796 | 1026.05918068426 |
| tel_threat_NativeDominants \| NativeDominants \| Threat \| with imputed \| ok #1 | BIC | 1025.52564395336 | 1025.11857041929 |
| tel_threat_AbioticNaturalProcesses \| AbioticNaturalProcesses \| Threat \| with imputed \| ok #1 | BIC | 1025.38309822882 | 1024.95751720618 |
| tel_threat_Encroachment \| Encroachment \| Threat \| with imputed \| ok #1 | BIC | 1026.57790732009 | 1026.17262356966 |
| tel_threat_BiomassAccumulation \| BiomassAccumulation \| Threat \| with imputed \| ok #1 | BIC | 1026.34857893635 | 1025.94886914209 |
| tel_threat_Eutrophization \| Eutrophization \| Threat \| with imputed \| ok #1 | BIC | 1023.50904588451 | 1023.09932959591 |
| tel_threat_HarmfulMow_obs \| HarmfulMow \| Threat \| observed only \| ok #1 | BIC | 415.773675130229 | 415.743927345272 |
| tel_threat_HarmfulGrazing_obs \| HarmfulGrazing \| Threat \| observed only \| ok #1 | BIC | 415.714154411531 | 415.682007925965 |
| tel_threat_NativeDominants_obs \| NativeDominants \| Threat \| observed only \| ok #1 | BIC | 415.382333877035 | 415.352749522292 |
| tel_threat_AbioticNaturalProcesses_obs \| AbioticNaturalProcesses \| Threat \| observed only \| ok #1 | BIC | 409.553061650482 | 409.52174644312 |
| tel_threat_Encroachment_obs \| Encroachment \| Threat \| observed only \| ok #1 | BIC | 416.00256629894 | 415.969565057985 |
| tel_threat_BiomassAccumulation_obs \| BiomassAccumulation \| Threat \| observed only \| ok #1 | BIC | 412.376832980972 | 412.346659185783 |
| tel_threat_Eutrophization_obs \| Eutrophization \| Threat \| observed only \| ok #1 | BIC | 413.388124061627 | 413.358351847875 |
| tel_threat_HarmfulMow \| HarmfulMow \| Threat \| with imputed \| ok #1 | logLik | -482.464738977065 | -482.281537668201 |
| tel_threat_HarmfulGrazing \| HarmfulGrazing \| Threat \| with imputed \| ok #1 | logLik | -482.450519448933 | -482.274212473243 |
| tel_threat_GrazingByeffects \| GrazingByeffects \| Threat \| with imputed \| ok #1 | logLik | -482.593764929443 | -482.413713620216 |
| tel_threat_NativeDominants \| NativeDominants \| Threat \| with imputed \| ok #1 | logLik | -482.13180089214 | -481.94340848773 |
| tel_threat_AbioticNaturalProcesses \| AbioticNaturalProcesses \| Threat \| with imputed \| ok #1 | logLik | -482.060528029871 | -481.862881881174 |
| tel_threat_Encroachment \| Encroachment \| Threat \| with imputed \| ok #1 | logLik | -482.657932575504 | -482.470435062917 |
| tel_threat_BiomassAccumulation \| BiomassAccumulation \| Threat \| with imputed \| ok #1 | logLik | -482.543268383634 | -482.358557849129 |
| tel_threat_Eutrophization \| Eutrophization \| Threat \| with imputed \| ok #1 | logLik | -481.123501857716 | -480.933788076042 |
| tel_threat_HarmfulMow_obs \| HarmfulMow \| Threat \| observed only \| ok #1 | logLik | -182.180883735284 | -182.172487577015 |
| tel_threat_HarmfulGrazing_obs \| HarmfulGrazing \| Threat \| observed only \| ok #1 | logLik | -182.151123375935 | -182.141527867361 |
| tel_threat_NativeDominants_obs \| NativeDominants \| Threat \| observed only \| ok #1 | logLik | -181.985213108687 | -181.976898665524 |
| tel_threat_AbioticNaturalProcesses_obs \| AbioticNaturalProcesses \| Threat \| observed only \| ok #1 | logLik | -179.07057699541 | -179.061397125938 |
| tel_threat_Encroachment_obs \| Encroachment \| Threat \| observed only \| ok #1 | logLik | -182.295329319639 | -182.285306433371 |
| tel_threat_BiomassAccumulation_obs \| BiomassAccumulation \| Threat \| observed only \| ok #1 | logLik | -180.482462660655 | -180.47385349727 |
| tel_threat_Eutrophization_obs \| Eutrophization \| Threat \| observed only \| ok #1 | logLik | -180.988108200983 | -180.979699828316 |

### revised__19_teleius_gvif.csv

| row | column | before | after |
|---|---|---|---|
| tel_threat_HarmfulMow \| with imputed \| HarmfulMow #1 | GVIF | 1.21139725507683 | 1.20978826373034 |
| tel_threat_HarmfulMow \| with imputed \| ERA #1 | GVIF | 1.20361889287732 | 1.20275622697478 |
| tel_threat_HarmfulMow \| with imputed \| DOY_STD #1 | GVIF | 1.00827136133937 | 1.00742063825096 |
| tel_threat_HarmfulGrazing \| with imputed \| HarmfulGrazing #1 | GVIF | 1.0099041693942 | 1.01065095927244 |
| tel_threat_HarmfulGrazing \| with imputed \| ERA #1 | GVIF | 1.00455204585474 | 1.00457709590221 |
| tel_threat_HarmfulGrazing \| with imputed \| DOY_STD #1 | GVIF | 1.00535655036903 | 1.00606708086952 |
| tel_threat_GrazingByeffects \| with imputed \| GrazingByeffects #1 | GVIF | 1.01200237095374 | 1.01063655937526 |
| tel_threat_GrazingByeffects \| with imputed \| ERA #1 | GVIF | 1.009319892037 | 1.00903974623555 |
| tel_threat_GrazingByeffects \| with imputed \| DOY_STD #1 | GVIF | 1.00268626649669 | 1.0016032505218 |
| tel_threat_NativeDominants \| with imputed \| NativeDominants #1 | GVIF | 1.08032167063119 | 1.08128812716374 |
| tel_threat_NativeDominants \| with imputed \| ERA #1 | GVIF | 1.07726845182532 | 1.07768977397678 |
| tel_threat_NativeDominants \| with imputed \| DOY_STD #1 | GVIF | 1.00294994613356 | 1.00349116066022 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| AbioticNaturalProcesses #1 | GVIF | 1.01529813951594 | 1.01498931771037 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| ERA #1 | GVIF | 1.00162547061117 | 1.00161785447097 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| DOY_STD #1 | GVIF | 1.01377929143101 | 1.01345351352433 |
| tel_threat_Encroachment \| with imputed \| Encroachment #1 | GVIF | 1.03821588599934 | 1.03963571132229 |
| tel_threat_Encroachment \| with imputed \| ERA #1 | GVIF | 1.02163590584644 | 1.02135563242426 |
| tel_threat_Encroachment \| with imputed \| DOY_STD #1 | GVIF | 1.01683967322141 | 1.01849897556006 |
| tel_threat_BiomassAccumulation \| with imputed \| BiomassAccumulation #1 | GVIF | 1.02746294860294 | 1.02747675445353 |
| tel_threat_BiomassAccumulation \| with imputed \| ERA #1 | GVIF | 1.02452249190013 | 1.02417525584268 |
| tel_threat_BiomassAccumulation \| with imputed \| DOY_STD #1 | GVIF | 1.00308938745443 | 1.00342340774927 |
| tel_threat_Eutrophization \| with imputed \| Eutrophization #1 | GVIF | 1.01821491818086 | 1.01938760936943 |
| tel_threat_Eutrophization \| with imputed \| ERA #1 | GVIF | 1.00083451263314 | 1.00087498872286 |
| tel_threat_Eutrophization \| with imputed \| DOY_STD #1 | GVIF | 1.01747706152271 | 1.01858883962032 |
| tel_threat_HarmfulMow_obs \| observed only \| HarmfulMow #1 | GVIF | 1.19456764106477 | 1.19589285525376 |
| tel_threat_HarmfulMow_obs \| observed only \| ERA #1 | GVIF | 1.18619302073189 | 1.1901139823534 |
| tel_threat_HarmfulMow_obs \| observed only \| DOY_STD #1 | GVIF | 1.01208608696592 | 1.0115647388765 |
| tel_threat_HarmfulGrazing_obs \| observed only \| HarmfulGrazing #1 | GVIF | 1.00516019053903 | 1.0050508851114 |
| tel_threat_HarmfulGrazing_obs \| observed only \| ERA #1 | GVIF | 1.00634063938771 | 1.00741397900949 |
| tel_threat_HarmfulGrazing_obs \| observed only \| DOY_STD #1 | GVIF | 1.00714741947985 | 1.00802624054818 |
| tel_threat_NativeDominants_obs \| observed only \| NativeDominants #1 | GVIF | 1.04325325884276 | 1.04390649190156 |
| tel_threat_NativeDominants_obs \| observed only \| ERA #1 | GVIF | 1.0455470563332 | 1.04712582361693 |
| tel_threat_NativeDominants_obs \| observed only \| DOY_STD #1 | GVIF | 1.00795275553776 | 1.00940999662925 |
| tel_threat_AbioticNaturalProcesses_obs \| observed only \| AbioticNaturalProcesses #1 | GVIF | 1.03139504253794 | 1.03122233815352 |
| tel_threat_AbioticNaturalProcesses_obs \| observed only \| ERA #1 | GVIF | 1.02171490004001 | 1.02277122279491 |
| tel_threat_AbioticNaturalProcesses_obs \| observed only \| DOY_STD #1 | GVIF | 1.01641852911351 | 1.01709402937594 |
| tel_threat_Encroachment_obs \| observed only \| Encroachment #1 | GVIF | 1.01362763026327 | 1.01402379126931 |
| tel_threat_Encroachment_obs \| observed only \| ERA #1 | GVIF | 1.01164082843555 | 1.01240323553092 |
| tel_threat_Encroachment_obs \| observed only \| DOY_STD #1 | GVIF | 1.00979752640452 | 1.01139893532937 |
| tel_threat_BiomassAccumulation_obs \| observed only \| BiomassAccumulation #1 | GVIF | 1.01736673193668 | 1.01703916641989 |
| tel_threat_BiomassAccumulation_obs \| observed only \| ERA #1 | GVIF | 1.02121047043568 | 1.02207104515032 |
| tel_threat_BiomassAccumulation_obs \| observed only \| DOY_STD #1 | GVIF | 1.00534258386689 | 1.00626506070257 |
| tel_threat_Eutrophization_obs \| observed only \| Eutrophization #1 | GVIF | 1.08527465061888 | 1.08807283862357 |
| tel_threat_Eutrophization_obs \| observed only \| ERA #1 | GVIF | 1.02093217995499 | 1.02121842931579 |
| tel_threat_Eutrophization_obs \| observed only \| DOY_STD #1 | GVIF | 1.06869410024993 | 1.07280753133645 |
| tel_threat_HarmfulMow \| with imputed \| HarmfulMow #1 | GVIF_adjusted | 1.10063493269877 | 1.09990375203031 |
| tel_threat_HarmfulMow \| with imputed \| ERA #1 | GVIF_adjusted | 1.09709566259161 | 1.09670243319452 |
| tel_threat_HarmfulMow \| with imputed \| DOY_STD #1 | GVIF_adjusted | 1.00412716392864 | 1.00370346131263 |
| tel_threat_HarmfulGrazing \| with imputed \| HarmfulGrazing #1 | GVIF_adjusted | 1.00493988347274 | 1.00531137428781 |
| tel_threat_HarmfulGrazing \| with imputed \| ERA #1 | GVIF_adjusted | 1.00227343866568 | 1.00228593520123 |
| tel_threat_HarmfulGrazing \| with imputed \| DOY_STD #1 | GVIF_adjusted | 1.00267469817934 | 1.00302895315615 |
| tel_threat_GrazingByeffects \| with imputed \| GrazingByeffects #1 | GVIF_adjusted | 1.00598328562344 | 1.00530421235328 |
| tel_threat_GrazingByeffects \| with imputed \| ERA #1 | GVIF_adjusted | 1.00464913877284 | 1.00450970440089 |
| tel_threat_GrazingByeffects \| with imputed \| DOY_STD #1 | GVIF_adjusted | 1.00134223245436 | 1.00080130421668 |
| tel_threat_NativeDominants \| with imputed \| NativeDominants #1 | GVIF_adjusted | 1.03938523687379 | 1.03985005032636 |
| tel_threat_NativeDominants \| with imputed \| ERA #1 | GVIF_adjusted | 1.03791543577756 | 1.03811838148488 |
| tel_threat_NativeDominants \| with imputed \| DOY_STD #1 | GVIF_adjusted | 1.00147388689549 | 1.00174405945841 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| AbioticNaturalProcesses #1 | GVIF_adjusted | 1.00762003727394 | 1.00746678243522 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| ERA #1 | GVIF_adjusted | 1.0008124053044 | 1.00080860031825 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| DOY_STD #1 | GVIF_adjusted | 1.00686607422785 | 1.00670428305651 |

_Showing the first 60 of 90 rows._

### revised__19_teleius_random_effects.csv

| row | column | before | after |
|---|---|---|---|
| tel_threat_HarmfulMow \| with imputed \| SITMAP #1 | levels | 253 | 252 |
| tel_threat_HarmfulMow \| with imputed \| OBSERVER #1 | levels | 48 | 47 |
| tel_threat_HarmfulGrazing \| with imputed \| SITMAP #1 | levels | 253 | 252 |
| tel_threat_HarmfulGrazing \| with imputed \| OBSERVER #1 | levels | 48 | 47 |
| tel_threat_GrazingByeffects \| with imputed \| SITMAP #1 | levels | 253 | 252 |
| tel_threat_GrazingByeffects \| with imputed \| OBSERVER #1 | levels | 48 | 47 |
| tel_threat_NativeDominants \| with imputed \| SITMAP #1 | levels | 253 | 252 |
| tel_threat_NativeDominants \| with imputed \| OBSERVER #1 | levels | 48 | 47 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| SITMAP #1 | levels | 253 | 252 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| OBSERVER #1 | levels | 48 | 47 |
| tel_threat_Encroachment \| with imputed \| SITMAP #1 | levels | 253 | 252 |
| tel_threat_Encroachment \| with imputed \| OBSERVER #1 | levels | 48 | 47 |
| tel_threat_BiomassAccumulation \| with imputed \| SITMAP #1 | levels | 253 | 252 |
| tel_threat_BiomassAccumulation \| with imputed \| OBSERVER #1 | levels | 48 | 47 |
| tel_threat_Eutrophization \| with imputed \| SITMAP #1 | levels | 253 | 252 |
| tel_threat_Eutrophization \| with imputed \| OBSERVER #1 | levels | 48 | 47 |
| tel_threat_HarmfulMow \| with imputed \| SITMAP #1 | variance | 1.84864839782934 | 1.84234532795981 |
| tel_threat_HarmfulMow \| with imputed \| OBSERVER #1 | variance | 0.74188693469479 | 0.743305941264216 |
| tel_threat_HarmfulGrazing \| with imputed \| SITMAP #1 | variance | 1.82586795037222 | 1.82030252987496 |
| tel_threat_HarmfulGrazing \| with imputed \| OBSERVER #1 | variance | 0.734611908440729 | 0.736469997247773 |
| tel_threat_GrazingByeffects \| with imputed \| SITMAP #1 | variance | 1.85888020444314 | 1.85212155004351 |
| tel_threat_GrazingByeffects \| with imputed \| YEAR #1 | variance | 1.20330466584577e-08 | 1.00417238860136e-08 |
| tel_threat_GrazingByeffects \| with imputed \| OBSERVER #1 | variance | 0.749609975241999 | 0.750784119600561 |
| tel_threat_NativeDominants \| with imputed \| SITMAP #1 | variance | 1.85254430066909 | 1.84596521626256 |
| tel_threat_NativeDominants \| with imputed \| YEAR #1 | variance | 8.49872832377899e-09 | 1.00287610618166e-08 |
| tel_threat_NativeDominants \| with imputed \| OBSERVER #1 | variance | 0.697612036317008 | 0.699068747001126 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| SITMAP #1 | variance | 1.77675385624677 | 1.77005639712452 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| YEAR #1 | variance | 1.38198425892473e-08 | 1.06755909532616e-08 |
| tel_threat_AbioticNaturalProcesses \| with imputed \| OBSERVER #1 | variance | 0.74700822698469 | 0.748343201941425 |
| tel_threat_Encroachment \| with imputed \| SITMAP #1 | variance | 1.85272991777049 | 1.84602715909816 |
| tel_threat_Encroachment \| with imputed \| YEAR #1 | variance | 1.01280679732221e-08 | 6.89245251785523e-09 |
| tel_threat_Encroachment \| with imputed \| OBSERVER #1 | variance | 0.753750241662332 | 0.754642180120125 |
| tel_threat_BiomassAccumulation \| with imputed \| SITMAP #1 | variance | 1.87089744727091 | 1.86402478835549 |
| tel_threat_BiomassAccumulation \| with imputed \| YEAR #1 | variance | 1.35625067436308e-08 | 9.06354216950516e-09 |
| tel_threat_BiomassAccumulation \| with imputed \| OBSERVER #1 | variance | 0.743563076369765 | 0.744891533538897 |
| tel_threat_Eutrophization \| with imputed \| SITMAP #1 | variance | 1.76881923042336 | 1.76347985321928 |
| tel_threat_Eutrophization \| with imputed \| OBSERVER #1 | variance | 0.687395144720669 | 0.688162292774307 |
| tel_threat_HarmfulMow_obs \| observed only \| SITMAP #1 | variance | 4.17141054062763 | 4.15779806566554 |
| tel_threat_HarmfulMow_obs \| observed only \| YEAR #1 | variance | 6.20284695679588 | 6.20571167282055 |
| tel_threat_HarmfulMow_obs \| observed only \| OBSERVER #1 | variance | 9.32930767114305 | 9.31485941257868 |
| tel_threat_HarmfulGrazing_obs \| observed only \| SITMAP #1 | variance | 4.03786296784899 | 4.02289631783563 |
| tel_threat_HarmfulGrazing_obs \| observed only \| YEAR #1 | variance | 6.02960303742197 | 6.03259491752268 |
| tel_threat_HarmfulGrazing_obs \| observed only \| OBSERVER #1 | variance | 9.30132873132724 | 9.28538928181144 |
| tel_threat_NativeDominants_obs \| observed only \| SITMAP #1 | variance | 4.27897054891546 | 4.26488806151694 |
| tel_threat_NativeDominants_obs \| observed only \| YEAR #1 | variance | 6.30494584970405 | 6.30735919357481 |
| tel_threat_NativeDominants_obs \| observed only \| OBSERVER #1 | variance | 9.23671918516038 | 9.22221784118397 |
| tel_threat_AbioticNaturalProcesses_obs \| observed only \| SITMAP #1 | variance | 4.5066584238976 | 4.49066047803573 |
| tel_threat_AbioticNaturalProcesses_obs \| observed only \| YEAR #1 | variance | 7.36557758139246 | 7.36540581355371 |
| tel_threat_AbioticNaturalProcesses_obs \| observed only \| OBSERVER #1 | variance | 9.25451589008959 | 9.23876109581992 |
| tel_threat_Encroachment_obs \| observed only \| SITMAP #1 | variance | 4.14045297804008 | 4.12451565380419 |
| tel_threat_Encroachment_obs \| observed only \| YEAR #1 | variance | 6.15422151098615 | 6.15769770666127 |
| tel_threat_Encroachment_obs \| observed only \| OBSERVER #1 | variance | 9.35733734372336 | 9.34012476792014 |
| tel_threat_BiomassAccumulation_obs \| observed only \| SITMAP #1 | variance | 4.28291337835876 | 4.26819680268124 |
| tel_threat_BiomassAccumulation_obs \| observed only \| YEAR #1 | variance | 6.56639824366416 | 6.5684859097469 |
| tel_threat_BiomassAccumulation_obs \| observed only \| OBSERVER #1 | variance | 10.1271969225091 | 10.1101057379174 |
| tel_threat_Eutrophization_obs \| observed only \| SITMAP #1 | variance | 4.24472134383608 | 4.23068961984314 |
| tel_threat_Eutrophization_obs \| observed only \| YEAR #1 | variance | 6.29935865915267 | 6.30202031954827 |
| tel_threat_Eutrophization_obs \| observed only \| OBSERVER #1 | variance | 8.84871968538665 | 8.83378201470091 |
| tel_threat_HarmfulMow \| with imputed \| SITMAP #1 | sd | 1.35965010125008 | 1.3573302206758 |
| tel_threat_HarmfulMow \| with imputed \| YEAR #1 | sd | 8.03777420814795e-05 | 7.9330195946273e-05 |

_Showing the first 60 of 103 rows._

### revised__19_threat_counts.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris teleius \| LandUseChange \| with imputed \| FALSE #1 | with_threat_negative | 36 | 35 |
| Phengaris teleius \| HarmfulMow \| with imputed \| TRUE #1 | with_threat_negative | 814 | 809 |
| Phengaris teleius \| GrazingByeffects \| with imputed \| TRUE #1 | with_threat_negative | 25 | 24 |
| Phengaris teleius \| Encroachment \| with imputed \| TRUE #1 | with_threat_negative | 593 | 592 |
| Phengaris teleius \| BiomassAccumulation \| with imputed \| TRUE #1 | with_threat_negative | 393 | 392 |
| Phengaris teleius \| Eutrophization \| with imputed \| TRUE #1 | with_threat_negative | 485 | 484 |
| Phengaris teleius \| HarmfulMow \| observed only \| TRUE #1 | with_threat_negative | 217 | 216 |

### revised__19_threat_estimates.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris teleius \| with imputed \| NativeDominants \| 0.60 (0.22-1.64) #1 | p_value | 0.318248679090937 | 0.318384001754805 |
| Phengaris teleius \| with imputed \| Encroachment \| 1.04 (0.60-1.79) #1 | p_value | 0.890361158105357 | 0.897258898679567 |
| Phengaris teleius \| with imputed \| Eutrophization \| 0.57 (0.30-1.09) #1 | p_value | 0.087059082288774 | 0.0869621770245281 |
| Phengaris teleius \| observed only \| HarmfulMow \| 0.80 (0.32-1.97) #1 | p_value | 0.62166096019634 | 0.623677507144572 |
| Phengaris teleius \| observed only \| HarmfulGrazing \| 1.44 (0.39-5.30) #1 | p_value | 0.579261503600122 | 0.579539131197103 |
| Phengaris teleius \| observed only \| AbioticNaturalProcesses \| 0.10 (0.01-0.72) #1 | p_value | 0.0215822238564647 | 0.0216003606786781 |
| Phengaris teleius \| observed only \| BiomassAccumulation \| 0.37 (0.13-1.06) #1 | p_value | 0.0639246348311568 | 0.0640334988298613 |
| Phengaris teleius \| observed only \| Eutrophization \| 0.38 (0.12-1.25) #1 | p_value | 0.112378624433614 | 0.112595452137019 |
| Phengaris teleius \| with imputed \| Eutrophization \| 0.57 (0.30-1.09) #1 | p_holm | 0.696472658310192 | 0.695697416196225 |
| Phengaris teleius \| observed only \| AbioticNaturalProcesses \| 0.10 (0.01-0.72) #1 | p_holm | 0.151075566995253 | 0.151202524750747 |
| Phengaris teleius \| observed only \| BiomassAccumulation \| 0.37 (0.13-1.06) #1 | p_holm | 0.383547808986941 | 0.384200992979168 |
| Phengaris teleius \| observed only \| Eutrophization \| 0.38 (0.12-1.25) #1 | p_holm | 0.56189312216807 | 0.562977260685094 |


# b07_common_data_aic

Snapshot `13_prikop` compared with `14_aic`.

80 files compared: 71 identical, 0 numerically identical (differences below 1e-9 relative), 9 changed, 0 added, 0 removed.

## Files that differ

| file | kind | status | rows_before | rows_after | rows_removed | rows_added | cols_removed | cols_added | cells_changed |
|---|---|---|---|---|---|---|---|---|---|
| 09_nausithous_aic_grassland.csv | tables | changed | 2 | 2 | 0 | 0 |  | engine, n_own_data, status | 5 |
| 09_nausithous_aic_habitat.csv | tables | changed | 5 | 5 | 0 | 0 |  | engine, n_own_data, status | 0 |
| 09_nausithous_aic_management.csv | tables | changed | 5 | 5 | 0 | 0 |  | engine, n_own_data, status | 20 |
| 09_nausithous_aic_natura2000.csv | tables | changed | 3 | 3 | 0 | 0 |  | engine, n_own_data, status | 0 |
| 10_teleius_aic_habitat.csv | tables | changed | 4 | 5 | 0 | 1 |  | engine, n_own_data, status | 12 |
| 10_teleius_aic_management.csv | tables | changed | 3 | 3 | 0 | 0 |  | engine, n_own_data, status | 11 |
| 10_teleius_aic_management_subset.csv | tables | changed | 4 | 4 | 0 | 0 |  | engine, n_own_data, status | 0 |
| 10_teleius_aic_protection.csv | tables | changed | 4 | 4 | 0 | 0 |  | engine, n_own_data, status | 0 |
| 11_both_species_aic.csv | tables | changed | 5 | 5 | 0 | 0 |  | engine, n_own_data, status | 26 |

## Changed numbers

The full list of 74 changed cells is in `b07_common_data_aic.csv`.

### 09_nausithous_aic_grassland.csv

| row | column | before | after |
|---|---|---|---|
| nau_ttp \| Regularly managed grassland #1 | n_obs | 3132 | 1710 |
| nau_ttp \| Regularly managed grassland #1 | AIC | 4206.79710767477 | 2311.04022500828 |
| nau_ttp \| Regularly managed grassland #1 | BIC | 4230.99481590322 | 2332.81721960627 |
| nau_ttp \| Regularly managed grassland #1 | logLik | -2099.39855383739 | -1151.52011250414 |
| nau_ttp \| Regularly managed grassland #1 | delta_AIC | 1964.48214287596 | 68.7252602094668 |

### 09_nausithous_aic_management.csv

| row | column | before | after |
|---|---|---|---|
| nau_method_timing \| Mowing method by timing (selected management model) #1 | n_obs | 1711 | 1710 |
| nau_timing \| Mowing timing #1 | n_obs | 1711 | 1710 |
| nau_method \| Mowing method #1 | n_obs | 1711 | 1710 |
| nau_mow_null \| Null model on records where mowing was assessed #1 | n_obs | 1711 | 1710 |
| nau_method_timing \| Mowing method by timing (selected management model) #1 | AIC | 2245.16166382103 | 2242.31708443961 |
| nau_timing \| Mowing timing #1 | AIC | 2245.73058591778 | 2243.0884105809 |
| nau_method \| Mowing method #1 | AIC | 2305.97092654138 | 2304.0342804901 |
| nau_mow_null \| Null model on records where mowing was assessed #1 | AIC | 2313.14065004997 | 2311.34940653021 |
| nau_method_timing \| Mowing method by timing (selected management model) #1 | BIC | 2277.83066346438 | 2274.98257633659 |
| nau_timing \| Mowing timing #1 | BIC | 2267.50991901335 | 2264.86540517889 |
| nau_method \| Mowing method #1 | BIC | 2327.75025963694 | 2325.81127508808 |
| nau_mow_null \| Null model on records where mowing was assessed #1 | BIC | 2329.47514987164 | 2327.6821524787 |
| nau_method_timing \| Mowing method by timing (selected management model) #1 | logLik | -1116.58083191051 | -1115.1585422198 |
| nau_timing \| Mowing timing #1 | logLik | -1118.86529295889 | -1117.54420529045 |
| nau_method \| Mowing method #1 | logLik | -1148.98546327069 | -1148.01714024505 |
| nau_mow_null \| Null model on records where mowing was assessed #1 | logLik | -1153.57032502498 | -1152.67470326511 |
| nau_timing \| Mowing timing #1 | delta_AIC | 0.568922096753795 | 0.771326141294139 |
| nau_management_het \| Mowing timing by method by within-site heterogeneity #1 | delta_AIC | 3.17382244355758 | 6.01840182497745 |
| nau_method \| Mowing method #1 | delta_AIC | 60.80926272035 | 61.7171960504893 |
| nau_mow_null \| Null model on records where mowing was assessed #1 | delta_AIC | 67.9789862289417 | 69.0323220906048 |

### 10_teleius_aic_habitat.csv

| row | column | before | after |
|---|---|---|---|
| tel_plant \| Host plant abundance, all records #1 | n_obs | 1310 | 699 |
| tel_plant_poly \| Host plant abundance, quadratic #1 | n_obs | 1310 | 699 |
| tel_plant \| Host plant abundance, all records #1 | AIC | 648.228276439653 | 565.465463945242 |
| tel_plant_poly \| Host plant abundance, quadratic #1 | AIC | 650.173947840762 | 563.735076352017 |
| tel_plant \| Host plant abundance, all records #1 | BIC | 668.939406104434 | 583.664066914177 |
| tel_plant_poly \| Host plant abundance, quadratic #1 | BIC | 676.062859921738 | 586.483330063186 |
| tel_plant \| Host plant abundance, all records #1 | logLik | -320.114138219827 | -278.732731972621 |
| tel_plant_poly \| Host plant abundance, quadratic #1 | logLik | -320.086973920381 | -276.867538176008 |
| tel_area_null \| Null model on records with a positive site area #1 | delta_AIC | 0 | 16.0794852024928 |
| tel_area \| Site area (selected habitat model) #1 | delta_AIC | 1.81376997458574 | 17.8932551770786 |
| tel_plant \| Host plant abundance, all records #1 | delta_AIC | 74.4061928391832 | 7.72286554726463 |
| tel_plant_poly \| Host plant abundance, quadratic #1 | delta_AIC | 76.3518642402923 | 5.99247795403983 |

### 10_teleius_aic_management.csv

| row | column | before | after |
|---|---|---|---|
| tel_method \| Mowing method, all records #1 | n_obs | 697 | 689 |
| tel_timing \| Mowing timing, all records #1 | n_obs | 744 | 689 |
| tel_method \| Mowing method, all records #1 | AIC | 621.414905392081 | 611.810710210255 |
| tel_timing \| Mowing timing, all records #1 | AIC | 684.423498981235 | 605.051658041988 |
| tel_method \| Mowing method, all records #1 | BIC | 639.602047035123 | 629.951675294309 |
| tel_timing \| Mowing timing, all records #1 | BIC | 702.871663120567 | 623.192623126043 |
| tel_method \| Mowing method, all records #1 | logLik | -306.70745269604 | -301.905355105127 |
| tel_timing \| Mowing timing, all records #1 | logLik | -338.211749490617 | -298.525829020994 |
| tel_method_timing \| Mowing method by timing, all records #1 | delta_AIC | 0 | 3.08790279076038 |
| tel_method \| Mowing method, all records #1 | delta_AIC | 13.2753445593327 | 6.75905216826663 |
| tel_timing \| Mowing timing, all records #1 | delta_AIC | 76.2839381484863 | 0 |

### 11_both_species_aic.csv

| row | column | before | after |
|---|---|---|---|
| both_mapped_habitat \| Species and mapped habitat code #1 | n_obs | 682 | 123 |
| both_management_type \| Species, mowing and grazing #1 | n_obs | 2393 | 123 |
| both_cooccurrence \| Co-occurrence of the two species by species #1 | n_obs | 4463 | 123 |
| both_recorded_habitat \| Species and recorded habitat types #1 | n_obs | 4437 | 123 |
| both_species \| Species only #1 | n_obs | 4463 | 123 |
| both_mapped_habitat \| Species and mapped habitat code #1 | df | 27 | 20 |
| both_mapped_habitat \| Species and mapped habitat code #1 | AIC | 904.865524889532 | 168.460283584374 |
| both_management_type \| Species, mowing and grazing #1 | AIC | 2789.44554774188 | 151.645571002492 |
| both_cooccurrence \| Co-occurrence of the two species by species #1 | AIC | 5200.17780233785 | 148.176311290392 |
| both_recorded_habitat \| Species and recorded habitat types #1 | AIC | 5299.6167065562 | 154.699355399447 |
| both_species \| Species only #1 | AIC | 5348.50015120603 | 149.939495758605 |
| both_mapped_habitat \| Species and mapped habitat code #1 | BIC | 1027.04132565131 | 224.703970691822 |
| both_management_type \| Species, mowing and grazing #1 | BIC | 2824.12736626933 | 168.518677134726 |
| both_cooccurrence \| Co-occurrence of the two species by species #1 | BIC | 5232.19568466099 | 162.237233067254 |
| both_recorded_habitat \| Species and recorded habitat types #1 | BIC | 5357.19631031861 | 180.009014597799 |
| both_species \| Species only #1 | BIC | 5374.11445706455 | 161.188233180094 |
| both_mapped_habitat \| Species and mapped habitat code #1 | logLik | -425.432762444766 | -64.230141792187 |
| both_management_type \| Species, mowing and grazing #1 | logLik | -1388.72277387094 | -69.8227855012459 |
| both_cooccurrence \| Co-occurrence of the two species by species #1 | logLik | -2595.08890116892 | -69.0881556451962 |
| both_recorded_habitat \| Species and recorded habitat types #1 | logLik | -2640.8083532781 | -68.3496776997236 |
| both_species \| Species only #1 | logLik | -2670.25007560302 | -70.9697478793023 |
| both_mapped_habitat \| Species and mapped habitat code #1 | delta_AIC | 0 | 20.2839722939817 |
| both_management_type \| Species, mowing and grazing #1 | delta_AIC | 1884.58002285235 | 3.46925971209959 |
| both_cooccurrence \| Co-occurrence of the two species by species #1 | delta_AIC | 4295.31227744832 | 0 |
| both_recorded_habitat \| Species and recorded habitat types #1 | delta_AIC | 4394.75118166666 | 6.52304410905487 |
| both_species \| Species only #1 | delta_AIC | 4443.6346263165 | 1.76318446821222 |


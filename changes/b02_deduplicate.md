# b02_deduplicate

Snapshot `08_functions` compared with `09_dedup`.

80 files compared: 16 identical, 0 numerically identical (differences below 1e-9 relative), 64 changed, 0 added, 0 removed.

## Files that differ

| file | kind | status | rows_before | rows_after | rows_removed | rows_added | cols_removed | cols_added | cells_changed |
|---|---|---|---|---|---|---|---|---|---|
| data_analysis.csv | processed | changed | 4540 | 4463 | 77 | 0 | range_Pnau, range_Ptel, range_both | ID_RECORD | 32 |
| data_clean.csv | processed | changed | 4540 | 4463 | 77 | 0 | range_Pnau, range_Ptel, range_both | ID_RECORD | 32 |
| data_with_imputed.csv | processed | changed | 5409 | 5300 | 109 | 0 | range_Pnau, range_Ptel, range_both | ID_RECORD | 60 |
| 02_imputation_by_species.csv | tables | changed | 6 | 6 | 0 | 0 |  |  | 5 |
| 02_imputation_totals.csv | tables | changed | 2 | 2 | 0 | 0 |  |  | 2 |
| 04_cleaning_summary.csv | tables | changed | 3 | 3 | 0 | 0 |  |  | 3 |
| 04_flag_counts.csv | tables | changed | 5 | 5 | 0 | 0 |  |  | 13 |
| 06_habitat_coverage.csv | tables | changed | 4 | 4 | 0 | 0 |  |  | 9 |
| 06_mapped_habitats_nausithous.csv | tables | changed | 18 | 18 | 0 | 0 |  |  | 7 |
| 06_mapped_habitats_teleius.csv | tables | changed | 8 | 8 | 0 | 0 |  |  | 4 |
| 06_natural_habitat_share.csv | tables | changed | 2 | 2 | 0 | 0 |  |  | 2 |
| 06_natural_vs_biotop.csv | tables | changed | 4 | 4 | 0 | 0 |  |  | 2 |
| 07_abundance_by_species.csv | tables | changed | 2 | 2 | 0 | 0 |  |  | 6 |
| 07_data_extent.csv | tables | changed | 5 | 5 | 0 | 0 |  |  | 1 |
| 07_habitat_type_counts.csv | tables | changed | 4 | 4 | 0 | 0 |  |  | 9 |
| 07_host_plant_abundance.csv | tables | changed | 12 | 12 | 0 | 0 |  |  | 10 |
| 07_management_combined.csv | tables | changed | 20 | 20 | 0 | 0 |  |  | 14 |
| 07_management_method.csv | tables | changed | 12 | 12 | 0 | 0 |  |  | 10 |
| 07_management_timing.csv | tables | changed | 12 | 12 | 0 | 0 |  |  | 11 |
| 07_management_types_nausithous.csv | tables | changed | 3 | 3 | 0 | 0 |  |  | 5 |
| 07_management_types_teleius.csv | tables | changed | 3 | 3 | 0 | 0 |  |  | 4 |
| 07_mapping_fields.csv | tables | changed | 737 | 731 | 6 | 0 |  |  | 437 |
| 07_occupancy_by_species.csv | tables | changed | 4 | 4 | 0 | 0 |  |  | 4 |
| 07_positivity_by_year.csv | tables | changed | 12 | 12 | 0 | 0 |  |  | 20 |
| 07_protection_evl.csv | tables | changed | 8 | 8 | 0 | 0 |  |  | 7 |
| 07_protection_evl_combined.csv | tables | changed | 12 | 12 | 0 | 0 |  |  | 10 |
| 07_protection_evl_target.csv | tables | changed | 8 | 8 | 0 | 0 |  |  | 7 |
| 07_protection_full.csv | tables | changed | 24 | 24 | 0 | 0 |  |  | 15 |
| 07_protection_mzchu.csv | tables | changed | 8 | 8 | 0 | 0 |  |  | 8 |
| 07_recorded_habitats_nausithous.csv | tables | changed | 4 | 4 | 0 | 0 |  |  | 7 |
| 07_recorded_habitats_teleius.csv | tables | changed | 4 | 4 | 0 | 0 |  |  | 5 |
| 07_records_by_year.csv | tables | changed | 12 | 12 | 0 | 0 |  |  | 10 |
| 07_records_per_observer.csv | tables | changed | 80 | 80 | 0 | 0 |  |  | 20 |
| 07_species_cooccurrence.csv | tables | changed | 5 | 5 | 0 | 0 |  |  | 5 |
| 09_nausithous_aic_grassland.csv | tables | changed | 2 | 2 | 0 | 0 |  |  | 9 |
| 09_nausithous_aic_habitat.csv | tables | changed | 5 | 5 | 0 | 0 |  |  | 24 |
| 09_nausithous_aic_management.csv | tables | changed | 5 | 5 | 0 | 0 |  |  | 24 |
| 09_nausithous_aic_natura2000.csv | tables | changed | 3 | 3 | 0 | 0 |  |  | 14 |
| 09_nausithous_coefficients.csv | tables | changed | 101 | 101 | 0 | 0 |  |  | 382 |
| 09_nausithous_fit_statistics.csv | tables | changed | 35 | 35 | 0 | 0 |  |  | 175 |
| 09_nausithous_fitting_issues.csv | tables | changed | 2 | 2 | 2 | 2 |  |  | 0 |
| 09_nausithous_random_effects.csv | tables | changed | 56 | 56 | 0 | 0 |  |  | 112 |
| 10_teleius_aic_habitat.csv | tables | changed | 4 | 4 | 0 | 0 |  |  | 19 |
| 10_teleius_aic_management.csv | tables | changed | 3 | 3 | 0 | 0 |  |  | 14 |
| 10_teleius_aic_management_subset.csv | tables | changed | 4 | 4 | 0 | 0 |  |  | 19 |
| 10_teleius_aic_protection.csv | tables | changed | 4 | 4 | 0 | 0 |  |  | 19 |
| 10_teleius_coefficients.csv | tables | changed | 309 | 309 | 0 | 0 |  |  | 1185 |
| 10_teleius_fit_statistics.csv | tables | changed | 38 | 38 | 2 | 2 |  |  | 165 |
| 10_teleius_fitting_issues.csv | tables | changed | 2 | 2 | 1 | 1 |  |  | 0 |
| 10_teleius_random_effects.csv | tables | changed | 57 | 57 | 0 | 0 |  |  | 103 |
| 11_both_species_aic.csv | tables | changed | 5 | 5 | 0 | 0 |  |  | 24 |
| 11_both_species_coefficients.csv | tables | changed | 41 | 41 | 0 | 0 |  |  | 159 |
| 11_both_species_fit_statistics.csv | tables | changed | 5 | 5 | 1 | 1 |  |  | 20 |
| 11_both_species_fitting_issues.csv | tables | changed | 1 | 2 | 1 | 2 |  |  | 0 |
| 11_both_species_random_effects.csv | tables | changed | 10 | 10 | 0 | 0 |  |  | 20 |
| 11_species_comparisons.csv | tables | changed | 3 | 3 | 0 | 0 |  |  | 5 |
| 13_pca_loadings.csv | tables | changed | 17 | 17 | 0 | 0 |  |  | 34 |
| 13_pca_variance.csv | tables | changed | 4 | 4 | 0 | 0 |  |  | 8 |
| 13_rda_anova_by_term.csv | tables | changed | 3 | 3 | 0 | 0 |  |  | 5 |
| 13_rda_anova_overall.csv | tables | changed | 2 | 2 | 0 | 0 |  |  | 3 |
| 13_rda_tap_anova_by_axis.csv | tables | changed | 3 | 3 | 0 | 0 |  |  | 6 |
| 13_rda_tap_anova_by_term.csv | tables | changed | 3 | 3 | 0 | 0 |  |  | 5 |
| 13_rda_tap_anova_overall.csv | tables | changed | 2 | 2 | 0 | 0 |  |  | 3 |
| 13_rda_variable_scores.csv | tables | changed | 17 | 17 | 0 | 0 |  |  | 34 |

## Changed numbers

The full list of 3386 changed cells is in `b02_deduplicate.csv`.

### data_analysis.csv

| row | column | before | after |
|---|---|---|---|
| 48053829 \| Phengaris nausithous \| FALSE #1 | row_n | 21 | 36 |
| 52918761 \| Phengaris nausithous \| FALSE #1 | row_n | 89 | 90 |
| 65296258 \| Phengaris nausithous \| FALSE #1 | row_n | 174 | 214 |
| 52919402 \| Phengaris nausithous \| FALSE #1 | row_n | 174 | 175 |
| 52919385 \| Phengaris nausithous \| FALSE #1 | row_n | 216 | 256 |
| 65296363 \| Phengaris nausithous \| FALSE #1 | row_n | 220 | 261 |
| 52996379 \| Phengaris nausithous \| FALSE #1 | row_n | 221 | 222 |
| 52919383 \| Phengaris nausithous \| FALSE #1 | row_n | 255 | 256 |
| 52919179 \| Phengaris nausithous \| FALSE #1 | row_n | 264 | 265 |
| 65296788 \| Phengaris nausithous \| FALSE #1 | row_n | 266 | 307 |
| 65296061 \| Phengaris nausithous \| FALSE #1 | row_n | 271 | 272 |
| 65296146 \| Phengaris nausithous \| FALSE #1 | row_n | 316 | 358 |
| 54473781 \| Phengaris nausithous \| FALSE #1 | row_n | 336 | 337 |
| 57615885 \| Phengaris nausithous \| FALSE #1 | row_n | 371 | 372 |
| 52919341 \| Phengaris nausithous \| FALSE #1 | row_n | 380 | 381 |
| 65296748 \| Phengaris nausithous \| FALSE #1 | row_n | 382 | 383 |
| 54705980 \| Phengaris nausithous \| FALSE #1 | row_n | 390 | 391 |
| 54473862 \| Phengaris nausithous \| FALSE #1 | row_n | 391 | 431 |
| 65296075 \| Phengaris nausithous \| FALSE #1 | row_n | 400 | 401 |
| 52919300 \| Phengaris nausithous \| FALSE #1 | row_n | 400 | 401 |
| 54720497 \| Phengaris nausithous \| FALSE #1 | row_n | 427 | 468 |
| 52919264 \| Phengaris nausithous \| FALSE #1 | row_n | 452 | 493 |
| 52918889 \| Phengaris nausithous \| FALSE #1 | row_n | 475 | 516 |
| 54720452 \| Phengaris nausithous \| FALSE #1 | row_n | 530 | 531 |
| 54720571 \| Phengaris nausithous \| FALSE #1 | row_n | 532 | 533 |
| 65296213 \| Phengaris nausithous \| FALSE #1 | row_n | 553 | 554 |
| 54720603 \| Phengaris nausithous \| FALSE #1 | row_n | 570 | 612 |
| 57615624 \| Phengaris nausithous \| FALSE #1 | row_n | 578 | 579 |
| 65296472 \| Phengaris nausithous \| FALSE #1 | row_n | 626 | 664 |
| 54473878 \| Phengaris nausithous \| FALSE #1 | row_n | 763 | 764 |
| 54473865 \| Phengaris teleius \| FALSE #1 | row_n | 440 | 441 |
| 54705980 \| Phengaris teleius \| TRUE #1 | row_n | 210 | 211 |

### data_clean.csv

| row | column | before | after |
|---|---|---|---|
| 48053829 \| Phengaris nausithous \| FALSE #1 | row_n | 21 | 36 |
| 52918761 \| Phengaris nausithous \| FALSE #1 | row_n | 89 | 90 |
| 65296258 \| Phengaris nausithous \| FALSE #1 | row_n | 174 | 214 |
| 52919402 \| Phengaris nausithous \| FALSE #1 | row_n | 174 | 175 |
| 52919385 \| Phengaris nausithous \| FALSE #1 | row_n | 216 | 256 |
| 65296363 \| Phengaris nausithous \| FALSE #1 | row_n | 220 | 261 |
| 52996379 \| Phengaris nausithous \| FALSE #1 | row_n | 221 | 222 |
| 52919383 \| Phengaris nausithous \| FALSE #1 | row_n | 255 | 256 |
| 52919179 \| Phengaris nausithous \| FALSE #1 | row_n | 264 | 265 |
| 65296788 \| Phengaris nausithous \| FALSE #1 | row_n | 266 | 307 |
| 65296061 \| Phengaris nausithous \| FALSE #1 | row_n | 271 | 272 |
| 65296146 \| Phengaris nausithous \| FALSE #1 | row_n | 316 | 358 |
| 54473781 \| Phengaris nausithous \| FALSE #1 | row_n | 336 | 337 |
| 57615885 \| Phengaris nausithous \| FALSE #1 | row_n | 371 | 372 |
| 52919341 \| Phengaris nausithous \| FALSE #1 | row_n | 380 | 381 |
| 65296748 \| Phengaris nausithous \| FALSE #1 | row_n | 382 | 383 |
| 54705980 \| Phengaris nausithous \| FALSE #1 | row_n | 390 | 391 |
| 54473862 \| Phengaris nausithous \| FALSE #1 | row_n | 391 | 431 |
| 65296075 \| Phengaris nausithous \| FALSE #1 | row_n | 400 | 401 |
| 52919300 \| Phengaris nausithous \| FALSE #1 | row_n | 400 | 401 |
| 54720497 \| Phengaris nausithous \| FALSE #1 | row_n | 427 | 468 |
| 52919264 \| Phengaris nausithous \| FALSE #1 | row_n | 452 | 493 |
| 52918889 \| Phengaris nausithous \| FALSE #1 | row_n | 475 | 516 |
| 54720452 \| Phengaris nausithous \| FALSE #1 | row_n | 530 | 531 |
| 54720571 \| Phengaris nausithous \| FALSE #1 | row_n | 532 | 533 |
| 65296213 \| Phengaris nausithous \| FALSE #1 | row_n | 553 | 554 |
| 54720603 \| Phengaris nausithous \| FALSE #1 | row_n | 570 | 612 |
| 57615624 \| Phengaris nausithous \| FALSE #1 | row_n | 578 | 579 |
| 65296472 \| Phengaris nausithous \| FALSE #1 | row_n | 626 | 664 |
| 54473878 \| Phengaris nausithous \| FALSE #1 | row_n | 763 | 764 |
| 54473865 \| Phengaris teleius \| FALSE #1 | row_n | 440 | 441 |
| 54705980 \| Phengaris teleius \| TRUE #1 | row_n | 210 | 211 |

### data_with_imputed.csv

| row | column | before | after |
|---|---|---|---|
| 65296220 \| Phengaris nausithous \| FALSE #1 | ALP | 0 | 100 |
| 65296784 \| Phengaris teleius \| FALSE #1 | ALP | 0 | 100 |
| 65296486 \| Phengaris nausithous \| FALSE #1 | PAN | 100 | 0 |
| 65296964 \| Phengaris teleius \| FALSE #1 | PAN | 100 | 0 |
| 48053829 \| Phengaris nausithous \| FALSE #1 | row_n | 21 | 36 |
| 53739077 \| Phengaris nausithous \| FALSE #1 | row_n | 63 | 88 |
| 53739036 \| Phengaris nausithous \| FALSE #1 | row_n | 88 | 89 |
| 52918761 \| Phengaris nausithous \| FALSE #1 | row_n | 89 | 90 |
| 65297001 \| Phengaris nausithous \| FALSE #1 | row_n | 108 | 109 |
| 65296232 \| Phengaris nausithous \| FALSE #1 | row_n | 108 | 109 |
| 65296258 \| Phengaris nausithous \| FALSE #1 | row_n | 174 | 214 |
| 52919402 \| Phengaris nausithous \| FALSE #1 | row_n | 174 | 175 |
| 53738254 \| Phengaris nausithous \| FALSE #1 | row_n | 199 | 239 |
| 54720593 \| Phengaris nausithous \| FALSE #1 | row_n | 216 | 256 |
| 52919386 \| Phengaris nausithous \| FALSE #1 | row_n | 216 | 256 |
| 52919385 \| Phengaris nausithous \| FALSE #1 | row_n | 216 | 256 |
| 65296363 \| Phengaris nausithous \| FALSE #1 | row_n | 220 | 261 |
| 52996379 \| Phengaris nausithous \| FALSE #1 | row_n | 221 | 222 |
| 65296809 \| Phengaris nausithous \| FALSE #1 | row_n | 229 | 270 |
| 54720588 \| Phengaris nausithous \| FALSE #1 | row_n | 252 | 293 |
| 65296371 \| Phengaris nausithous \| FALSE #1 | row_n | 253 | 293 |
| 52919383 \| Phengaris nausithous \| FALSE #1 | row_n | 255 | 256 |
| 52919179 \| Phengaris nausithous \| FALSE #1 | row_n | 264 | 265 |
| 65296788 \| Phengaris nausithous \| FALSE #1 | row_n | 266 | 307 |
| 65296623 \| Phengaris nausithous \| FALSE #1 | row_n | 271 | 272 |
| 65296061 \| Phengaris nausithous \| FALSE #1 | row_n | 271 | 272 |
| 65296146 \| Phengaris nausithous \| FALSE #1 | row_n | 316 | 358 |
| 54473781 \| Phengaris nausithous \| FALSE #1 | row_n | 336 | 337 |
| 57615885 \| Phengaris nausithous \| FALSE #1 | row_n | 371 | 372 |
| 52919341 \| Phengaris nausithous \| FALSE #1 | row_n | 380 | 381 |
| 65296748 \| Phengaris nausithous \| FALSE #1 | row_n | 382 | 383 |
| 54720502 \| Phengaris nausithous \| FALSE #1 | row_n | 385 | 426 |
| 54705980 \| Phengaris nausithous \| FALSE #1 | row_n | 390 | 391 |
| 54473862 \| Phengaris nausithous \| FALSE #1 | row_n | 391 | 431 |
| 65296075 \| Phengaris nausithous \| FALSE #1 | row_n | 400 | 401 |
| 52919300 \| Phengaris nausithous \| FALSE #1 | row_n | 400 | 401 |
| 54720497 \| Phengaris nausithous \| FALSE #1 | row_n | 427 | 468 |
| 52919264 \| Phengaris nausithous \| FALSE #1 | row_n | 452 | 493 |
| 52918889 \| Phengaris nausithous \| FALSE #1 | row_n | 475 | 516 |
| 54720452 \| Phengaris nausithous \| FALSE #1 | row_n | 530 | 531 |
| 54720571 \| Phengaris nausithous \| FALSE #1 | row_n | 532 | 533 |
| 65296191 \| Phengaris nausithous \| FALSE #1 | row_n | 533 | 575 |
| 65296213 \| Phengaris nausithous \| FALSE #1 | row_n | 553 | 554 |
| 54720603 \| Phengaris nausithous \| FALSE #1 | row_n | 570 | 612 |
| 57615624 \| Phengaris nausithous \| FALSE #1 | row_n | 578 | 579 |
| 65296472 \| Phengaris nausithous \| FALSE #1 | row_n | 626 | 664 |
| 65296486 \| Phengaris nausithous \| FALSE #1 | row_n | 692 | 693 |
| 65296611 \| Phengaris nausithous \| FALSE #1 | row_n | 693 | 713 |
| 65296081 \| Phengaris nausithous \| FALSE #1 | row_n | 693 | 713 |
| 65296220 \| Phengaris nausithous \| FALSE #1 | row_n | 694 | 714 |
| 54473878 \| Phengaris nausithous \| FALSE #1 | row_n | 763 | 764 |
| 53739049 \| Phengaris teleius \| FALSE #1 | row_n | 42 | 59 |
| 53739071 \| Phengaris teleius \| FALSE #1 | row_n | 59 | 60 |
| 53738224 \| Phengaris teleius \| FALSE #1 | row_n | 119 | 137 |
| 65296964 \| Phengaris teleius \| FALSE #1 | row_n | 372 | 373 |
| 65297014 \| Phengaris teleius \| FALSE #1 | row_n | 373 | 389 |
| 65296789 \| Phengaris teleius \| FALSE #1 | row_n | 373 | 389 |
| 65296784 \| Phengaris teleius \| FALSE #1 | row_n | 374 | 390 |
| 54473865 \| Phengaris teleius \| FALSE #1 | row_n | 440 | 441 |
| 54705980 \| Phengaris teleius \| TRUE #1 | row_n | 210 | 211 |

### 02_imputation_by_species.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous \| FALSE #1 | number | 1620 | 1598 |
| Phengaris nausithous \| FALSE #2 | number | 2222 | 2157 |
| Phengaris teleius \| FALSE #1 | number | 215 | 213 |
| Phengaris teleius \| FALSE #2 | number | 507 | 492 |
| Phengaris teleius \| TRUE #1 | number | 843 | 838 |

### 02_imputation_totals.csv

| row | column | before | after |
|---|---|---|---|
| FALSE #1 | records | 4564 | 4460 |
| TRUE #1 | records | 845 | 840 |

### 04_cleaning_summary.csv

| row | column | before | after |
|---|---|---|---|
| records with imputed absences (step 02) #1 | records | 5409 | 5300 |
| targeted monitoring only #1 | records | 5409 | 5300 |
| host plant present, excluded years dropped #1 | records | 4540 | 4463 |

### 04_flag_counts.csv

| row | column | before | after |
|---|---|---|---|
| EVL #1 | records_flagged | 846 | 824 |
| EVL_target #1 | records_flagged | 372 | 364 |
| MZCHU #1 | records_flagged | 307 | 293 |
| PROTECT #1 | records_flagged | 928 | 905 |
| SPEC_NUM #1 | records_flagged | 105 | 104 |
| EVL #1 | records_total | 4540 | 4463 |
| EVL_target #1 | records_total | 4540 | 4463 |
| MZCHU #1 | records_total | 4540 | 4463 |
| PROTECT #1 | records_total | 4540 | 4463 |
| SPEC_NUM #1 | records_total | 4540 | 4463 |
| EVL #1 | percent | 18.6 | 18.5 |
| MZCHU #1 | percent | 6.8 | 6.6 |
| PROTECT #1 | percent | 20.4 | 20.3 |

### 06_habitat_coverage.csv

| row | column | before | after |
|---|---|---|---|
| AREA_SITE #1 | records_with_value | 4540 | 4463 |
| BIOTOP #1 | records_with_value | 684 | 682 |
| FSB #1 | records_with_value | 4540 | 4463 |
| HET_OUT #1 | records_with_value | 4540 | 4463 |
| AREA_SITE #1 | records_total | 4540 | 4463 |
| BIOTOP #1 | records_total | 4540 | 4463 |
| FSB #1 | records_total | 4540 | 4463 |
| HET_OUT #1 | records_total | 4540 | 4463 |
| BIOTOP #1 | percent | 15.1 | 15.3 |

### 06_mapped_habitats_nausithous.csv

| row | column | before | after |
|---|---|---|---|
| -1 #1 | AREA | 157.464791524732 | 150.363487475663 |
| T1.6 #1 | AREA | 0.282267285937784 | 0.141133642968892 |
| -1 #1 | mean_area | 1280.20155711164 | 1240.62283395761 |
| T1.6 #1 | mean_area | 100.80974497778 | 54.2821703726507 |
| -1 #1 | median_area | 25.1097440633093 | 22.5279965537484 |
| -1 #1 | COUNT | 1230 | 1212 |
| T1.6 #1 | COUNT | 28 | 26 |

### 06_mapped_habitats_teleius.csv

| row | column | before | after |
|---|---|---|---|
| -1 #1 | AREA | 23.3046905512473 | 22.9893938258797 |
| -1 #1 | mean_area | 1447.49630753089 | 1445.87382552702 |
| -1 #1 | median_area | 17.687709230464 | 14.3263157546753 |
| -1 #1 | COUNT | 161 | 159 |

### 06_natural_habitat_share.csv

| row | column | before | after |
|---|---|---|---|
| row #1 | records | 3856 | 3781 |
| row #2 | records | 684 | 682 |

### 06_natural_vs_biotop.csv

| row | column | before | after |
|---|---|---|---|
| FALSE #1 | Freq | 3856 | 3781 |
| TRUE #2 | Freq | 684 | 682 |

### 07_abundance_by_species.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous #1 | records | 1467 | 1447 |
| Phengaris teleius #1 | records | 564 | 561 |
| Phengaris nausithous #1 | mean | 7.01499659168371 | 7.0573600552868 |
| Phengaris teleius #1 | mean | 10.5691489361702 | 10.6114081996435 |
| Phengaris nausithous #1 | sd | 12.1526688515264 | 12.2228115622574 |
| Phengaris teleius #1 | sd | 29.6833586689184 | 29.7566105196908 |

### 07_data_extent.csv

| row | column | before | after |
|---|---|---|---|
| records #1 | value | 4540 | 4463 |

### 07_habitat_type_counts.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous #1 | TTP | 1108 | 1074 |
| Phengaris nausithous #2 | TTP | 946 | 932 |
| Phengaris teleius #1 | TTP | 675 | 665 |
| Phengaris teleius #2 | TTP | 156 | 155 |
| Phengaris nausithous #1 | ZARUST | 457 | 451 |
| Phengaris nausithous #2 | ZARUST | 370 | 369 |
| Phengaris nausithous #1 | JINY | 217 | 211 |
| Phengaris nausithous #2 | JINY | 164 | 160 |
| Phengaris teleius #1 | JINY | 152 | 149 |

### 07_host_plant_abundance.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous #1 | COUNT | 1245 | 1221 |
| Phengaris nausithous #2 | COUNT | 427 | 412 |
| Phengaris nausithous #3 | COUNT | 75 | 71 |
| Phengaris teleius #1 | COUNT | 699 | 690 |
| Phengaris teleius #2 | COUNT | 355 | 353 |
| Phengaris teleius #3 | COUNT | 68 | 67 |
| Phengaris nausithous #4 | COUNT | 734 | 726 |
| Phengaris nausithous #5 | COUNT | 619 | 610 |
| Phengaris nausithous #6 | COUNT | 116 | 113 |
| Phengaris teleius #4 | COUNT | 96 | 94 |

### 07_management_combined.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous \| appropriate mow and appropriate timing #1 | COUNT | 330 | 316 |
| Phengaris nausithous \| appropriate mow only #1 | COUNT | 97 | 95 |
| Phengaris nausithous \| appropriate timing only #1 | COUNT | 229 | 220 |
| Phengaris nausithous \| inappropriate mow and inappropriate timing #1 | COUNT | 287 | 284 |
| Phengaris nausithous \| <NA> #1 | COUNT | 804 | 789 |
| Phengaris teleius \| appropriate mow and appropriate timing #1 | COUNT | 247 | 243 |
| Phengaris teleius \| appropriate timing only #1 | COUNT | 161 | 157 |
| Phengaris teleius \| inappropriate mow and inappropriate timing #1 | COUNT | 134 | 133 |
| Phengaris teleius \| <NA> #1 | COUNT | 536 | 533 |
| Phengaris nausithous \| appropriate mow and appropriate timing #2 | COUNT | 365 | 357 |
| Phengaris nausithous \| appropriate mow only #2 | COUNT | 61 | 60 |
| Phengaris nausithous \| appropriate timing only #2 | COUNT | 268 | 263 |
| Phengaris nausithous \| <NA> #2 | COUNT | 659 | 653 |
| Phengaris teleius \| <NA> #2 | COUNT | 90 | 88 |

### 07_management_method.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous #1 | COUNT | 519 | 507 |
| Phengaris nausithous #2 | COUNT | 431 | 415 |
| Phengaris nausithous #3 | COUNT | 797 | 782 |
| Phengaris teleius #1 | COUNT | 299 | 294 |
| Phengaris teleius #2 | COUNT | 293 | 289 |
| Phengaris teleius #3 | COUNT | 530 | 527 |
| Phengaris nausithous #4 | COUNT | 390 | 385 |
| Phengaris nausithous #5 | COUNT | 430 | 421 |
| Phengaris nausithous #6 | COUNT | 649 | 643 |
| Phengaris teleius #6 | COUNT | 88 | 86 |

### 07_management_timing.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous #1 | COUNT | 389 | 384 |
| Phengaris nausithous #2 | COUNT | 583 | 559 |
| Phengaris nausithous #3 | COUNT | 775 | 761 |
| Phengaris teleius #1 | COUNT | 181 | 180 |
| Phengaris teleius #2 | COUNT | 440 | 432 |
| Phengaris teleius #3 | COUNT | 501 | 498 |
| Phengaris nausithous #4 | COUNT | 183 | 182 |
| Phengaris nausithous #5 | COUNT | 671 | 658 |
| Phengaris nausithous #6 | COUNT | 615 | 609 |
| Phengaris teleius #5 | COUNT | 114 | 113 |
| Phengaris teleius #6 | COUNT | 69 | 68 |

### 07_management_types_nausithous.csv

| row | column | before | after |
|---|---|---|---|
| mowing #1 | COUNT | 864 | 850 |
| neglected grassland #1 | COUNT | 370 | 369 |
| grazing #1 | PERC | 2.38257317903336 | 2.41545893719807 |
| mowing #1 | PERC | 58.8155207624234 | 58.6611456176674 |
| neglected grassland #1 | PERC | 25.1872021783526 | 25.4658385093168 |

### 07_management_types_teleius.csv

| row | column | before | after |
|---|---|---|---|
| mowing #1 | COUNT | 135 | 134 |
| grazing #1 | PERC | 4.45544554455445 | 4.5 |
| mowing #1 | PERC | 66.8316831683168 | 67 |
| neglected grassland #1 | PERC | 21.2871287128713 | 21.5 |

### 07_mapping_fields.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous #290 | POSITIVE | 1 | 0 |
| Phengaris nausithous #335 | POSITIVE | 0 | 1 |
| Phengaris nausithous #338 | POSITIVE | 1 | 0 |
| Phengaris nausithous #339 | POSITIVE | 0 | 1 |
| Phengaris nausithous #340 | POSITIVE | 1 | 0 |
| Phengaris nausithous #341 | POSITIVE | 0 | 1 |
| Phengaris nausithous #342 | POSITIVE | 1 | 0 |
| Phengaris nausithous #343 | POSITIVE | 0 | 1 |
| Phengaris nausithous #344 | POSITIVE | 0 | 1 |
| Phengaris nausithous #345 | POSITIVE | 1 | 0 |
| Phengaris nausithous #352 | POSITIVE | 1 | 0 |
| Phengaris nausithous #354 | POSITIVE | 0 | 1 |
| Phengaris nausithous #361 | POSITIVE | 1 | 0 |
| Phengaris nausithous #362 | POSITIVE | 1 | 0 |
| Phengaris nausithous #363 | POSITIVE | 0 | 1 |
| Phengaris nausithous #364 | POSITIVE | 0 | 1 |
| Phengaris nausithous #365 | POSITIVE | 1 | 0 |
| Phengaris nausithous #367 | POSITIVE | 0 | 1 |
| Phengaris nausithous #371 | POSITIVE | 1 | 0 |
| Phengaris nausithous #372 | POSITIVE | 1 | 0 |
| Phengaris nausithous #373 | POSITIVE | 0 | 1 |
| Phengaris nausithous #374 | POSITIVE | 0 | 1 |
| Phengaris nausithous #375 | POSITIVE | 0 | 1 |
| Phengaris nausithous #377 | POSITIVE | 1 | 0 |
| Phengaris nausithous #380 | POSITIVE | 0 | 1 |
| Phengaris nausithous #382 | POSITIVE | 1 | 0 |
| Phengaris nausithous #383 | POSITIVE | 1 | 0 |
| Phengaris nausithous #385 | POSITIVE | 0 | 1 |
| Phengaris nausithous #387 | POSITIVE | 1 | 0 |
| Phengaris nausithous #391 | POSITIVE | 1 | 0 |
| Phengaris nausithous #394 | POSITIVE | 0 | 1 |
| Phengaris nausithous #395 | POSITIVE | 0 | 1 |
| Phengaris nausithous #396 | POSITIVE | 0 | 1 |
| Phengaris nausithous #404 | POSITIVE | 1 | 0 |
| Phengaris nausithous #406 | POSITIVE | 1 | 0 |
| Phengaris nausithous #410 | POSITIVE | 0 | 1 |
| Phengaris nausithous #411 | POSITIVE | 1 | 0 |
| Phengaris nausithous #412 | POSITIVE | 0 | 1 |
| Phengaris nausithous #414 | POSITIVE | 0 | 1 |
| Phengaris nausithous #415 | POSITIVE | 1 | 0 |
| Phengaris nausithous #416 | POSITIVE | 1 | 0 |
| Phengaris nausithous #417 | POSITIVE | 1 | 0 |
| Phengaris nausithous #420 | POSITIVE | 0 | 1 |
| Phengaris nausithous #423 | POSITIVE | 0 | 1 |
| Phengaris nausithous #426 | POSITIVE | 0 | 1 |
| Phengaris nausithous #429 | POSITIVE | 0 | 1 |
| Phengaris nausithous #435 | POSITIVE | 1 | 0 |
| Phengaris nausithous #439 | POSITIVE | 0 | 1 |
| Phengaris teleius #75 | POSITIVE | 0 | 1 |
| Phengaris teleius #76 | POSITIVE | 1 | 0 |
| Phengaris teleius #77 | POSITIVE | 0 | 1 |
| Phengaris teleius #78 | POSITIVE | 1 | 0 |
| Phengaris teleius #106 | POSITIVE | 0 | 1 |
| Phengaris teleius #107 | POSITIVE | 1 | 0 |
| Phengaris teleius #119 | POSITIVE | 0 | 1 |
| Phengaris teleius #120 | POSITIVE | 1 | 0 |
| Phengaris teleius #122 | POSITIVE | 0 | 1 |
| Phengaris teleius #123 | POSITIVE | 1 | 0 |
| Phengaris teleius #127 | POSITIVE | 0 | 1 |
| Phengaris teleius #129 | POSITIVE | 1 | 0 |

_Showing the first 60 of 437 rows._

### 07_occupancy_by_species.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous #1 | COUNT | 1747 | 1704 |
| Phengaris nausithous #2 | COUNT | 1469 | 1449 |
| Phengaris teleius #1 | COUNT | 1122 | 1110 |
| Phengaris teleius #2 | COUNT | 202 | 200 |

### 07_positivity_by_year.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous #1 | records | 771 | 769 |
| Phengaris nausithous #2 | records | 683 | 682 |
| Phengaris nausithous #3 | records | 538 | 523 |
| Phengaris nausithous #4 | records | 563 | 541 |
| Phengaris nausithous #5 | records | 291 | 280 |
| Phengaris nausithous #6 | records | 370 | 358 |
| Phengaris teleius #3 | records | 173 | 169 |
| Phengaris teleius #4 | records | 282 | 277 |
| Phengaris teleius #5 | records | 118 | 115 |
| Phengaris teleius #6 | records | 132 | 130 |
| Phengaris nausithous #1 | mean_positive | 0.535667963683528 | 0.53446033810143 |
| Phengaris nausithous #2 | mean_positive | 0.445095168374817 | 0.44574780058651 |
| Phengaris nausithous #3 | mean_positive | 0.429368029739777 | 0.432122370936902 |
| Phengaris nausithous #4 | mean_positive | 0.571936056838366 | 0.580406654343808 |
| Phengaris nausithous #5 | mean_positive | 0.415807560137457 | 0.421428571428571 |
| Phengaris nausithous #6 | mean_positive | 0.210810810810811 | 0.212290502793296 |
| Phengaris teleius #3 | mean_positive | 0.23121387283237 | 0.230769230769231 |
| Phengaris teleius #4 | mean_positive | 0.131205673758865 | 0.129963898916967 |
| Phengaris teleius #5 | mean_positive | 0.11864406779661 | 0.121739130434783 |
| Phengaris teleius #6 | mean_positive | 0.0757575757575758 | 0.0769230769230769 |

### 07_protection_evl.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous #1 | COUNT | 1469 | 1437 |
| Phengaris teleius #1 | COUNT | 943 | 936 |
| Phengaris nausithous #2 | COUNT | 1169 | 1153 |
| Phengaris nausithous #3 | COUNT | 278 | 267 |
| Phengaris teleius #3 | COUNT | 179 | 174 |
| Phengaris nausithous #4 | COUNT | 300 | 296 |
| Phengaris teleius #4 | COUNT | 89 | 87 |

### 07_protection_evl_combined.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous #1 | COUNT | 1469 | 1437 |
| Phengaris teleius #1 | COUNT | 943 | 936 |
| Phengaris nausithous #2 | COUNT | 1169 | 1153 |
| Phengaris nausithous #3 | COUNT | 160 | 153 |
| Phengaris teleius #3 | COUNT | 101 | 99 |
| Phengaris nausithous #4 | COUNT | 166 | 163 |
| Phengaris teleius #4 | COUNT | 47 | 45 |
| Phengaris nausithous #5 | COUNT | 118 | 114 |
| Phengaris teleius #5 | COUNT | 78 | 75 |
| Phengaris nausithous #6 | COUNT | 134 | 133 |

### 07_protection_evl_target.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous #1 | COUNT | 1629 | 1590 |
| Phengaris teleius #1 | COUNT | 1044 | 1035 |
| Phengaris nausithous #2 | COUNT | 1335 | 1316 |
| Phengaris teleius #2 | COUNT | 160 | 158 |
| Phengaris nausithous #3 | COUNT | 118 | 114 |
| Phengaris teleius #3 | COUNT | 78 | 75 |
| Phengaris nausithous #4 | COUNT | 134 | 133 |

### 07_protection_full.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous #1 | COUNT | 1439 | 1408 |
| Phengaris teleius #1 | COUNT | 929 | 922 |
| Phengaris nausithous #2 | COUNT | 1139 | 1123 |
| Phengaris nausithous #3 | COUNT | 30 | 29 |
| Phengaris nausithous #5 | COUNT | 109 | 104 |
| Phengaris teleius #5 | COUNT | 70 | 69 |
| Phengaris teleius #6 | COUNT | 33 | 32 |
| Phengaris nausithous #7 | COUNT | 51 | 49 |
| Phengaris teleius #7 | COUNT | 31 | 30 |
| Phengaris nausithous #8 | COUNT | 35 | 32 |
| Phengaris teleius #8 | COUNT | 14 | 13 |
| Phengaris nausithous #9 | COUNT | 91 | 90 |
| Phengaris nausithous #10 | COUNT | 110 | 109 |
| Phengaris nausithous #11 | COUNT | 27 | 24 |
| Phengaris teleius #11 | COUNT | 31 | 28 |

### 07_protection_mzchu.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous #1 | COUNT | 1639 | 1602 |
| Phengaris teleius #1 | COUNT | 1046 | 1038 |
| Phengaris nausithous #2 | COUNT | 1380 | 1363 |
| Phengaris teleius #2 | COUNT | 168 | 167 |
| Phengaris nausithous #3 | COUNT | 108 | 102 |
| Phengaris teleius #3 | COUNT | 76 | 72 |
| Phengaris nausithous #4 | COUNT | 89 | 86 |
| Phengaris teleius #4 | COUNT | 34 | 33 |

### 07_recorded_habitats_nausithous.csv

| row | column | before | after |
|---|---|---|---|
| other #1 | COUNT | 164 | 160 |
| managed grassland #1 | COUNT | 946 | 932 |
| neglected grassland #1 | COUNT | 370 | 369 |
| other #1 | PERC | 11.1640571817563 | 11.0420979986197 |
| road verge, ditch #1 | PERC | 11.9809394145677 | 12.1463077984817 |
| managed grassland #1 | PERC | 64.3975493533016 | 64.32022084196 |
| neglected grassland #1 | PERC | 25.1872021783526 | 25.4658385093168 |

### 07_recorded_habitats_teleius.csv

| row | column | before | after |
|---|---|---|---|
| managed grassland #1 | COUNT | 156 | 155 |
| other #1 | PERC | 7.42574257425743 | 7.5 |
| road verge, ditch #1 | PERC | 7.42574257425743 | 7.5 |
| managed grassland #1 | PERC | 77.2277227722772 | 77.5 |
| neglected grassland #1 | PERC | 21.2871287128713 | 21.5 |

### 07_records_by_year.csv

| row | column | before | after |
|---|---|---|---|
| row #2 | COUNT | 453 | 451 |
| row #3 | COUNT | 726 | 725 |
| row #5 | COUNT | 440 | 427 |
| row #6 | COUNT | 271 | 265 |
| row #7 | COUNT | 486 | 468 |
| row #8 | COUNT | 359 | 350 |
| row #9 | COUNT | 274 | 263 |
| row #10 | COUNT | 135 | 132 |
| row #11 | COUNT | 414 | 402 |
| row #12 | COUNT | 88 | 86 |

### 07_records_per_observer.csv

| row | column | before | after |
|---|---|---|---|
| Papoušek Zdeněk #1 | obs_num | 772 | 762 |
| Tájek Přemysl #1 | obs_num | 342 | 335 |
| Beneš Jiří #1 | obs_num | 339 | 330 |
| Spitzer Lukáš #1 | obs_num | 299 | 296 |
| Fišer Marek #1 | obs_num | 192 | 184 |
| Švarc Jiří #1 | obs_num | 182 | 177 |
| Hrnčíř Jan #1 | obs_num | 160 | 156 |
| Valda Slávek #1 | obs_num | 119 | 115 |
| Jiří Švarc #1 | obs_num | 105 | 101 |
| Klimešová Kateřina #1 | obs_num | 83 | 81 |
| Slavomír Valda #1 | obs_num | 70 | 64 |
| Křivan Václav #1 | obs_num | 62 | 58 |
| Ričl David #1 | obs_num | 58 | 56 |
| Moravec Pavel #1 | obs_num | 56 | 54 |
| Vrabel Michal #1 | obs_num | 35 | 34 |
| Antonín Krása #1 | obs_num | 26 | 25 |
| Hanč Zdeněk #1 | obs_num | 22 | 20 |
| Krása Antonín #1 | obs_num | 21 | 20 |
| Pokorný Jiří #1 | obs_num | 19 | 18 |
| Jiří Pokorný #1 | obs_num | 15 | 14 |

### 07_species_cooccurrence.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous #1 | COUNT | 1747 | 1704 |
| Phengaris teleius #1 | COUNT | 1122 | 1110 |
| Phengaris nausithous #2 | COUNT | 1364 | 1345 |
| Phengaris nausithous #3 | COUNT | 105 | 104 |
| Phengaris teleius #2 | COUNT | 202 | 200 |

### 09_nausithous_aic_grassland.csv

| row | column | before | after |
|---|---|---|---|
| nau_management_ttp \| Mowing timing by method by grassland type #1 | n_obs | 1752 | 1710 |
| nau_ttp \| Regularly managed grassland #1 | n_obs | 3195 | 3132 |
| nau_management_ttp \| Mowing timing by method by grassland type #1 | AIC | 2291.90962705483 | 2242.31496479881 |
| nau_ttp \| Regularly managed grassland #1 | AIC | 4283.39885676527 | 4206.79710767477 |
| nau_management_ttp \| Mowing timing by method by grassland type #1 | BIC | 2330.1892199553 | 2280.42470534529 |
| nau_ttp \| Regularly managed grassland #1 | BIC | 4307.67622623252 | 4230.99481590322 |
| nau_management_ttp \| Mowing timing by method by grassland type #1 | logLik | -1138.95481352742 | -1114.15748239941 |
| nau_ttp \| Regularly managed grassland #1 | logLik | -2137.69942838264 | -2099.39855383739 |
| nau_ttp \| Regularly managed grassland #1 | delta_AIC | 1991.48922971044 | 1964.48214287596 |

### 09_nausithous_aic_habitat.csv

| row | column | before | after |
|---|---|---|---|
| nau_plant \| Host plant abundance #1 | n_obs | 1805 | 1742 |
| nau_resource_density \| Site area by host plant abundance #1 | n_obs | 1805 | 1742 |
| nau_area_null \| Null model on records with a positive site area #1 | n_obs | 1805 | 1742 |
| nau_area \| Site area #1 | n_obs | 1805 | 1742 |
| nau_area_poly \| Site area, quadratic #1 | n_obs | 1805 | 1742 |
| nau_plant \| Host plant abundance #1 | AIC | 2267.29550619458 | 2193.95144558564 |
| nau_resource_density \| Site area by host plant abundance #1 | AIC | 2268.73173883587 | 2196.16748511272 |
| nau_area_null \| Null model on records with a positive site area #1 | AIC | 2318.38215782943 | 2245.98699317204 |
| nau_area \| Site area #1 | AIC | 2318.55389175973 | 2246.89335548103 |
| nau_area_poly \| Site area, quadratic #1 | AIC | 2320.25747029359 | 2248.59773966929 |
| nau_plant \| Host plant abundance #1 | BIC | 2289.28876967764 | 2215.80260221529 |
| nau_resource_density \| Site area by host plant abundance #1 | BIC | 2301.72163406048 | 2228.94422005719 |
| nau_area_null \| Null model on records with a positive site area #1 | BIC | 2334.87710544173 | 2262.37536064427 |
| nau_area \| Site area #1 | BIC | 2340.54715524279 | 2268.74451211068 |
| nau_area_poly \| Site area, quadratic #1 | BIC | 2347.74904964742 | 2275.91168545635 |
| nau_plant \| Host plant abundance #1 | logLik | -1129.64775309729 | -1092.97572279282 |
| nau_resource_density \| Site area by host plant abundance #1 | logLik | -1128.36586941794 | -1092.08374255636 |
| nau_area_null \| Null model on records with a positive site area #1 | logLik | -1156.19107891472 | -1119.99349658602 |
| nau_area \| Site area #1 | logLik | -1155.27694587986 | -1119.44667774052 |
| nau_area_poly \| Site area, quadratic #1 | logLik | -1155.12873514679 | -1119.29886983465 |
| nau_resource_density \| Site area by host plant abundance #1 | delta_AIC | 1.43623264129656 | 2.21603952707619 |
| nau_area_null \| Null model on records with a positive site area #1 | delta_AIC | 51.0866516348556 | 52.0355475863939 |
| nau_area \| Site area #1 | delta_AIC | 51.2583855651492 | 52.9419098953877 |
| nau_area_poly \| Site area, quadratic #1 | delta_AIC | 52.9619640990095 | 54.6462940836477 |

### 09_nausithous_aic_management.csv

| row | column | before | after |
|---|---|---|---|
| nau_method_timing \| Mowing method by timing (selected management model) #1 | n_obs | 1753 | 1711 |
| nau_timing \| Mowing timing #1 | n_obs | 1753 | 1711 |
| nau_management_het \| Mowing timing by method by within-site heterogeneity #1 | n_obs | 1753 | 1711 |
| nau_method \| Mowing method #1 | n_obs | 1753 | 1711 |
| nau_mow_null \| Null model on records where mowing was assessed #1 | n_obs | 1753 | 1711 |
| nau_method_timing \| Mowing method by timing (selected management model) #1 | AIC | 2294.86936954759 | 2245.16166382103 |
| nau_timing \| Mowing timing #1 | AIC | 2296.03878367488 | 2245.73058591778 |
| nau_management_het \| Mowing timing by method by within-site heterogeneity #1 | AIC | 2300.60426823777 | 2251.05047516745 |
| nau_method \| Mowing method #1 | AIC | 2356.08276928432 | 2305.97092654138 |
| nau_mow_null \| Null model on records where mowing was assessed #1 | AIC | 2363.42783466398 | 2313.14065004997 |
| nau_method_timing \| Mowing method by timing (selected management model) #1 | BIC | 2327.68387285712 | 2277.83066346438 |
| nau_timing \| Mowing timing #1 | BIC | 2317.91511921457 | 2267.50991901335 |
| nau_management_het \| Mowing timing by method by within-site heterogeneity #1 | BIC | 2355.29510708698 | 2305.49880790637 |
| nau_method \| Mowing method #1 | BIC | 2377.959104824 | 2327.75025963694 |
| nau_mow_null \| Null model on records where mowing was assessed #1 | BIC | 2379.83508631874 | 2329.47514987164 |
| nau_method_timing \| Mowing method by timing (selected management model) #1 | logLik | -1141.4346847738 | -1116.58083191051 |
| nau_timing \| Mowing timing #1 | logLik | -1144.01939183744 | -1118.86529295889 |
| nau_management_het \| Mowing timing by method by within-site heterogeneity #1 | logLik | -1140.30213411888 | -1115.52523758372 |
| nau_method \| Mowing method #1 | logLik | -1174.04138464216 | -1148.98546327069 |
| nau_mow_null \| Null model on records where mowing was assessed #1 | logLik | -1178.71391733199 | -1153.57032502498 |
| nau_timing \| Mowing timing #1 | delta_AIC | 1.16941412729011 | 0.568922096753795 |
| nau_management_het \| Mowing timing by method by within-site heterogeneity #1 | delta_AIC | 5.73489869017567 | 5.88881134642179 |
| nau_method \| Mowing method #1 | delta_AIC | 61.2133997367255 | 60.80926272035 |
| nau_mow_null \| Null model on records where mowing was assessed #1 | delta_AIC | 68.5584651163877 | 67.9789862289417 |

### 09_nausithous_aic_natura2000.csv

| row | column | before | after |
|---|---|---|---|
| nau_evl \| Natura 2000 membership #1 | n_obs | 3216 | 3153 |
| nau_evl_combined \| Natura 2000 membership and designation (selected protection model) #1 | n_obs | 3216 | 3153 |
| nau_evl_target \| Natura 2000 designated for Phengaris #1 | n_obs | 3216 | 3153 |
| nau_evl \| Natura 2000 membership #1 | AIC | 4301.58781709382 | 4224.31157148259 |
| nau_evl_combined \| Natura 2000 membership and designation (selected protection model) #1 | AIC | 4303.32988333231 | 4226.0745763398 |
| nau_evl_target \| Natura 2000 designated for Phengaris #1 | AIC | 4306.62492957612 | 4229.89898544921 |
| nau_evl \| Natura 2000 membership #1 | BIC | 4325.89139161501 | 4248.53601012077 |
| nau_evl_combined \| Natura 2000 membership and designation (selected protection model) #1 | BIC | 4333.7093514838 | 4256.35512463752 |
| nau_evl_target \| Natura 2000 designated for Phengaris #1 | BIC | 4330.92850409731 | 4254.12342408739 |
| nau_evl \| Natura 2000 membership #1 | logLik | -2146.79390854691 | -2108.15578574129 |
| nau_evl_combined \| Natura 2000 membership and designation (selected protection model) #1 | logLik | -2146.66494166615 | -2108.0372881699 |
| nau_evl_target \| Natura 2000 designated for Phengaris #1 | logLik | -2149.31246478806 | -2110.94949272461 |
| nau_evl_combined \| Natura 2000 membership and designation (selected protection model) #1 | delta_AIC | 1.74206623848931 | 1.76300485721094 |
| nau_evl_target \| Natura 2000 designated for Phengaris #1 | delta_AIC | 5.03711248229683 | 5.58741396662663 |

### 09_nausithous_coefficients.csv

| row | column | before | after |
|---|---|---|---|
| nau_null \| Null model with year and site random effects \| (Intercept) #1 | estimate | -0.297290426751254 | -0.275562394124457 |
| nau_year_factor \| Year as a factor \| (Intercept) #1 | estimate | 0.142914606560338 | 0.138060228095524 |
| nau_year_factor \| Year as a factor \| as.factor(YEAR)2020 #1 | estimate | -0.363423110236533 | -0.355926722308889 |
| nau_year_factor \| Year as a factor \| as.factor(YEAR)2021 #1 | estimate | -0.427344643625748 | -0.411257367625929 |
| nau_year_factor \| Year as a factor \| as.factor(YEAR)2022 #1 | estimate | 0.146840005493398 | 0.186382740331349 |
| nau_year_factor \| Year as a factor \| as.factor(YEAR)2023 #1 | estimate | -0.482922498013837 | -0.454971938862206 |
| nau_year_factor \| Year as a factor \| as.factor(YEAR)2024 #1 | estimate | -1.46295958213835 | -1.44923395874655 |
| nau_year_linear \| Year as a linear trend \| (Intercept) #1 | estimate | 324.092080507398 | 312.792783186835 |
| nau_year_linear \| Year as a linear trend \| as.numeric(YEAR) #1 | estimate | -0.160449051939693 | -0.154854185496389 |
| nau_year_poly \| Year as a quadratic trend \| (Intercept) #1 | estimate | -0.182343099025034 | -0.170577278363757 |
| nau_year_poly \| Year as a quadratic trend \| poly(as.numeric(YEAR), 2)1 #1 | estimate | -15.6517277555354 | -14.9561257284106 |
| nau_year_poly \| Year as a quadratic trend \| poly(as.numeric(YEAR), 2)2 #1 | estimate | -10.2449390247266 | -10.3726598880068 |
| nau_spatial \| Spatial position \| (Intercept) #1 | estimate | 2.55983063689651 | 2.56352644980756 |
| nau_spatiotemporal \| Year and spatial position \| (Intercept) #1 | estimate | 3.39683943078548 | 3.36371162212884 |
| nau_spatiotemporal \| Year and spatial position \| as.factor(YEAR)2020 #1 | estimate | -0.54003379952567 | -0.531001839329952 |
| nau_spatiotemporal \| Year and spatial position \| as.factor(YEAR)2021 #1 | estimate | -0.580490656407816 | -0.562187833880366 |
| nau_spatiotemporal \| Year and spatial position \| as.factor(YEAR)2022 #1 | estimate | 0.0229181432365471 | 0.0596859322237843 |
| nau_spatiotemporal \| Year and spatial position \| as.factor(YEAR)2023 #1 | estimate | -0.79089881280134 | -0.762346157235948 |
| nau_spatiotemporal \| Year and spatial position \| as.factor(YEAR)2024 #1 | estimate | -1.68805004788343 | -1.66750967743873 |
| nau_area_null \| Null model on records with a positive site area \| (Intercept) #1 | estimate | 0.151310032043075 | 0.174698746176986 |
| nau_area \| Site area \| (Intercept) #1 | estimate | -0.000162935234042129 | 0.0630535126494014 |
| nau_area \| Site area \| log10(AREA_SITE) #1 | estimate | 0.0632282722893747 | 0.0463172706604357 |
| nau_area_null_glm \| Intercept-only model on records with a known site area \| (Intercept) #1 | estimate | -0.173318133764737 | -0.162104764945165 |
| nau_area_poly \| Site area, quadratic \| (Intercept) #1 | estimate | 0.151627462567179 | 0.173793407265621 |
| nau_area_poly \| Site area, quadratic \| poly(log10(AREA_SITE), 2)1 #1 | estimate | 3.14399746696416 | 2.25479785405631 |
| nau_area_poly \| Site area, quadratic \| poly(log10(AREA_SITE), 2)2 #1 | estimate | 1.26432631402476 | 1.17125290017209 |
| nau_plant \| Host plant abundance \| (Intercept) #1 | estimate | -0.829180625918103 | -0.756737657305242 |
| nau_plant \| Host plant abundance \| PLANT_QUANT #1 | estimate | 0.670415499228875 | 0.635314859359412 |
| nau_plant_poly \| Host plant abundance, quadratic \| (Intercept) #1 | estimate | -0.293123020854911 | -0.269291889907231 |
| nau_plant_poly \| Host plant abundance, quadratic \| poly(PLANT_QUANT, 2)1 #1 | estimate | 24.1242426658973 | 23.0881927127345 |
| nau_plant_poly \| Host plant abundance, quadratic \| poly(PLANT_QUANT, 2)2 #1 | estimate | -8.90436055279238 | -8.43488127582046 |
| nau_resource_density \| Site area by host plant abundance \| (Intercept) #1 | estimate | -1.30685998850869 | -1.14008135106023 |
| nau_resource_density \| Site area by host plant abundance \| log(AREA_SITE) #1 | estimate | 0.0866042324652403 | 0.068799589133229 |
| nau_resource_density \| Site area by host plant abundance \| as.numeric(PLANT_QUANT) #1 | estimate | 0.93267977513101 | 0.859243863048338 |
| nau_resource_density \| Site area by host plant abundance \| log(AREA_SITE):as.numeric(PLANT_QUANT) #1 | estimate | -0.0474707157090441 | -0.040131714800419 |
| nau_mow_null \| Null model on records where mowing was assessed \| (Intercept) #1 | estimate | -0.248710120476715 | -0.224711708782824 |
| nau_timing \| Mowing timing \| (Intercept) #1 | estimate | -0.940488875979724 | -0.877113834413583 |
| nau_timing \| Mowing timing \| as.factor(TIMING)1 #1 | estimate | 0.990026614902762 | 0.939571038758419 |
| nau_method \| Mowing method \| (Intercept) #1 | estimate | -0.407504414104635 | -0.374748063540757 |
| nau_method \| Mowing method \| as.factor(METHOD)1 #1 | estimate | 0.331115843938583 | 0.314238949202192 |
| nau_method_timing \| Mowing method by timing (selected management model) \| (Intercept) #1 | estimate | -1.09105656324504 | -1.01100264310045 |
| nau_method_timing \| Mowing method by timing (selected management model) \| as.factor(METHOD)1 #1 | estimate | 0.49741352018943 | 0.445176842153569 |
| nau_method_timing \| Mowing method by timing (selected management model) \| as.factor(TIMING)1 #1 | estimate | 1.16205314825266 | 1.09323069687379 |
| nau_method_timing \| Mowing method by timing (selected management model) \| as.factor(METHOD)1:as.factor(TIMING)1 #1 | estimate | -0.53383286104827 | -0.478698144961246 |
| nau_mow_null_glm \| Intercept-only model on records where mowing was assessed \| (Intercept) #1 | estimate | -0.152032034887639 | -0.139324879387728 |
| nau_graze_null \| Null model on records where grazing was assessed \| (Intercept) #1 | estimate | -0.243209504689761 | -0.193684616901953 |
| nau_graze \| Grazing present \| (Intercept) #1 | estimate | -0.300349574336127 | -0.279531647381791 |
| nau_graze \| Grazing present \| as.factor(GRAZE)1 #1 | estimate | 0.107129645311489 | 0.138772207160822 |
| nau_graze_method \| Grazing intensity \| (Intercept) #1 | estimate | -0.549196902794933 | -0.454773235110146 |
| nau_graze_method \| Grazing intensity \| as.factor(GRAZE_MET)1 #1 | estimate | 0.376691932131465 | 0.305282548770004 |
| nau_management_het \| Mowing timing by method by within-site heterogeneity \| (Intercept) #1 | estimate | -0.835350850854421 | -0.779189253058248 |
| nau_management_het \| Mowing timing by method by within-site heterogeneity \| as.factor(TIMING)1 #1 | estimate | 0.641416580302252 | 0.684455411906642 |
| nau_management_het \| Mowing timing by method by within-site heterogeneity \| as.factor(METHOD)1 #1 | estimate | -0.165489777846486 | -0.202109532718672 |
| nau_management_het \| Mowing timing by method by within-site heterogeneity \| HET_INN #1 | estimate | -0.193863830591063 | -0.175727044763205 |
| nau_management_het \| Mowing timing by method by within-site heterogeneity \| as.factor(TIMING)1:as.factor(METHOD)1 #1 | estimate | 0.448600938354166 | 0.372550841227677 |
| nau_management_het \| Mowing timing by method by within-site heterogeneity \| as.factor(TIMING)1:HET_INN #1 | estimate | 0.440534387421146 | 0.340943691427406 |
| nau_management_het \| Mowing timing by method by within-site heterogeneity \| as.factor(METHOD)1:HET_INN #1 | estimate | 0.458927353851428 | 0.446073400094477 |
| nau_management_het \| Mowing timing by method by within-site heterogeneity \| as.factor(TIMING)1:as.factor(METHOD)1:HET_INN #1 | estimate | -0.74601962715785 | -0.631421156960519 |
| nau_protect \| Any protection \| (Intercept) #1 | estimate | -0.365224799903257 | -0.343786156029965 |
| nau_protect \| Any protection \| as.factor(PROTECT)1 #1 | estimate | 0.338277042184847 | 0.343660630960797 |

_Showing the first 60 of 382 rows._

### 09_nausithous_fit_statistics.csv

| row | column | before | after |
|---|---|---|---|
| nau_null \| Null model with year and site random effects \| Baseline \| glmer \| ok #1 | n_obs | 3216 | 3153 |
| nau_year_factor \| Year as a factor \| Baseline \| glm \| ok #1 | n_obs | 3216 | 3153 |
| nau_year_linear \| Year as a linear trend \| Baseline \| glm \| ok #1 | n_obs | 3216 | 3153 |
| nau_year_poly \| Year as a quadratic trend \| Baseline \| glm \| ok #1 | n_obs | 3216 | 3153 |
| nau_spatial \| Spatial position \| Baseline \| glm \| ok #1 | n_obs | 3216 | 3153 |
| nau_spatiotemporal \| Year and spatial position \| Baseline \| glm \| ok #1 | n_obs | 3216 | 3153 |
| nau_area_null \| Null model on records with a positive site area \| Habitat \| glmer \| ok #1 | n_obs | 1805 | 1742 |
| nau_area \| Site area \| Habitat \| glmer \| ok #1 | n_obs | 1805 | 1742 |
| nau_area_null_glm \| Intercept-only model on records with a known site area \| Habitat \| glm \| ok #1 | n_obs | 3216 | 3153 |
| nau_area_poly \| Site area, quadratic \| Habitat \| glmer \| ok #1 | n_obs | 1805 | 1742 |
| nau_plant \| Host plant abundance \| Habitat \| glmer \| ok #1 | n_obs | 1805 | 1742 |
| nau_plant_poly \| Host plant abundance, quadratic \| Habitat \| glmer \| ok #1 | n_obs | 3216 | 3153 |
| nau_resource_density \| Site area by host plant abundance \| Habitat \| glmer \| ok #1 | n_obs | 1805 | 1742 |
| nau_mow_null \| Null model on records where mowing was assessed \| Management \| glmer \| ok #1 | n_obs | 1753 | 1711 |
| nau_timing \| Mowing timing \| Management \| glmer \| ok #1 | n_obs | 1753 | 1711 |
| nau_method \| Mowing method \| Management \| glmer \| ok #1 | n_obs | 1753 | 1711 |
| nau_method_timing \| Mowing method by timing (selected management model) \| Management \| glmer \| ok #1 | n_obs | 1753 | 1711 |
| nau_mow_null_glm \| Intercept-only model on records where mowing was assessed \| Management \| glm \| ok #1 | n_obs | 1753 | 1711 |
| nau_graze_null \| Null model on records where grazing was assessed \| Management \| glmer \| ok #1 | n_obs | 77 | 75 |
| nau_graze \| Grazing present \| Management \| glmer \| ok #1 | n_obs | 3216 | 3153 |
| nau_graze_method \| Grazing intensity \| Management \| glmer \| ok #1 | n_obs | 77 | 75 |
| nau_management_het \| Mowing timing by method by within-site heterogeneity \| Management \| glmer \| fitted with warnings #1 | n_obs | 1753 | 1711 |
| nau_protect \| Any protection \| Conservation \| glmer \| ok #1 | n_obs | 3216 | 3153 |
| nau_evl \| Natura 2000 membership \| Conservation \| glmer \| ok #1 | n_obs | 3216 | 3153 |
| nau_evl_target \| Natura 2000 designated for Phengaris \| Conservation \| glmer \| ok #1 | n_obs | 3216 | 3153 |
| nau_evl_combined \| Natura 2000 membership and designation (selected protection model) \| Conservation \| glmer \| ok #1 | n_obs | 3216 | 3153 |
| nau_evl_year \| Natura 2000 membership by year \| Conservation \| glmer \| fitted with warnings #1 | n_obs | 3216 | 3153 |
| nau_mzchu \| Small-scale protected area \| Conservation \| glmer \| ok #1 | n_obs | 3216 | 3153 |
| nau_mzchu_evl \| Small-scale protected area by Natura 2000 \| Conservation \| glmer \| ok #1 | n_obs | 3216 | 3153 |
| nau_ttp \| Regularly managed grassland \| Conservation \| glmer \| ok #1 | n_obs | 3195 | 3132 |
| nau_management_ttp \| Mowing timing by method by grassland type \| Conservation \| glmer \| ok #1 | n_obs | 1752 | 1710 |
| nau_fsb \| Habitat quality evaluation \| Conservation \| glmer \| ok #1 | n_obs | 3216 | 3153 |
| nau_het_inner \| Within-site habitat heterogeneity \| Conservation \| glmer \| ok #1 | n_obs | 3216 | 3153 |
| nau_het_outer \| Between-habitat heterogeneity \| Conservation \| glmer \| ok #1 | n_obs | 3216 | 3153 |
| nau_threats \| Number of threats and pressures \| Conservation \| glmer \| ok #1 | n_obs | 1805 | 1742 |
| nau_null \| Null model with year and site random effects \| Baseline \| glmer \| ok #1 | AIC | 4310.62354607341 | 4234.28931391716 |
| nau_year_factor \| Year as a factor \| Baseline \| glm \| ok #1 | AIC | 4295.57947726522 | 4214.48328370882 |
| nau_year_linear \| Year as a linear trend \| Baseline \| glm \| ok #1 | AIC | 4383.19726445433 | 4304.00089780814 |
| nau_year_poly \| Year as a quadratic trend \| Baseline \| glm \| ok #1 | AIC | 4360.64297449137 | 4280.75317436406 |
| nau_spatial \| Spatial position \| Baseline \| glm \| ok #1 | AIC | 4280.37106758743 | 4199.96485808099 |
| nau_spatiotemporal \| Year and spatial position \| Baseline \| glm \| ok #1 | AIC | 4106.77754328929 | 4031.93110386905 |
| nau_area_null \| Null model on records with a positive site area \| Habitat \| glmer \| ok #1 | AIC | 2318.38215782943 | 2245.98699317204 |
| nau_area \| Site area \| Habitat \| glmer \| ok #1 | AIC | 2318.55389175973 | 2246.89335548103 |
| nau_area_null_glm \| Intercept-only model on records with a known site area \| Habitat \| glm \| ok #1 | AIC | 4436.261552875 | 4352.34036346321 |
| nau_area_poly \| Site area, quadratic \| Habitat \| glmer \| ok #1 | AIC | 2320.25747029359 | 2248.59773966929 |
| nau_plant \| Host plant abundance \| Habitat \| glmer \| ok #1 | AIC | 2267.29550619458 | 2193.95144558564 |
| nau_plant_poly \| Host plant abundance, quadratic \| Habitat \| glmer \| ok #1 | AIC | 4182.28840639304 | 4104.73784117391 |
| nau_resource_density \| Site area by host plant abundance \| Habitat \| glmer \| ok #1 | AIC | 2268.73173883587 | 2196.16748511272 |
| nau_mow_null \| Null model on records where mowing was assessed \| Management \| glmer \| ok #1 | AIC | 2363.42783466398 | 2313.14065004997 |
| nau_timing \| Mowing timing \| Management \| glmer \| ok #1 | AIC | 2296.03878367488 | 2245.73058591778 |
| nau_method \| Mowing method \| Management \| glmer \| ok #1 | AIC | 2356.08276928432 | 2305.97092654138 |
| nau_method_timing \| Mowing method by timing (selected management model) \| Management \| glmer \| ok #1 | AIC | 2294.86936954759 | 2245.16166382103 |
| nau_mow_null_glm \| Intercept-only model on records where mowing was assessed \| Management \| glm \| ok #1 | AIC | 2422.07361026744 | 2365.66651994062 |
| nau_graze_null \| Null model on records where grazing was assessed \| Management \| glmer \| ok #1 | AIC | 111.54762073429 | 109.298922069296 |
| nau_graze \| Grazing present \| Management \| glmer \| ok #1 | AIC | 4312.44952814752 | 4235.98229764988 |
| nau_graze_method \| Grazing intensity \| Management \| glmer \| ok #1 | AIC | 113.18654804333 | 111.055585577011 |
| nau_management_het \| Mowing timing by method by within-site heterogeneity \| Management \| glmer \| fitted with warnings #1 | AIC | 2300.60426823777 | 2251.05047516745 |
| nau_protect \| Any protection \| Conservation \| glmer \| ok #1 | AIC | 4300.4562438795 | 4222.9244165344 |
| nau_evl \| Natura 2000 membership \| Conservation \| glmer \| ok #1 | AIC | 4301.58781709382 | 4224.31157148259 |
| nau_evl_target \| Natura 2000 designated for Phengaris \| Conservation \| glmer \| ok #1 | AIC | 4306.62492957612 | 4229.89898544921 |

_Showing the first 60 of 175 rows._

### 09_nausithous_random_effects.csv

| row | column | before | after |
|---|---|---|---|
| nau_null \| Null model with year and site random effects \| X:Y \| (Intercept) #1 | variance | 0.260823212999397 | 0.0883706639529877 |
| nau_null \| Null model with year and site random effects \| YEAR \| (Intercept) #1 | variance | 0.279702362399849 | 0.262665983116897 |
| nau_area_null \| Null model on records with a positive site area \| X:Y \| (Intercept) #1 | variance | 0.400406190281458 | 0.0676951261545825 |
| nau_area_null \| Null model on records with a positive site area \| YEAR \| (Intercept) #1 | variance | 3.73419896847066 | 3.10397135082836 |
| nau_area \| Site area \| X:Y \| (Intercept) #1 | variance | 0.393228837202534 | 0.0665444508740821 |
| nau_area \| Site area \| YEAR \| (Intercept) #1 | variance | 3.75445552830356 | 3.12734935798137 |
| nau_area_poly \| Site area, quadratic \| X:Y \| (Intercept) #1 | variance | 0.392683138130889 | 0.0656722707049707 |
| nau_area_poly \| Site area, quadratic \| YEAR \| (Intercept) #1 | variance | 3.76792945794257 | 3.13971399051894 |
| nau_plant \| Host plant abundance \| X:Y \| (Intercept) #1 | variance | 0.484334714493422 | 0.0920058738677968 |
| nau_plant \| Host plant abundance \| YEAR \| (Intercept) #1 | variance | 3.53055376507443 | 2.89386587321271 |
| nau_plant_poly \| Host plant abundance, quadratic \| X:Y \| (Intercept) #1 | variance | 0.305105920907793 | 0.0931982314047389 |
| nau_plant_poly \| Host plant abundance, quadratic \| YEAR \| (Intercept) #1 | variance | 0.252478253783621 | 0.235242689949194 |
| nau_resource_density \| Site area by host plant abundance \| X:Y \| (Intercept) #1 | variance | 0.484478485237221 | 0.0950458978552773 |
| nau_resource_density \| Site area by host plant abundance \| YEAR \| (Intercept) #1 | variance | 3.5515062948588 | 2.91510217529376 |
| nau_mow_null \| Null model on records where mowing was assessed \| X:Y \| (Intercept) #1 | variance | 0.322326210418631 | 0.10238192560835 |
| nau_mow_null \| Null model on records where mowing was assessed \| YEAR \| (Intercept) #1 | variance | 0.270522812939194 | 0.245493310237923 |
| nau_timing \| Mowing timing \| X:Y \| (Intercept) #1 | variance | 0.388867506699684 | 0.124868603491937 |
| nau_timing \| Mowing timing \| YEAR \| (Intercept) #1 | variance | 0.2718685429237 | 0.243413676439595 |
| nau_method \| Mowing method \| X:Y \| (Intercept) #1 | variance | 0.328293372672537 | 0.104217581760775 |
| nau_method \| Mowing method \| YEAR \| (Intercept) #1 | variance | 0.276708175762048 | 0.249791383618339 |
| nau_method_timing \| Mowing method by timing (selected management model) \| X:Y \| (Intercept) #1 | variance | 0.400316997330685 | 0.135178763033707 |
| nau_method_timing \| Mowing method by timing (selected management model) \| YEAR \| (Intercept) #1 | variance | 0.27452560584887 | 0.244352466315981 |
| nau_graze_null \| Null model on records where grazing was assessed \| X:Y \| (Intercept) #1 | variance | 0.275229572590398 | 0.0866550349016212 |
| nau_graze_null \| Null model on records where grazing was assessed \| YEAR \| (Intercept) #1 | variance | 0 | 0.00461050179317519 |
| nau_graze \| Grazing present \| X:Y \| (Intercept) #1 | variance | 0.261583050807288 | 0.0888771469651709 |
| nau_graze \| Grazing present \| YEAR \| (Intercept) #1 | variance | 0.280753055414496 | 0.263952136095425 |
| nau_graze_method \| Grazing intensity \| X:Y \| (Intercept) #1 | variance | 0.2598140148352 | 0.0690963775680833 |
| nau_graze_method \| Grazing intensity \| YEAR \| (Intercept) #1 | variance | 0.00953648507710546 | 0.0293890113409965 |
| nau_management_het \| Mowing timing by method by within-site heterogeneity \| X:Y \| (Intercept) #1 | variance | 0.403969369310471 | 0.137482797381215 |
| nau_management_het \| Mowing timing by method by within-site heterogeneity \| YEAR \| (Intercept) #1 | variance | 0.275291142434801 | 0.244951628234025 |
| nau_protect \| Any protection \| X:Y \| (Intercept) #1 | variance | 0.26838404623855 | 0.0891294492315746 |
| nau_protect \| Any protection \| YEAR \| (Intercept) #1 | variance | 0.283256065522 | 0.265891430979784 |
| nau_evl \| Natura 2000 membership \| X:Y \| (Intercept) #1 | variance | 0.266452411661594 | 0.0881561754053097 |
| nau_evl \| Natura 2000 membership \| YEAR \| (Intercept) #1 | variance | 0.282361777886593 | 0.264951089811872 |
| nau_evl_target \| Natura 2000 designated for Phengaris \| X:Y \| (Intercept) #1 | variance | 0.264436396546707 | 0.0882780119816633 |
| nau_evl_target \| Natura 2000 designated for Phengaris \| YEAR \| (Intercept) #1 | variance | 0.282211859142823 | 0.26491986231596 |
| nau_evl_combined \| Natura 2000 membership and designation (selected protection model) \| X:Y \| (Intercept) #1 | variance | 0.266859977333554 | 0.0882925972241141 |
| nau_evl_combined \| Natura 2000 membership and designation (selected protection model) \| YEAR \| (Intercept) #1 | variance | 0.282769823120311 | 0.265317329320232 |
| nau_evl_year \| Natura 2000 membership by year \| X:Y \| (Intercept) #1 | variance | 0.266582124231587 | 0.0953251334000599 |
| nau_mzchu \| Small-scale protected area \| X:Y \| (Intercept) #1 | variance | 0.261074899171556 | 0.0884501465343395 |
| nau_mzchu \| Small-scale protected area \| YEAR \| (Intercept) #1 | variance | 0.279913317059889 | 0.2629917401288 |
| nau_mzchu_evl \| Small-scale protected area by Natura 2000 \| X:Y \| (Intercept) #1 | variance | 0.267963880642484 | 0.0894291770017953 |
| nau_mzchu_evl \| Small-scale protected area by Natura 2000 \| YEAR \| (Intercept) #1 | variance | 0.281776775394737 | 0.264236572408376 |
| nau_ttp \| Regularly managed grassland \| X:Y \| (Intercept) #1 | variance | 0.263996743504199 | 0.0892611133246966 |
| nau_ttp \| Regularly managed grassland \| YEAR \| (Intercept) #1 | variance | 0.279724110288574 | 0.262722023441469 |
| nau_management_ttp \| Mowing timing by method by grassland type \| X:Y \| (Intercept) #1 | variance | 0.4006591198501 | 0.135480913757298 |
| nau_management_ttp \| Mowing timing by method by grassland type \| YEAR \| (Intercept) #1 | variance | 0.274801898827034 | 0.244453281190822 |
| nau_fsb \| Habitat quality evaluation \| X:Y \| (Intercept) #1 | variance | 0.264665066815367 | 0.0944447899674976 |
| nau_fsb \| Habitat quality evaluation \| YEAR \| (Intercept) #1 | variance | 0.261347156224545 | 0.247005538994098 |
| nau_het_inner \| Within-site habitat heterogeneity \| X.Y \| (Intercept) #1 | variance | 0.261597934139237 | 0.0892195573463056 |
| nau_het_inner \| Within-site habitat heterogeneity \| YEAR \| (Intercept) #1 | variance | 0.0597407040658635 | 0.0779724447049436 |
| nau_het_inner \| Within-site habitat heterogeneity \| YEAR.1 \| (Intercept) #1 | variance | 0.221575089132644 | 0.185996001346967 |
| nau_het_outer \| Between-habitat heterogeneity \| X:Y \| (Intercept) #1 | variance | 0.261396690733532 | 0.0888678480529596 |
| nau_het_outer \| Between-habitat heterogeneity \| YEAR \| (Intercept) #1 | variance | 0.280193202508749 | 0.262654554029555 |
| nau_threats \| Number of threats and pressures \| X:Y \| (Intercept) #1 | variance | 0.418395635412387 | 0.0716580055552901 |
| nau_threats \| Number of threats and pressures \| YEAR \| (Intercept) #1 | variance | 3.58048401778931 | 2.93601631512092 |
| nau_null \| Null model with year and site random effects \| X:Y \| (Intercept) #1 | sd | 0.510708540166891 | 0.297272036950985 |
| nau_null \| Null model with year and site random effects \| YEAR \| (Intercept) #1 | sd | 0.528868946337227 | 0.512509495635834 |
| nau_area_null \| Null model on records with a positive site area \| X:Y \| (Intercept) #1 | sd | 0.632776572165451 | 0.26018287060178 |
| nau_area_null \| Null model on records with a positive site area \| YEAR \| (Intercept) #1 | sd | 1.93240755754853 | 1.76180911305066 |

_Showing the first 60 of 112 rows._

### 10_teleius_aic_habitat.csv

| row | column | before | after |
|---|---|---|---|
| tel_area_null \| Null model on records with a positive site area #1 | n_obs | 713 | 699 |
| tel_area \| Site area (selected habitat model) #1 | n_obs | 713 | 699 |
| tel_plant \| Host plant abundance, all records #1 | n_obs | 1324 | 1310 |
| tel_plant_poly \| Host plant abundance, quadratic #1 | n_obs | 1324 | 1310 |
| tel_area_null \| Null model on records with a positive site area #1 | AIC | 583.790103417863 | 573.82208360047 |
| tel_area \| Site area (selected habitat model) #1 | AIC | 585.728099107691 | 575.635853575056 |
| tel_plant \| Host plant abundance, all records #1 | AIC | 648.479092107318 | 648.228276439653 |
| tel_plant_poly \| Host plant abundance, quadratic #1 | AIC | 650.422002526688 | 650.173947840762 |
| tel_area_null \| Null model on records with a positive site area #1 | BIC | 597.498547679106 | 587.471035827171 |
| tel_area \| Site area (selected habitat model) #1 | BIC | 604.006024789348 | 593.834456543991 |
| tel_plant \| Host plant abundance, all records #1 | BIC | 669.232743053306 | 668.939406104434 |
| tel_plant_poly \| Host plant abundance, quadratic #1 | BIC | 676.364066209173 | 676.062859921738 |
| tel_area_null \| Null model on records with a positive site area #1 | logLik | -288.895051708932 | -283.911041800235 |
| tel_area \| Site area (selected habitat model) #1 | logLik | -288.864049553846 | -283.817926787528 |
| tel_plant \| Host plant abundance, all records #1 | logLik | -320.239546053659 | -320.114138219827 |
| tel_plant_poly \| Host plant abundance, quadratic #1 | logLik | -320.211001263344 | -320.086973920381 |
| tel_area \| Site area (selected habitat model) #1 | delta_AIC | 1.93799568982797 | 1.81376997458574 |
| tel_plant \| Host plant abundance, all records #1 | delta_AIC | 64.6889886894546 | 74.4061928391832 |
| tel_plant_poly \| Host plant abundance, quadratic #1 | delta_AIC | 66.6318991088245 | 76.3518642402923 |

### 10_teleius_aic_management.csv

| row | column | before | after |
|---|---|---|---|
| tel_method_timing \| Mowing method by timing, all records #1 | n_obs | 698 | 689 |
| tel_method \| Mowing method, all records #1 | n_obs | 706 | 697 |
| tel_timing \| Mowing timing, all records #1 | n_obs | 754 | 744 |
| tel_method_timing \| Mowing method by timing, all records #1 | AIC | 611.025689437014 | 608.139560832748 |
| tel_method \| Mowing method, all records #1 | AIC | 624.035582795518 | 621.414905392081 |
| tel_timing \| Mowing timing, all records #1 | AIC | 689.560365324474 | 684.423498981235 |
| tel_method_timing \| Mowing method by timing, all records #1 | BIC | 638.315004053588 | 635.35100845883 |
| tel_method \| Mowing method, all records #1 | BIC | 642.274043745491 | 639.602047035123 |
| tel_timing \| Mowing timing, all records #1 | BIC | 708.061934796506 | 702.871663120567 |
| tel_method_timing \| Mowing method by timing, all records #1 | logLik | -299.512844718507 | -298.069780416374 |
| tel_method \| Mowing method, all records #1 | logLik | -308.017791397759 | -306.70745269604 |
| tel_timing \| Mowing timing, all records #1 | logLik | -340.780182662237 | -338.211749490617 |
| tel_method \| Mowing method, all records #1 | delta_AIC | 13.0098933585041 | 13.2753445593327 |
| tel_timing \| Mowing timing, all records #1 | delta_AIC | 78.5346758874603 | 76.2839381484863 |

### 10_teleius_aic_management_subset.csv

| row | column | before | after |
|---|---|---|---|
| tel_timing_mow_subset \| Mowing timing, records where mowing was assessed #1 | n_obs | 698 | 689 |
| tel_method_timing_mow_subset \| Mowing method by timing, records where mowing was assessed #1 | n_obs | 698 | 689 |
| tel_method_mow_subset \| Mowing method, records where mowing was assessed #1 | n_obs | 698 | 689 |
| tel_mow_null \| Null model on records where mowing was assessed #1 | n_obs | 698 | 689 |
| tel_timing_mow_subset \| Mowing timing, records where mowing was assessed #1 | AIC | 608.001130754466 | 605.051658041988 |
| tel_method_timing_mow_subset \| Mowing method by timing, records where mowing was assessed #1 | AIC | 611.025689437014 | 608.139560832748 |
| tel_method_mow_subset \| Mowing method, records where mowing was assessed #1 | AIC | 614.423558483703 | 611.810710210255 |
| tel_mow_null \| Null model on records where mowing was assessed #1 | AIC | 616.079672875027 | 613.389141896559 |
| tel_timing_mow_subset \| Mowing timing, records where mowing was assessed #1 | BIC | 626.194007165516 | 623.192623126043 |
| tel_method_timing_mow_subset \| Mowing method by timing, records where mowing was assessed #1 | BIC | 638.315004053588 | 635.35100845883 |
| tel_method_mow_subset \| Mowing method, records where mowing was assessed #1 | BIC | 632.616434894753 | 629.951675294309 |
| tel_mow_null \| Null model on records where mowing was assessed #1 | BIC | 629.724330183315 | 626.9948657096 |
| tel_timing_mow_subset \| Mowing timing, records where mowing was assessed #1 | logLik | -300.000565377233 | -298.525829020994 |
| tel_method_timing_mow_subset \| Mowing method by timing, records where mowing was assessed #1 | logLik | -299.512844718507 | -298.069780416374 |
| tel_method_mow_subset \| Mowing method, records where mowing was assessed #1 | logLik | -303.211779241852 | -301.905355105127 |
| tel_mow_null \| Null model on records where mowing was assessed #1 | logLik | -305.039836437514 | -303.69457094828 |
| tel_method_timing_mow_subset \| Mowing method by timing, records where mowing was assessed #1 | delta_AIC | 3.02455868254776 | 3.08790279076038 |
| tel_method_mow_subset \| Mowing method, records where mowing was assessed #1 | delta_AIC | 6.42242772923669 | 6.75905216826663 |
| tel_mow_null \| Null model on records where mowing was assessed #1 | delta_AIC | 8.078542120561 | 8.33748385457147 |

### 10_teleius_aic_protection.csv

| row | column | before | after |
|---|---|---|---|
| tel_evl \| Natura 2000 membership #1 | n_obs | 1324 | 1310 |
| tel_evl_combined \| Natura 2000 membership and designation (selected protection model) #1 | n_obs | 1324 | 1310 |
| tel_protection_combined \| Natura 2000 membership, designation and small-scale protection #1 | n_obs | 1324 | 1310 |
| tel_evl_target \| Natura 2000 designated for Phengaris #1 | n_obs | 1324 | 1310 |
| tel_evl \| Natura 2000 membership #1 | AIC | 1058.94362745628 | 1050.16154534026 |
| tel_evl_combined \| Natura 2000 membership and designation (selected protection model) #1 | AIC | 1059.91849123945 | 1050.81430914765 |
| tel_protection_combined \| Natura 2000 membership, designation and small-scale protection #1 | AIC | 1061.12967963845 | 1051.89176667012 |
| tel_evl_target \| Natura 2000 designated for Phengaris #1 | AIC | 1097.26197807516 | 1085.86149614149 |
| tel_evl \| Natura 2000 membership #1 | BIC | 1079.69727840226 | 1070.87267500504 |
| tel_evl_combined \| Natura 2000 membership and designation (selected protection model) #1 | BIC | 1085.86055492193 | 1076.70322122863 |
| tel_protection_combined \| Natura 2000 membership, designation and small-scale protection #1 | BIC | 1092.26015605743 | 1082.95846116729 |
| tel_evl_target \| Natura 2000 designated for Phengaris #1 | BIC | 1118.01562902115 | 1106.57262580627 |
| tel_evl \| Natura 2000 membership #1 | logLik | -525.471813728138 | -521.080772670129 |
| tel_evl_combined \| Natura 2000 membership and designation (selected protection model) #1 | logLik | -524.959245619723 | -520.407154573826 |
| tel_protection_combined \| Natura 2000 membership, designation and small-scale protection #1 | logLik | -524.564839819223 | -519.945883335061 |
| tel_evl_target \| Natura 2000 designated for Phengaris #1 | logLik | -544.630989037582 | -538.930748070744 |
| tel_evl_combined \| Natura 2000 membership and designation (selected protection model) #1 | delta_AIC | 0.974863783170804 | 0.65276380739374 |
| tel_protection_combined \| Natura 2000 membership, designation and small-scale protection #1 | delta_AIC | 2.1860521821709 | 1.73022132986284 |
| tel_evl_target \| Natura 2000 designated for Phengaris #1 | delta_AIC | 38.3183506188891 | 35.6999508012291 |

### 10_teleius_coefficients.csv

| row | column | before | after |
|---|---|---|---|
| tel_null \| Null model with year and site random effects \| (Intercept) #1 | estimate | -1.89521987821216 | -1.80623110449908 |
| tel_null_glm \| Intercept-only model \| (Intercept) #1 | estimate | -1.71460037336851 | -1.71379791317856 |
| tel_year_factor \| Year as a factor \| as.factor(YEAR)2021 #1 | estimate | 0.251314428280883 | 0.248811298062796 |
| tel_year_factor \| Year as a factor \| as.factor(YEAR)2022 #1 | estimate | -0.437556195511783 | -0.44849389264583 |
| tel_year_factor \| Year as a factor \| as.factor(YEAR)2023 #1 | estimate | -0.552549467137428 | -0.523279084837272 |
| tel_year_factor \| Year as a factor \| as.factor(YEAR)2024 #1 | estimate | -1.04865184934943 | -1.03212254739854 |
| tel_year_linear \| Year as a linear trend \| (Intercept) #1 | estimate | 299.162489848539 | 293.984297993551 |
| tel_year_linear \| Year as a linear trend \| as.numeric(YEAR) #1 | estimate | -0.148879719536778 | -0.146317791309989 |
| tel_year_poly \| Year as a quadratic trend \| (Intercept) #1 | estimate | -1.7411346801382 | -1.73884604634636 |
| tel_year_poly \| Year as a quadratic trend \| poly(as.numeric(YEAR), 2)1 #1 | estimate | -9.28807986557777 | -9.02637637999632 |
| tel_year_poly \| Year as a quadratic trend \| poly(as.numeric(YEAR), 2)2 #1 | estimate | -4.63154248391773 | -4.34598741075048 |
| tel_spatial \| Spatial position \| (Intercept) #1 | estimate | -2.28793010099511 | -2.29932189771463 |
| tel_spatiotemporal \| Year and spatial position, 2018 excluded \| (Intercept) #1 | estimate | 285.496630989709 | 279.504127138631 |
| tel_spatiotemporal \| Year and spatial position, 2018 excluded \| as.numeric(YEAR) #1 | estimate | -0.142272572559909 | -0.139316619601966 |
| tel_mapping_field \| Mapping field as a fixed effect \| (Intercept) #1 | estimate | -21.5661150571776 | -21.566346075962 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5056 #1 | estimate | 43.1321836924634 | 43.1324147291408 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5152 #1 | estimate | 22.2592622377387 | 22.2594932565243 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5153 #1 | estimate | 4.65361698932431e-05 | 0.000277553471443812 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5154 #1 | estimate | 4.65338240248522e-05 | 0.000277551688578128 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5155 #1 | estimate | 4.65365061824696e-05 | 0.000277554589664708 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5156 #1 | estimate | 21.5661150571783 | 21.5663460759623 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5250 #1 | estimate | 21.2296428205561 | 21.2298738393407 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5252 #1 | estimate | 22.2592622377361 | 22.2594932565209 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5253 #1 | estimate | 4.65372253266738e-05 | 0.000277557726427209 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5254 #1 | estimate | 20.4675027685095 | 20.4677337872958 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5255 #1 | estimate | 4.65349008735523e-05 | 0.000277552413660236 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5256 #1 | estimate | 4.65336361862718e-05 | 0.000277553379508305 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5257 #1 | estimate | 21.5661150571785 | 21.5663460759616 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5351 #1 | estimate | 43.1321835914452 | 43.1324146082762 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5352 #1 | estimate | 21.8537971296295 | 21.8540281484149 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5353 #1 | estimate | 23.6455565988587 | 23.6457876176425 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5354 #1 | estimate | 20.4675027685091 | 20.4677337872948 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5355 #1 | estimate | 4.65344627948552e-05 | 0.000277552630792827 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5356 #1 | estimate | 4.65340768043209e-05 | 0.000277553304326345 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5357 #1 | estimate | 4.65342677320382e-05 | 0.000277553048937493 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5361 #1 | estimate | 4.65343146127344e-05 | 0.000277553117306044 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5445 #1 | estimate | 4.65345622341496e-05 | 0.000277553289453051 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5453 #1 | estimate | 4.65289984519058e-05 | 0.000277551800253219 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5454 #1 | estimate | 19.486673515498 | 19.486904534283 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5455 #1 | estimate | 4.65355570206671e-05 | 0.00027755267340099 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5456 #1 | estimate | 20.8729678766173 | 20.8731988954032 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5457 #1 | estimate | 4.65340428673423e-05 | 0.000277552924781474 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5458 #1 | estimate | 4.65344663220928e-05 | 0.000277553660474582 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5459 #1 | estimate | 4.65347221378015e-05 | 0.00027755363000524 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5545 #1 | estimate | 4.65341732952777e-05 | 0.000277552672227974 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5552 #1 | estimate | 4.65333536327902e-05 | 0.000277553458664327 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5553 #1 | estimate | 4.65337718216911e-05 | 0.000277553832859957 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5554 #1 | estimate | 4.65342191376162e-05 | 0.00027755292813261 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5555 #1 | estimate | 4.653432461096e-05 | 0.000277553093536797 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5556 #1 | estimate | 4.6534974026346e-05 | 0.000277552725865119 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5557 #1 | estimate | 4.65346503047994e-05 | 0.000277553742969912 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5558 #1 | estimate | 4.65343767305568e-05 | 0.000277553165985665 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5642 #1 | estimate | 21.2784329847259 | 21.2786640035103 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5643 #1 | estimate | 4.65342242512507e-05 | 0.000277553166634848 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5644 #1 | estimate | 4.65342570992726e-05 | 0.000277553196147898 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5652 #1 | estimate | 4.65340388886682e-05 | 0.00027755327018768 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5653 #1 | estimate | 21.5661150571779 | 21.5663460759626 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5655 #1 | estimate | 4.65343102324445e-05 | 0.00027755365209042 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5656 #1 | estimate | 4.65342018475116e-05 | 0.000277553579837764 |
| tel_mapping_field \| Mapping field as a fixed effect \| as.factor(SITMAP)5742 #1 | estimate | 21.0552894334125 | 21.0555204521963 |

_Showing the first 60 of 1185 rows._

### 10_teleius_fit_statistics.csv

| row | column | before | after |
|---|---|---|---|
| tel_null \| Null model with year and site random effects \| Baseline \| glmer \| ok #1 | n_obs | 1324 | 1310 |
| tel_null_glm \| Intercept-only model \| Baseline \| glm \| ok #1 | n_obs | 1324 | 1310 |
| tel_year_factor \| Year as a factor \| Baseline \| glm \| ok #1 | n_obs | 1324 | 1310 |
| tel_year_linear \| Year as a linear trend \| Baseline \| glm \| ok #1 | n_obs | 1324 | 1310 |
| tel_year_poly \| Year as a quadratic trend \| Baseline \| glm \| ok #1 | n_obs | 1324 | 1310 |
| tel_spatial \| Spatial position \| Baseline \| glm \| ok #1 | n_obs | 1324 | 1310 |
| tel_spatiotemporal \| Year and spatial position, 2018 excluded \| Baseline \| glm \| ok #1 | n_obs | 1324 | 1310 |
| tel_mapping_field \| Mapping field as a fixed effect \| Baseline \| glm \| ok #1 | n_obs | 1324 | 1310 |
| tel_random_only \| Year and site random effects only \| Baseline \| glmer \| ok #1 | n_obs | 1324 | 1310 |
| tel_area_null \| Null model on records with a positive site area \| Habitat \| glmer \| ok #1 | n_obs | 713 | 699 |
| tel_area \| Site area (selected habitat model) \| Habitat \| glmer \| ok #1 | n_obs | 713 | 699 |
| tel_area_null_glm \| Intercept-only model on records with a positive site area \| Habitat \| glm \| ok #1 | n_obs | 713 | 699 |
| tel_plant_area_subset \| Host plant abundance, records with a positive site area \| Habitat \| glmer \| ok #1 | n_obs | 713 | 699 |
| tel_plant \| Host plant abundance, all records \| Habitat \| glmer \| ok #1 | n_obs | 1324 | 1310 |
| tel_plant_poly \| Host plant abundance, quadratic \| Habitat \| glmer \| ok #1 | n_obs | 1324 | 1310 |
| tel_mow_null \| Null model on records where mowing was assessed \| Management \| glmer \| ok #1 | n_obs | 698 | 689 |
| tel_timing_mow_subset \| Mowing timing, records where mowing was assessed \| Management \| glmer \| ok #1 | n_obs | 698 | 689 |
| tel_method_timing_mow_subset \| Mowing method by timing, records where mowing was assessed \| Management \| glmer \| ok #1 | n_obs | 698 | 689 |
| tel_mow_null_glm \| Intercept-only model on records where mowing was assessed \| Management \| glm \| ok #1 | n_obs | 698 | 689 |
| tel_graze \| Grazing present \| Management \| glmer \| ok #1 | n_obs | 1324 | 1310 |
| tel_method \| Mowing method, all records \| Management \| glmer \| ok #1 | n_obs | 706 | 697 |
| tel_timing \| Mowing timing, all records \| Management \| glmer \| ok #1 | n_obs | 754 | 744 |
| tel_method_timing \| Mowing method by timing, all records \| Management \| glmer \| ok #1 | n_obs | 698 | 689 |
| tel_protect \| Any protection \| Conservation \| glmer \| ok #1 | n_obs | 1324 | 1310 |
| tel_evl \| Natura 2000 membership \| Conservation \| glmer \| ok #1 | n_obs | 1324 | 1310 |
| tel_evl_target \| Natura 2000 designated for Phengaris \| Conservation \| glmer \| ok #1 | n_obs | 1324 | 1310 |
| tel_evl_combined \| Natura 2000 membership and designation (selected protection model) \| Conservation \| glmer \| ok #1 | n_obs | 1324 | 1310 |
| tel_protection_combined \| Natura 2000 membership, designation and small-scale protection \| Conservation \| glmer \| ok #1 | n_obs | 1324 | 1310 |
| tel_mzchu \| Small-scale protected area \| Conservation \| glmer \| ok #1 | n_obs | 1324 | 1310 |
| tel_fsb_subset \| Habitat quality evaluation, T / X / mosaic only \| Conservation \| glmer \| ok #1 | n_obs | 633 | 624 |
| tel_het_inner \| Within-site habitat heterogeneity \| Conservation \| glmer \| ok #1 | n_obs | 1324 | 1310 |
| tel_het_inner_dup_year \| Within-site habitat heterogeneity, year term repeated \| Conservation \| glmer \| ok #1 | n_obs | 1324 | 1310 |
| tel_het_outer \| Between-habitat heterogeneity \| Conservation \| glmer \| ok #1 | n_obs | 1324 | 1310 |
| tel_null \| Null model with year and site random effects \| Baseline \| glmer \| ok #1 | AIC | 1131.19337852043 | 1120.99559737719 |
| tel_null_glm \| Intercept-only model \| Baseline \| glm \| ok #1 | AIC | 1133.0607913244 | 1121.56703045204 |
| tel_year_factor \| Year as a factor \| Baseline \| glm \| ok #1 | AIC | 1124.24918175332 | 1113.46573520256 |
| tel_year_linear \| Year as a linear trend \| Baseline \| glm \| ok #1 | AIC | 1126.44271348718 | 1115.32568156428 |
| tel_year_poly \| Year as a quadratic trend \| Baseline \| glm \| ok #1 | AIC | 1126.04088796778 | 1115.19938412453 |
| tel_spatial \| Spatial position \| Baseline \| glm \| ok #1 | AIC | 1133.51556724075 | 1121.96659104702 |
| tel_spatiotemporal \| Year and spatial position, 2018 excluded \| Baseline \| glm \| ok #1 | AIC | 1127.98772986405 | 1116.8206105734 |
| tel_mapping_field \| Mapping field as a fixed effect \| Baseline \| glm \| ok #1 | AIC | 796.609906591305 | 795.429978018676 |
| tel_random_only \| Year and site random effects only \| Baseline \| glmer \| ok #1 | AIC | 1131.19337852043 | 1120.99559737719 |
| tel_area_null \| Null model on records with a positive site area \| Habitat \| glmer \| ok #1 | AIC | 583.790103417863 | 573.82208360047 |
| tel_area \| Site area (selected habitat model) \| Habitat \| glmer \| ok #1 | AIC | 585.728099107691 | 575.635853575056 |
| tel_area_null_glm \| Intercept-only model on records with a positive site area \| Habitat \| glm \| ok #1 | AIC | 587.337567493105 | 575.85430590294 |
| tel_plant_area_subset \| Host plant abundance, records with a positive site area \| Habitat \| glmer \| ok #1 | AIC | 335.852456366259 | 565.465463945242 |
| tel_plant \| Host plant abundance, all records \| Habitat \| glmer \| ok #1 | AIC | 648.479092107318 | 648.228276439653 |
| tel_plant_poly \| Host plant abundance, quadratic \| Habitat \| glmer \| ok #1 | AIC | 650.422002526688 | 650.173947840762 |
| tel_mow_null \| Null model on records where mowing was assessed \| Management \| glmer \| ok #1 | AIC | 616.079672875027 | 613.389141896559 |
| tel_timing_mow_subset \| Mowing timing, records where mowing was assessed \| Management \| glmer \| ok #1 | AIC | 608.001130754466 | 605.051658041988 |
| tel_method_timing_mow_subset \| Mowing method by timing, records where mowing was assessed \| Management \| glmer \| ok #1 | AIC | 611.025689437014 | 608.139560832748 |
| tel_mow_null_glm \| Intercept-only model on records where mowing was assessed \| Management \| glm \| ok #1 | AIC | 616.839326901912 | 613.668748736373 |
| tel_graze \| Grazing present \| Management \| glmer \| ok #1 | AIC | 648.48172239332 | 1110.89950612174 |
| tel_method \| Mowing method, all records \| Management \| glmer \| ok #1 | AIC | 624.035582795518 | 621.414905392081 |
| tel_timing \| Mowing timing, all records \| Management \| glmer \| ok #1 | AIC | 689.560365324474 | 684.423498981235 |
| tel_method_timing \| Mowing method by timing, all records \| Management \| glmer \| ok #1 | AIC | 611.025689437014 | 608.139560832748 |
| tel_protect \| Any protection \| Conservation \| glmer \| ok #1 | AIC | 1047.98507701271 | 1039.30016201631 |
| tel_evl \| Natura 2000 membership \| Conservation \| glmer \| ok #1 | AIC | 1058.94362745628 | 1050.16154534026 |
| tel_evl_target \| Natura 2000 designated for Phengaris \| Conservation \| glmer \| ok #1 | AIC | 1097.26197807516 | 1085.86149614149 |
| tel_evl_combined \| Natura 2000 membership and designation (selected protection model) \| Conservation \| glmer \| ok #1 | AIC | 1059.91849123945 | 1050.81430914765 |

_Showing the first 60 of 165 rows._

### 10_teleius_random_effects.csv

| row | column | before | after |
|---|---|---|---|
| tel_null \| Null model with year and site random effects \| X:Y \| (Intercept) #1 | variance | 0.413734040361455 | 0.157490063411053 |
| tel_null \| Null model with year and site random effects \| YEAR \| (Intercept) #1 | variance | 0.0767954688414187 | 0.0878996905267326 |
| tel_random_only \| Year and site random effects only \| X:Y \| (Intercept) #1 | variance | 0.413734040361455 | 0.157490063411053 |
| tel_random_only \| Year and site random effects only \| YEAR \| (Intercept) #1 | variance | 0.0767954688414187 | 0.0878996905267326 |
| tel_area_null \| Null model on records with a positive site area \| X:Y \| (Intercept) #1 | variance | 0.568088686354672 | 0.123396663432198 |
| tel_area_null \| Null model on records with a positive site area \| YEAR \| (Intercept) #1 | variance | 0.132599147502217 | 0.145185480877242 |
| tel_area \| Site area (selected habitat model) \| X:Y \| (Intercept) #1 | variance | 0.56726933583454 | 0.116310519816957 |
| tel_area \| Site area (selected habitat model) \| YEAR \| (Intercept) #1 | variance | 0.132845947421806 | 0.145297525850248 |
| tel_plant_area_subset \| Host plant abundance, records with a positive site area \| X:Y \| (Intercept) #1 | variance | 1575.75122502862 | 0.335804456132218 |
| tel_plant_area_subset \| Host plant abundance, records with a positive site area \| YEAR \| (Intercept) #1 | variance | 0 | 0.0917212866680151 |
| tel_plant \| Host plant abundance, all records \| X:Y \| (Intercept) #1 | variance | 1744.5693174141 | 1734.35370285048 |
| tel_plant_poly \| Host plant abundance, quadratic \| X:Y \| (Intercept) #1 | variance | 1743.32003488491 | 1734.46500626391 |
| tel_mow_null \| Null model on records where mowing was assessed \| X:Y \| (Intercept) #1 | variance | 0.375591159928449 | 0.308707684634067 |
| tel_mow_null \| Null model on records where mowing was assessed \| YEAR \| (Intercept) #1 | variance | 0.161084549243356 | 0.155764428664152 |
| tel_timing_mow_subset \| Mowing timing, records where mowing was assessed \| X:Y \| (Intercept) #1 | variance | 0.424314818372531 | 0.337211370911493 |
| tel_timing_mow_subset \| Mowing timing, records where mowing was assessed \| YEAR \| (Intercept) #1 | variance | 0.145499175964537 | 0.140483767722424 |
| tel_method_mow_subset \| Mowing method, records where mowing was assessed \| X:Y \| (Intercept) #1 | variance | 0.31911763752506 | 0.265221529553421 |
| tel_method_mow_subset \| Mowing method, records where mowing was assessed \| YEAR \| (Intercept) #1 | variance | 0.179125433007384 | 0.171048803865965 |
| tel_method_timing_mow_subset \| Mowing method by timing, records where mowing was assessed \| X:Y \| (Intercept) #1 | variance | 0.38216371096701 | 0.306400828247073 |
| tel_method_timing_mow_subset \| Mowing method by timing, records where mowing was assessed \| YEAR \| (Intercept) #1 | variance | 0.159492479701709 | 0.152226875772749 |
| tel_graze \| Grazing present \| X:Y \| (Intercept) #1 | variance | 1745.45323654602 | 0.172725920955408 |
| tel_graze \| Grazing present \| YEAR \| (Intercept) #1 | variance | 0 | 0.0890804901364312 |
| tel_method \| Mowing method, all records \| X:Y \| (Intercept) #1 | variance | 0.322768731428737 | 0.265977746064672 |
| tel_method \| Mowing method, all records \| YEAR \| (Intercept) #1 | variance | 0.159111857806282 | 0.151194392526378 |
| tel_timing \| Mowing timing, all records \| X:Y \| (Intercept) #1 | variance | 0.30660133239759 | 0.200556012248986 |
| tel_timing \| Mowing timing, all records \| YEAR \| (Intercept) #1 | variance | 0.214162222081454 | 0.183777379759583 |
| tel_method_timing \| Mowing method by timing, all records \| X:Y \| (Intercept) #1 | variance | 0.38216371096701 | 0.306400828247073 |
| tel_method_timing \| Mowing method by timing, all records \| YEAR \| (Intercept) #1 | variance | 0.159492479701709 | 0.152226875772749 |
| tel_protect \| Any protection \| X:Y \| (Intercept) #1 | variance | 0.172148607803055 | 2.26266099478342e-08 |
| tel_protect \| Any protection \| YEAR \| (Intercept) #1 | variance | 0.177095562939535 | 0.163288583044195 |
| tel_evl \| Natura 2000 membership \| X:Y \| (Intercept) #1 | variance | 0.179980929888483 | 1.83196241133673e-11 |
| tel_evl \| Natura 2000 membership \| YEAR \| (Intercept) #1 | variance | 0.164809630361821 | 0.152527250646817 |
| tel_evl_target \| Natura 2000 designated for Phengaris \| X:Y \| (Intercept) #1 | variance | 0.354918277462528 | 0.107400088781387 |
| tel_evl_target \| Natura 2000 designated for Phengaris \| YEAR \| (Intercept) #1 | variance | 0.160754559233529 | 0.15659454713741 |
| tel_evl_combined \| Natura 2000 membership and designation (selected protection model) \| X:Y \| (Intercept) #1 | variance | 0.187284917236823 | 4.62107082614708e-10 |
| tel_evl_combined \| Natura 2000 membership and designation (selected protection model) \| YEAR \| (Intercept) #1 | variance | 0.179244226999848 | 0.167039548514112 |
| tel_protection_combined \| Natura 2000 membership, designation and small-scale protection \| X:Y \| (Intercept) #1 | variance | 0.194153534728242 | 2.18310262689064e-10 |
| tel_protection_combined \| Natura 2000 membership, designation and small-scale protection \| YEAR \| (Intercept) #1 | variance | 0.187152269655984 | 0.174274985017217 |
| tel_mzchu \| Small-scale protected area \| X:Y \| (Intercept) #1 | variance | 0.255638892338077 | 0.0598317332178456 |
| tel_mzchu \| Small-scale protected area \| YEAR \| (Intercept) #1 | variance | 0.125616701689456 | 0.118693366515139 |
| tel_fsb \| Habitat quality evaluation, all levels \| X:Y \| (Intercept) #1 | variance | 0.333205834057698 | 0.0808083325735446 |
| tel_fsb \| Habitat quality evaluation, all levels \| YEAR \| (Intercept) #1 | variance | 0.0843176409046337 | 0.087433698737257 |
| tel_fsb_subset \| Habitat quality evaluation, T / X / mosaic only \| X:Y \| (Intercept) #1 | variance | 0.220162762452551 | 0.0829028821815087 |
| tel_fsb_subset \| Habitat quality evaluation, T / X / mosaic only \| YEAR \| (Intercept) #1 | variance | 0.184676965817017 | 0.178125705203093 |
| tel_het_inner \| Within-site habitat heterogeneity \| X:Y \| (Intercept) #1 | variance | 0.41374189102076 | 0.156645065400927 |
| tel_het_inner \| Within-site habitat heterogeneity \| YEAR \| (Intercept) #1 | variance | 0.076831789333664 | 0.0877388366985527 |
| tel_het_inner_dup_year \| Within-site habitat heterogeneity, year term repeated \| X.Y \| (Intercept) #1 | variance | 0.413737530186091 | 0.156623142396336 |
| tel_het_inner_dup_year \| Within-site habitat heterogeneity, year term repeated \| YEAR \| (Intercept) #1 | variance | 0.00206910912073398 | 0.0690172513612521 |
| tel_het_inner_dup_year \| Within-site habitat heterogeneity, year term repeated \| YEAR.1 \| (Intercept) #1 | variance | 0.074761329040413 | 0.018723034132174 |
| tel_het_outer \| Between-habitat heterogeneity \| X:Y \| (Intercept) #1 | variance | 0.413948919133758 | 0.157801831757029 |
| tel_het_outer \| Between-habitat heterogeneity \| YEAR \| (Intercept) #1 | variance | 0.076804862589332 | 0.0878910573602348 |
| tel_null \| Null model with year and site random effects \| X:Y \| (Intercept) #1 | sd | 0.643221610614456 | 0.39685017753688 |
| tel_null \| Null model with year and site random effects \| YEAR \| (Intercept) #1 | sd | 0.27711995388535 | 0.2964788196933 |
| tel_random_only \| Year and site random effects only \| X:Y \| (Intercept) #1 | sd | 0.643221610614456 | 0.39685017753688 |
| tel_random_only \| Year and site random effects only \| YEAR \| (Intercept) #1 | sd | 0.27711995388535 | 0.2964788196933 |
| tel_area_null \| Null model on records with a positive site area \| X:Y \| (Intercept) #1 | sd | 0.753716582247381 | 0.351278612261263 |
| tel_area_null \| Null model on records with a positive site area \| YEAR \| (Intercept) #1 | sd | 0.364141658564654 | 0.381032125781071 |
| tel_area \| Site area (selected habitat model) \| X:Y \| (Intercept) #1 | sd | 0.753172845922196 | 0.341043281442337 |
| tel_area \| Site area (selected habitat model) \| YEAR \| (Intercept) #1 | sd | 0.364480380023131 | 0.381179125674856 |
| tel_plant_area_subset \| Host plant abundance, records with a positive site area \| X:Y \| (Intercept) #1 | sd | 39.695733083401 | 0.579486372688968 |

_Showing the first 60 of 103 rows._

### 11_both_species_aic.csv

| row | column | before | after |
|---|---|---|---|
| both_mapped_habitat \| Species and mapped habitat code #1 | n_obs | 684 | 682 |
| both_cooccurrence \| Co-occurrence of the two species by species #1 | n_obs | 4540 | 4463 |
| both_recorded_habitat \| Species and recorded habitat types #1 | n_obs | 4514 | 4437 |
| both_management_type \| Species, mowing and grazing #1 | n_obs | 4540 | 4463 |
| both_species \| Species only #1 | n_obs | 4540 | 4463 |
| both_mapped_habitat \| Species and mapped habitat code #1 | AIC | 907.025497948758 | 904.865524889532 |
| both_cooccurrence \| Co-occurrence of the two species by species #1 | AIC | 5286.22596071042 | 5200.17780233785 |
| both_recorded_habitat \| Species and recorded habitat types #1 | AIC | 5382.38226996854 | 5304.26647109329 |
| both_management_type \| Species, mowing and grazing #1 | AIC | 5420.32842762412 | 5342.57652995075 |
| both_species \| Species only #1 | AIC | 5425.52476733462 | 5348.50015120603 |
| both_mapped_habitat \| Species and mapped habitat code #1 | BIC | 1029.28036172457 | 1027.04132565131 |
| both_cooccurrence \| Co-occurrence of the two species by species #1 | BIC | 5318.32937216559 | 5232.19568466099 |
| both_recorded_habitat \| Species and recorded habitat types #1 | BIC | 5440.11672058494 | 5361.8460748557 |
| both_management_type \| Species, mowing and grazing #1 | BIC | 5458.85252137033 | 5380.99798873852 |
| both_species \| Species only #1 | BIC | 5451.20749649876 | 5374.11445706455 |
| both_mapped_habitat \| Species and mapped habitat code #1 | logLik | -426.512748974379 | -425.432762444766 |
| both_cooccurrence \| Co-occurrence of the two species by species #1 | logLik | -2638.11298035521 | -2595.08890116892 |
| both_recorded_habitat \| Species and recorded habitat types #1 | logLik | -2682.19113498427 | -2643.13323554664 |
| both_management_type \| Species, mowing and grazing #1 | logLik | -2704.16421381206 | -2665.28826497537 |
| both_species \| Species only #1 | logLik | -2708.76238366731 | -2670.25007560302 |
| both_cooccurrence \| Co-occurrence of the two species by species #1 | delta_AIC | 4379.20046276166 | 4295.31227744832 |
| both_recorded_habitat \| Species and recorded habitat types #1 | delta_AIC | 4475.35677201979 | 4399.40094620376 |
| both_management_type \| Species, mowing and grazing #1 | delta_AIC | 4513.30292967536 | 4437.71100506121 |
| both_species \| Species only #1 | delta_AIC | 4518.49926938586 | 4443.6346263165 |

### 11_both_species_coefficients.csv

| row | column | before | after |
|---|---|---|---|
| both_species \| Species only \| (Intercept) #1 | estimate | -0.294512305357821 | -0.278592816389215 |
| both_species \| Species only \| as.factor(DRUH)Phengaris teleius #1 | estimate | -1.78736523920027 | -1.72151058829094 |
| both_management_type \| Species, mowing and grazing \| (Intercept) #1 | estimate | -0.416265820098261 | -0.397701582130195 |
| both_management_type \| Species, mowing and grazing \| as.factor(DRUH)Phengaris teleius #1 | estimate | -1.78856110650578 | -1.72152054057036 |
| both_management_type \| Species, mowing and grazing \| as.factor(MOW)1 #1 | estimate | 0.187593619452715 | 0.183012249905847 |
| both_management_type \| Species, mowing and grazing \| as.factor(GRAZE)1 #1 | estimate | 0.47646880125374 | 0.493360403549368 |
| both_mapped_habitat \| Species and mapped habitat code \| (Intercept) #1 | estimate | -15.9798916075923 | -16.9683627569985 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(DRUH)Phengaris teleius #1 | estimate | -1.39568322102169 | -1.38095513310673 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L10.2 #1 | estimate | 16.6772930209365 | 17.6588130889981 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L2.2 #1 | estimate | 15.6394300383837 | 16.6288081212965 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L2.3 #1 | estimate | 14.9540148962245 | 15.9420552401144 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L2.4 #1 | estimate | 16.6761317854841 | 17.6590224965563 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L3.1 #1 | estimate | 14.5771597937218 | 15.5691481385354 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L3.3A #1 | estimate | -0.419992845671641 | 1.89170068234159 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L3.3B #1 | estimate | 15.0805391460708 | 16.0697513445347 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L4 #1 | estimate | 34.5420012068597 | 35.3988178708382 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L5.1 #1 | estimate | 16.0605580889825 | 17.0448116191102 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L5.4 #1 | estimate | 0.00889421173020632 | 1.68745779040632 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)R2.1 #1 | estimate | 16.4222836525249 | 17.4064639302261 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)R2.2 #1 | estimate | 15.7841249750532 | 16.7739934299165 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)R2.3 #1 | estimate | 1.44577175836368 | -0.749435246594695 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)T1.1 #1 | estimate | 16.0127293074232 | 16.9990770700273 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)T1.2 #1 | estimate | 15.6549464745075 | 16.6394037928018 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)T1.6 #1 | estimate | 16.2366320419057 | 17.1611497706997 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)T1.7 #1 | estimate | -1.05771071619449 | -0.690185135387368 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)T1.9 #1 | estimate | 15.8300821710705 | 16.8177312168601 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)T2.3B #1 | estimate | 16.4476094418377 | 17.436638940062 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)T3.4C #1 | estimate | -0.27146154328171 | 0.499695455884021 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)T3.4D #1 | estimate | 15.6219487413137 | 16.6115192839913 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)V1F #1 | estimate | 32.8360616345722 | 35.6228696904899 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)V4A #1 | estimate | 15.977211805646 | 16.9687283655928 |
| both_recorded_habitat \| Species and recorded habitat types \| (Intercept) #1 | estimate | -0.386667834911694 | -0.379992813083736 |
| both_recorded_habitat \| Species and recorded habitat types \| as.factor(DRUH)Phengaris teleius #1 | estimate | -1.7993217665677 | -1.74654361120114 |
| both_recorded_habitat \| Species and recorded habitat types \| as.factor(TTP)1 #1 | estimate | 0.226816416637724 | 0.257095530157502 |
| both_recorded_habitat \| Species and recorded habitat types \| as.factor(ZARUST)1 #1 | estimate | 0.0442818785455404 | 0.0497834610370825 |
| both_recorded_habitat \| Species and recorded habitat types \| as.factor(PRIKOP)1 #1 | estimate | -0.1941410967231 | -0.170499429375623 |
| both_recorded_habitat \| Species and recorded habitat types \| as.factor(JINY)1 #1 | estimate | -0.281404533373362 | -0.267225879978962 |
| both_recorded_habitat \| Species and recorded habitat types \| as.factor(MOW)1 #1 | estimate | -0.0408351120872206 | -0.0629104407214839 |
| both_cooccurrence \| Co-occurrence of the two species by species \| (Intercept) #1 | estimate | -0.355461435499278 | -0.339478249958212 |
| both_cooccurrence \| Co-occurrence of the two species by species \| as.factor(SPEC_NUM)1 #1 | estimate | 21.9421879920853 | 227590.500720649 |
| both_cooccurrence \| Co-occurrence of the two species by species \| as.factor(DRUH)Phengaris teleius #1 | estimate | -1.55273687977781 | -1.5059744188338 |
| both_species \| Species only \| (Intercept) #1 | std_error | 0.216339000991335 | 0.208326192885167 |
| both_species \| Species only \| as.factor(DRUH)Phengaris teleius #1 | std_error | 0.105024892487316 | 0.0993309222105141 |
| both_management_type \| Species, mowing and grazing \| (Intercept) #1 | std_error | 0.222762173843407 | 0.214474868561219 |
| both_management_type \| Species, mowing and grazing \| as.factor(DRUH)Phengaris teleius #1 | std_error | 0.10524017878218 | 0.0995196929221277 |
| both_management_type \| Species, mowing and grazing \| as.factor(MOW)1 #1 | std_error | 0.0773837773645101 | 0.0739799610556784 |
| both_management_type \| Species, mowing and grazing \| as.factor(GRAZE)1 #1 | std_error | 0.25418994471235 | 0.245080488332254 |
| both_mapped_habitat \| Species and mapped habitat code \| (Intercept) #1 | std_error | 2642.05182443367 | 4324.59653424345 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(DRUH)Phengaris teleius #1 | std_error | 0.208128931070562 | 0.207633917474674 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L10.2 #1 | std_error | 2642.05231471377 | 4324.5968302303 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L2.2 #1 | std_error | 2642.05184897511 | 4324.59654909475 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L2.3 #1 | std_error | 2642.05210820402 | 4324.59670611155 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L2.4 #1 | std_error | 2642.05206978389 | 4324.59668235946 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L3.1 #1 | std_error | 2642.05209433944 | 4324.59669709895 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L3.3A #1 | std_error | 4498.37998015156 | 4715.04178961866 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L3.3B #1 | std_error | 2642.05211926789 | 4324.59671296793 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L4 #1 | std_error | 11053.1586796221 | 10940.0361369843 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L5.1 #1 | std_error | 2642.05196000433 | 4324.59661619322 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)L5.4 #1 | std_error | 3002.10536265174 | 4440.77275626447 |
| both_mapped_habitat \| Species and mapped habitat code \| as.factor(BIOTOP)R2.1 #1 | std_error | 2642.05198194066 | 4324.59662943823 |

_Showing the first 60 of 159 rows._

### 11_both_species_fit_statistics.csv

| row | column | before | after |
|---|---|---|---|
| both_species \| Species only \| Both species \| glmer \| ok #1 | n_obs | 4540 | 4463 |
| both_management_type \| Species, mowing and grazing \| Both species \| glmer \| ok #1 | n_obs | 4540 | 4463 |
| both_mapped_habitat \| Species and mapped habitat code \| Both species \| glmer \| ok #1 | n_obs | 684 | 682 |
| both_recorded_habitat \| Species and recorded habitat types \| Both species \| glmer \| fitted with warnings #1 | n_obs | 4514 | 4437 |
| both_species \| Species only \| Both species \| glmer \| ok #1 | AIC | 5425.52476733462 | 5348.50015120603 |
| both_management_type \| Species, mowing and grazing \| Both species \| glmer \| ok #1 | AIC | 5420.32842762412 | 5342.57652995075 |
| both_mapped_habitat \| Species and mapped habitat code \| Both species \| glmer \| ok #1 | AIC | 907.025497948758 | 904.865524889532 |
| both_recorded_habitat \| Species and recorded habitat types \| Both species \| glmer \| fitted with warnings #1 | AIC | 5382.38226996854 | 5304.26647109329 |
| both_species \| Species only \| Both species \| glmer \| ok #1 | BIC | 5451.20749649876 | 5374.11445706455 |
| both_management_type \| Species, mowing and grazing \| Both species \| glmer \| ok #1 | BIC | 5458.85252137033 | 5380.99798873852 |
| both_mapped_habitat \| Species and mapped habitat code \| Both species \| glmer \| ok #1 | BIC | 1029.28036172457 | 1027.04132565131 |
| both_recorded_habitat \| Species and recorded habitat types \| Both species \| glmer \| fitted with warnings #1 | BIC | 5440.11672058494 | 5361.8460748557 |
| both_species \| Species only \| Both species \| glmer \| ok #1 | logLik | -2708.76238366731 | -2670.25007560302 |
| both_management_type \| Species, mowing and grazing \| Both species \| glmer \| ok #1 | logLik | -2704.16421381206 | -2665.28826497537 |
| both_mapped_habitat \| Species and mapped habitat code \| Both species \| glmer \| ok #1 | logLik | -426.512748974379 | -425.432762444766 |
| both_recorded_habitat \| Species and recorded habitat types \| Both species \| glmer \| fitted with warnings #1 | logLik | -2682.19113498427 | -2643.13323554664 |
| both_species \| Species only \| Both species \| glmer \| ok #1 | deviance | 4445.5516375632 | 4672.13333595903 |
| both_management_type \| Species, mowing and grazing \| Both species \| glmer \| ok #1 | deviance | 4437.81889770484 | 4667.83625368509 |
| both_mapped_habitat \| Species and mapped habitat code \| Both species \| glmer \| ok #1 | deviance | 762.154854614808 | 766.322572976437 |
| both_recorded_habitat \| Species and recorded habitat types \| Both species \| glmer \| fitted with warnings #1 | deviance | 4402.02169100057 | 4598.05303596923 |

### 11_both_species_random_effects.csv

| row | column | before | after |
|---|---|---|---|
| both_species \| Species only \| X:Y \| (Intercept) #1 | variance | 0.59855255919984 | 0.387710738033137 |
| both_species \| Species only \| YEAR \| (Intercept) #1 | variance | 0.269118809421331 | 0.249584851501605 |
| both_management_type \| Species, mowing and grazing \| X:Y \| (Intercept) #1 | variance | 0.598860992380244 | 0.384856460910346 |
| both_management_type \| Species, mowing and grazing \| YEAR \| (Intercept) #1 | variance | 0.273508452248308 | 0.253730764560727 |
| both_mapped_habitat \| Species and mapped habitat code \| X:Y \| (Intercept) #1 | variance | 0.334927193763569 | 0.30992645538501 |
| both_mapped_habitat \| Species and mapped habitat code \| YEAR \| (Intercept) #1 | variance | 1.34501929350699e-07 | 1.94251092813791e-07 |
| both_recorded_habitat \| Species and recorded habitat types \| X:Y \| (Intercept) #1 | variance | 0.59870268356394 | 0.407041936049919 |
| both_recorded_habitat \| Species and recorded habitat types \| YEAR \| (Intercept) #1 | variance | 0.290571768173258 | 0.280983019180889 |
| both_cooccurrence \| Co-occurrence of the two species by species \| X:Y \| (Intercept) #1 | variance | 0.168942107768805 | 4.87101902945731e-08 |
| both_cooccurrence \| Co-occurrence of the two species by species \| YEAR \| (Intercept) #1 | variance | 0.223768267185202 | 0.207685800325552 |
| both_species \| Species only \| X:Y \| (Intercept) #1 | sd | 0.773661786053725 | 0.622664225753445 |
| both_species \| Species only \| YEAR \| (Intercept) #1 | sd | 0.518766623272287 | 0.49958467901008 |
| both_management_type \| Species, mowing and grazing \| X:Y \| (Intercept) #1 | sd | 0.773861093724348 | 0.620368004421848 |
| both_management_type \| Species, mowing and grazing \| YEAR \| (Intercept) #1 | sd | 0.522980355508989 | 0.503716948851959 |
| both_mapped_habitat \| Species and mapped habitat code \| X:Y \| (Intercept) #1 | sd | 0.578728946713027 | 0.556710387351458 |
| both_mapped_habitat \| Species and mapped habitat code \| YEAR \| (Intercept) #1 | sd | 0.00036674504679777 | 0.000440739257173434 |
| both_recorded_habitat \| Species and recorded habitat types \| X:Y \| (Intercept) #1 | sd | 0.773758801929865 | 0.637998382482212 |
| both_recorded_habitat \| Species and recorded habitat types \| YEAR \| (Intercept) #1 | sd | 0.539047092723129 | 0.530078314196015 |
| both_cooccurrence \| Co-occurrence of the two species by species \| X:Y \| (Intercept) #1 | sd | 0.411025677748733 | 0.000220703852015712 |
| both_cooccurrence \| Co-occurrence of the two species by species \| YEAR \| (Intercept) #1 | sd | 0.473041506831273 | 0.455725575676362 |

### 11_species_comparisons.csv

| row | column | before | after |
|---|---|---|---|
| Counted specimens, P. nausithous vs P. teleius \| Wilcoxon rank sum test with continuity correction #1 | statistic | 408845 | 401217 |
| Counted specimens by species \| Kruskal-Wallis rank sum test #1 | statistic | 0.171813895346869 | 0.163996415296518 |
| Site occupancy, P. nausithous vs P. teleius \| Wilcoxon rank sum test with continuity correction #1 | statistic | 2776654 | 2699010 |
| Counted specimens, P. nausithous vs P. teleius \| Wilcoxon rank sum test with continuity correction #1 | p_value | 0.678536042859397 | 0.685535347758655 |
| Counted specimens by species \| Kruskal-Wallis rank sum test #1 | p_value | 0.678504747354708 | 0.685503452331825 |

### 13_pca_loadings.csv

| row | column | before | after |
|---|---|---|---|
| PLANT_QUANT #1 | PC1 | 0.200327066095133 | 0.20732754322664 |
| HET_INN #1 | PC1 | 0.274349117589932 | 0.274021101971903 |
| EVL #1 | PC1 | 0.267659178337097 | 0.270741511089696 |
| MZCHU #1 | PC1 | 0.226305517997684 | 0.242741476986267 |
| LandUseChange #1 | PC1 | -0.185684272636202 | -0.103431159222714 |
| Abandonment #1 | PC1 | -0.258152151958709 | -0.254459440480633 |
| HarmfulMow #1 | PC1 | -0.245160260063045 | -0.240107667843063 |
| HarmfulGrazing #1 | PC1 | 0.263541787234415 | 0.2628296379294 |
| GrazingByeffects #1 | PC1 | 0.26590198815018 | 0.263581773636457 |
| FertilizerUse #1 | PC1 | -0.22380804653433 | -0.229585192988817 |
| Afforestation #1 | PC1 | -0.256904969679007 | -0.260675975063191 |
| Invasives #1 | PC1 | -0.195893767081271 | -0.210449591530336 |
| NativeDominants #1 | PC1 | -0.241065745549398 | -0.245662413947618 |
| AbioticNaturalProcesses #1 | PC1 | -0.23677232206522 | -0.240758059724039 |
| Encroachment #1 | PC1 | -0.252344870904739 | -0.253539600243381 |
| BiomassAccumulation #1 | PC1 | 0.267768414202845 | 0.27156895064611 |
| Eutrophization #1 | PC1 | -0.237374959782919 | -0.238227658258561 |
| PLANT_QUANT #1 | PC2 | -0.407925790198645 | -0.37330834860733 |
| HET_INN #1 | PC2 | -0.141660541587544 | -0.0342607521339254 |
| EVL #1 | PC2 | 0.200213176790724 | 0.169444587798718 |
| MZCHU #1 | PC2 | 0.356992207538641 | 0.193277580192473 |
| LandUseChange #1 | PC2 | 0.456834046352075 | 0.530947391832459 |
| Abandonment #1 | PC2 | -0.189577891355001 | -0.254060978154748 |
| HarmfulMow #1 | PC2 | 0.190845201223261 | 0.305548054561713 |
| HarmfulGrazing #1 | PC2 | -0.0521778793986611 | 0.108552520448669 |
| GrazingByeffects #1 | PC2 | 0.0124390263322856 | 0.144169227063849 |
| FertilizerUse #1 | PC2 | 0.00421796869782316 | 0.204059223201746 |
| Afforestation #1 | PC2 | 0.182004934577426 | 0.22960967356512 |
| Invasives #1 | PC2 | -0.177835152553691 | 0.169424727842059 |
| NativeDominants #1 | PC2 | -0.304687298084966 | -0.284138957716105 |
| AbioticNaturalProcesses #1 | PC2 | 0.293274593178547 | 0.120884902363268 |
| Encroachment #1 | PC2 | 0.0622840332234238 | -0.11700615614491 |
| BiomassAccumulation #1 | PC2 | 0.101843253776172 | -0.0281010693508564 |
| Eutrophization #1 | PC2 | -0.322799007798152 | -0.296859425884276 |

### 13_pca_variance.csv

| row | column | before | after |
|---|---|---|---|
| PC1 #1 | Standard deviation | 3.5180173157939 | 3.51333661187248 |
| PC2 #1 | Standard deviation | 1.62418361353159 | 1.74366096476176 |
| PC3 #1 | Standard deviation | 1.40910672246986 | 1.2712640526824 |
| PC1 #1 | Proportion of Variance | 0.72803 | 0.72609 |
| PC2 #1 | Proportion of Variance | 0.15517 | 0.17884 |
| PC3 #1 | Proportion of Variance | 0.1168 | 0.09507 |
| PC1 #1 | Cumulative Proportion | 0.72803 | 0.72609 |
| PC2 #1 | Cumulative Proportion | 0.8832 | 0.90493 |

### 13_rda_anova_by_term.csv

| row | column | before | after |
|---|---|---|---|
| DRUH #1 | Variance | 5.17699869928788 | 4.97429034631321 |
| POSITIVE #1 | Variance | 8.60506837982128 | 8.23066563247611 |
| Residual #1 | Variance | 3.21793292089084 | 3.79504402121068 |
| DRUH #1 | F | 1.60879633807118 | 1.31073324011834 |
| POSITIVE #1 | F | 2.67409812179649 | 2.16879319092863 |

### 13_rda_anova_overall.csv

| row | column | before | after |
|---|---|---|---|
| Model #1 | Variance | 13.7820670791092 | 13.2049559787893 |
| Residual #1 | Variance | 3.21793292089084 | 3.79504402121068 |
| Model #1 | F | 2.14144722993383 | 1.73976321552348 |

### 13_rda_tap_anova_by_axis.csv

| row | column | before | after |
|---|---|---|---|
| RDA1 #1 | Variance | 11.4731650542918 | 11.5722730681324 |
| RDA2 #1 | Variance | 2.30890202481734 | 1.63268291065693 |
| Residual #1 | Variance | 3.21793292089084 | 3.79504402121068 |
| RDA1 #1 | F | 3.56538353543915 | 3.04931194564659 |
| RDA2 #1 | F | 1.43502184885703 | 0.860428970800755 |
| RDA2 #1 | Pr(>F) | 0.416666666666667 | 0.666666666666667 |

### 13_rda_tap_anova_by_term.csv

| row | column | before | after |
|---|---|---|---|
| DRUH #1 | Variance | 5.17699869928788 | 4.97429034631321 |
| POSITIVE #1 | Variance | 8.60506837982128 | 8.23066563247611 |
| Residual #1 | Variance | 3.21793292089084 | 3.79504402121068 |
| DRUH #1 | F | 1.60879633807118 | 1.31073324011834 |
| POSITIVE #1 | F | 2.67409812179649 | 2.16879319092863 |

### 13_rda_tap_anova_overall.csv

| row | column | before | after |
|---|---|---|---|
| Model #1 | Variance | 13.7820670791092 | 13.2049559787893 |
| Residual #1 | Variance | 3.21793292089084 | 3.79504402121068 |
| Model #1 | F | 2.14144722993383 | 1.73976321552348 |

### 13_rda_variable_scores.csv

| row | column | before | after |
|---|---|---|---|
| PLANT_QUANT #1 | RDA1 | 0.563763219314082 | 0.56929818439257 |
| HET_INN #1 | RDA1 | 0.613121294361928 | 0.604104230243795 |
| EVL #1 | RDA1 | 0.524311644815404 | 0.537124895272448 |
| MZCHU #1 | RDA1 | 0.432408392524887 | 0.473727660143641 |
| LandUseChange #1 | RDA1 | -0.489815847976774 | -0.395660361857627 |
| Abandonment #1 | RDA1 | -0.480150792033108 | -0.471508243463599 |
| HarmfulMow #1 | RDA1 | -0.630325951717816 | -0.623729394380862 |
| HarmfulGrazing #1 | RDA1 | 0.53712453112074 | 0.532241387327709 |
| GrazingByeffects #1 | RDA1 | 0.528887221177671 | 0.523260340657672 |
| FertilizerUse #1 | RDA1 | -0.569226732753265 | -0.574951420228989 |
| Afforestation #1 | RDA1 | -0.641856939045084 | -0.643363573226062 |
| Invasives #1 | RDA1 | -0.47268410188809 | -0.524316613047634 |
| NativeDominants #1 | RDA1 | -0.428749695289664 | -0.445454225881385 |
| AbioticNaturalProcesses #1 | RDA1 | -0.545775961931753 | -0.555681041548562 |
| Encroachment #1 | RDA1 | -0.504005599742426 | -0.507990709332079 |
| BiomassAccumulation #1 | RDA1 | 0.59741456739552 | 0.606474121045573 |
| Eutrophization #1 | RDA1 | -0.41954042300514 | -0.426504203563323 |
| PLANT_QUANT #1 | RDA2 | -0.180199274176569 | -0.181008090975726 |
| HET_INN #1 | RDA2 | -0.155870110876712 | -0.171479928309389 |
| EVL #1 | RDA2 | 0.114176036072046 | 0.0804853288653269 |
| MZCHU #1 | RDA2 | 0.349168296517193 | 0.282808927780099 |
| LandUseChange #1 | RDA2 | 0.40319667190687 | 0.130484736251788 |
| Abandonment #1 | RDA2 | -0.0155326384326752 | 0.0100455826620723 |
| HarmfulMow #1 | RDA2 | -0.0489340654841986 | -0.00866342370396527 |
| HarmfulGrazing #1 | RDA2 | -0.200757426307316 | -0.199460434786924 |
| GrazingByeffects #1 | RDA2 | -0.147314236078793 | -0.16136285215109 |
| FertilizerUse #1 | RDA2 | -0.286274886310871 | -0.281649586273013 |
| Afforestation #1 | RDA2 | -0.0139778048054674 | 0.00550507444637192 |
| Invasives #1 | RDA2 | -0.442494169329018 | -0.37137924254245 |
| NativeDominants #1 | RDA2 | -0.143693860630612 | -0.0999682300834796 |
| AbioticNaturalProcesses #1 | RDA2 | 0.341283388234228 | 0.32805850674108 |
| Encroachment #1 | RDA2 | 0.247253181693384 | 0.245041707835476 |
| BiomassAccumulation #1 | RDA2 | 0.2161707175395 | 0.189589386970449 |
| Eutrophization #1 | RDA2 | -0.167530769380505 | -0.152729578241636 |


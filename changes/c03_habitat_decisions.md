# c03_habitat_decisions

Snapshot `20_habitat_all` compared with `21_decisions_all`.

230 files compared: 217 identical, 0 numerically identical (differences below 1e-9 relative), 11 changed, 2 added, 0 removed.

## Files that differ

| file | kind | status | rows_before | rows_after | rows_removed | rows_added | cols_removed | cols_added | cells_changed |
|---|---|---|---|---|---|---|---|---|---|
| data_lokal_kves.csv | processed | changed | 33839 | 75158 | 0 | 41319 | ID_LOKAL;KVES_ID;KOD;KATEGORIE;AREA_real;LENGTH_real | ID_LOKAL;KVES_VERSION;AREA_SOURCE;KVES_ID;KOD;KATEGORIE;AREA_real;LENGTH_real | 0 |
| data_lokal_vmb_current.csv | processed | changed | 21572 | 42470 | 0 | 20898 | ID_LOKAL;SEGMENT_ID;FSB;FSB_EVAL;BIOTOP_SEZ;ROK_AKT;DATUM;SHAPE_Area;NUTS3;NATURAL_PCT;B_ID;BIOTOP;STEJ_PR;HABITAT;REPRE;ZACH;KVALITA;DG;RB;AREA_real;LENGTH_real | ID_LOKAL;KVES_VERSION;AREA_SOURCE;SEGMENT_ID;FSB;FSB_EVAL;BIOTOP_SEZ;ROK_AKT;DATUM;SHAPE_Area;NUTS3;NATURAL_PCT;B_ID;BIOTOP;STEJ_PR;HABITAT;REPRE;ZACH;KVALITA;DG;RB;AREA_real;LENGTH_real | 0 |
| revised__data_analysis.csv | processed | changed | 6576 | 6576 | 0 | 0 |  | KVES_VERSION, X_SHARE, SANGUISORBA_SHARE, AREA_SOURCE | 23613 |
| 05_current_vs_frozen.csv | tables | changed | 7 | 7 | 7 | 7 |  |  | 0 |
| 05_habitat_by_fsb_current.csv | tables | changed | 10 | 10 | 0 | 0 |  |  | 20 |
| 05_habitat_intersection_summary_current.csv | tables | changed | 4 | 4 | 0 | 0 |  |  | 2 |
| 05_habitat_sources.csv | tables | changed | 3 | 3 | 1 | 1 |  |  | 1 |
| 05_kves_dominant_class.csv | tables | changed | 36 | 45 | 4 | 13 | sites | site_areas | 0 |
| revised__06_area_weighted_coverage.csv | tables | changed | 6 | 6 | 0 | 0 |  | area_from_kves, median_area_ha, median_x_share, median_sanguisorba_share, records_with_sanguisorba_meadow | 16 |
| revised__06_dominant_habitats.csv | tables | changed | 20 | 20 | 3 | 3 |  |  | 15 |
| revised__06_kves_classes.csv | tables | changed | 20 | 20 | 8 | 8 |  |  | 35 |
| 05_site_areas.csv | tables | added |  |  |  |  |  |  |  |
| revised__06_habitat_shares_by_occupancy.csv | tables | added |  |  |  |  |  |  |  |

## Changed numbers

The full list of 23702 changed cells is in `c03_habitat_decisions.csv`.

### revised__data_analysis.csv

| row | column | before | after |
|---|---|---|---|
| 48053918 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 6794.256994450119 |
| 48053917 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 3553.0105573827136 |
| 48053916 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 3090.668512705038 |
| 48053915 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 91200.75880376706 |
| 48053537 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 11488.8329037018 |
| 48053536 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 96920.38777249996 |
| 48053535 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 420.0663869110865 |
| 49920270 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 5632.017182697538 |
| 49920222 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 5632.017182697538 |
| 49920271 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 50246.6380578836 |
| 48053603 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 10011.990461949383 |
| 48053534 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 6837.269360847769 |
| 48053533 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 2193.68364017457 |
| 48053532 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 5668.440751922506 |
| 48053531 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 5466.97170396335 |
| 48053447 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 6066.7997973729525 |
| 48053445 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 7479.910845476725 |
| 48053444 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 41583.47891093854 |
| 49920182 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 6922.946539292979 |
| 49920181 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 11957.475748454332 |
| 49920180 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 13012.317677451088 |
| 49920223 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 63384.8115082436 |
| 49920221 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 31737.005786877635 |
| 49920220 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 11008.190083495245 |
| 49920219 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 8559.31657516249 |
| 49920218 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 1920.1408039266237 |
| 48054103 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 22763.798924462375 |
| 48053616 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 25809.299017415447 |
| 48053615 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 3554.615964053863 |
| 48053530 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 2880.3251826790156 |
| 48053514 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 3361.40381348084 |
| 48053513 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 1415.69065533578 |
| 48053512 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 3756.288300113815 |
| 48053511 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 1908.9172045504695 |
| 48053510 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 1422.20172238641 |
| 48053509 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 9522.02011604546 |
| 48053508 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 22574.9366238676 |
| 48053507 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 1291.7646064736696 |
| 48053443 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 13366.392701656026 |
| 52600098 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 6083.704247930061 |
| 52600097 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 7705.511038711752 |
| 52600096 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 10939.318871345904 |
| 52600094 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 683.6595607239287 |
| 52600093 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 7085.202640769779 |
| 52600092 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 10092.002832561693 |
| 52600091 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 8951.025014700841 |
| 52600090 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 8193.524123684116 |
| 52600089 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 4615.8007114333095 |
| 52600088 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 2054.7161048099447 |
| 52600087 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 7917.748233564877 |
| 52600086 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 5889.933681750093 |
| 52600085 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 3240.9865857376785 |
| 48053811 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 7277.8388730114875 |
| 48053810 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 7085.202640769779 |
| 48053809 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 10939.318871345904 |
| 48053808 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 14312.760920639113 |
| 48053807 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 3309.4451784090998 |
| 48053806 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 48081.47800095958 |
| 48053614 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 14972.16621294492 |
| 48053613 \| Phengaris nausithous \| FALSE #1 | AREA_SITE | 0 | 4656.405941756056 |

_Showing the first 60 of 23613 rows._

### 05_habitat_by_fsb_current.csv

| row | column | before | after |
|---|---|---|---|
| T #1 | overlaps | 5471 | 9205 |
| - #1 | overlaps | 4173 | 6088 |
| X #1 | overlaps | 3439 | 7835 |
| moz. #1 | overlaps | 2450 | 5150 |
| L #1 | overlaps | 1621 | 4608 |
| K #1 | overlaps | 474 | 1174 |
| M #1 | overlaps | 354 | 822 |
| V #1 | overlaps | 261 | 548 |
| R #1 | overlaps | 174 | 344 |
| S #1 | overlaps | 9 | 22 |
| T #1 | area_ha | 2752.98822294243 | 7049.17042117835 |
| - #1 | area_ha | 4468.61379438538 | 8527.47451179029 |
| X #1 | area_ha | 2468.79129744356 | 3860.25501716117 |
| moz. #1 | area_ha | 1269.37081746483 | 2763.31199871795 |
| L #1 | area_ha | 572.395821014523 | 661.578472776636 |
| K #1 | area_ha | 71.4728824050207 | 94.6986071966222 |
| M #1 | area_ha | 103.958631101882 | 149.12459660086 |
| V #1 | area_ha | 195.158062770653 | 219.833879806597 |
| R #1 | area_ha | 98.8847967414389 | 166.152235556534 |
| S #1 | area_ha | 0.181281373282953 | 0.191089637975959 |

### 05_habitat_intersection_summary_current.csv

| row | column | before | after |
|---|---|---|---|
| site x segment overlaps #1 | value | 18426 | 35796 |
| median segments per site #1 | value | 1 | 3 |

### 05_habitat_sources.csv

| row | column | before | after |
|---|---|---|---|
| current \| revised \| Data/Input/habitats/habitat_segments.gpkg (https://data.nature.cz/ds/21, retrieved 2026-10-15) #1 | overlaps | 21572 | 42470 |

### revised__06_area_weighted_coverage.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous \| line #1 | with_area | 0 | 3 |
| Phengaris nausithous \| point #1 | with_area | 0 | 1732 |
| Phengaris teleius \| line #1 | with_area | 0 | 3 |
| Phengaris teleius \| point #1 | with_area | 0 | 1391 |
| Phengaris nausithous \| line #1 | dominant_habitat_known | 1 | 3 |
| Phengaris nausithous \| point #1 | dominant_habitat_known | 1593 | 1732 |
| Phengaris teleius \| line #1 | dominant_habitat_known | 1 | 3 |
| Phengaris teleius \| point #1 | dominant_habitat_known | 1282 | 1391 |
| Phengaris nausithous \| line #1 | median_natural_share |  | 0.066537233637308 |
| Phengaris nausithous \| point #1 | median_natural_share |  | 0.0928534133169939 |
| Phengaris teleius \| line #1 | median_natural_share |  | 0.066537233637308 |
| Phengaris teleius \| point #1 | median_natural_share |  | 0.0225888496012319 |
| Phengaris nausithous \| line #1 | median_habitats | 0 | 1 |
| Phengaris nausithous \| point #1 | median_habitats | 0 | 2 |
| Phengaris teleius \| line #1 | median_habitats | 0 | 1 |
| Phengaris teleius \| point #1 | median_habitats | 0 | 1 |

### revised__06_dominant_habitats.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous \| -1 #1 | occupied_records | 584 | 593 |
| Phengaris nausithous \| T1.1 #1 | occupied_records | 307 | 315 |
| Phengaris nausithous \| T1.5 #1 | occupied_records | 221 | 233 |
| Phengaris nausithous \| X5 #1 | occupied_records | 110 | 112 |
| Phengaris nausithous \| X1 #1 | occupied_records | 52 | 57 |
| Phengaris nausithous \| T1.4 #1 | occupied_records | 46 | 48 |
| Phengaris nausithous \| T1.9 #1 | occupied_records | 42 | 46 |
| Phengaris nausithous \| T1.6 #1 | occupied_records | 39 | 43 |
| Phengaris nausithous \| X7B #1 | occupied_records | 27 | 29 |
| Phengaris teleius \| -1 #1 | occupied_records | 60 | 62 |
| Phengaris teleius \| T1.1 #1 | occupied_records | 44 | 48 |
| Phengaris teleius \| T1.5 #1 | occupied_records | 28 | 26 |
| Phengaris teleius \| T1.9 #1 | occupied_records | 16 | 17 |
| Phengaris teleius \| X5 #1 | occupied_records | 12 | 13 |
| Phengaris teleius \| T1.6 #1 | occupied_records | 4 | 5 |

### revised__06_kves_classes.csv

| row | column | before | after |
|---|---|---|---|
| Phengaris nausithous \| B311 \| Degradovaný travní porost #1 | records | 1319 | 546 |
| Phengaris nausithous \| C111 \| Aluviální a vlhké louky #1 | records | 912 | 763 |
| Phengaris nausithous \| C113 \| Mezofilní louky #1 | records | 764 | 916 |
| Phengaris nausithous \| B111 \| Orná půda #1 | records | 166 | 213 |
| Phengaris nausithous \| A411 \| Dopravní síť #1 | records | 106 | 141 |
| Phengaris nausithous \| B411 \| Rozptýlená zeleň #1 | records | 84 | 92 |
| Phengaris teleius \| B311 \| Degradovaný travní porost #1 | records | 939 | 335 |
| Phengaris teleius \| C111 \| Aluviální a vlhké louky #1 | records | 600 | 561 |
| Phengaris teleius \| C113 \| Mezofilní louky #1 | records | 533 | 583 |
| Phengaris teleius \| B111 \| Orná půda #1 | records | 132 | 174 |
| Phengaris teleius \| A411 \| Dopravní síť #1 | records | 85 | 110 |
| Phengaris teleius \| B411 \| Rozptýlená zeleň #1 | records | 69 | 75 |
| Phengaris nausithous \| B311 \| Degradovaný travní porost #1 | occupied_records | 555 | 209 |
| Phengaris nausithous \| C111 \| Aluviální a vlhké louky #1 | occupied_records | 382 | 297 |
| Phengaris nausithous \| C113 \| Mezofilní louky #1 | occupied_records | 346 | 432 |
| Phengaris nausithous \| B111 \| Orná půda #1 | occupied_records | 39 | 65 |
| Phengaris nausithous \| A411 \| Dopravní síť #1 | occupied_records | 42 | 55 |
| Phengaris nausithous \| B411 \| Rozptýlená zeleň #1 | occupied_records | 28 | 34 |
| Phengaris teleius \| B311 \| Degradovaný travní porost #1 | occupied_records | 72 | 21 |
| Phengaris teleius \| C111 \| Aluviální a vlhké louky #1 | occupied_records | 61 | 54 |
| Phengaris teleius \| C113 \| Mezofilní louky #1 | occupied_records | 51 | 62 |
| Phengaris teleius \| A411 \| Dopravní síť #1 | occupied_records | 2 | 4 |
| Phengaris teleius \| B411 \| Rozptýlená zeleň #1 | occupied_records | 0 | 2 |
| Phengaris nausithous \| B311 \| Degradovaný travní porost #1 | occupied_share | 0.421 | 0.383 |
| Phengaris nausithous \| C111 \| Aluviální a vlhké louky #1 | occupied_share | 0.419 | 0.389 |
| Phengaris nausithous \| C113 \| Mezofilní louky #1 | occupied_share | 0.453 | 0.472 |
| Phengaris nausithous \| B111 \| Orná půda #1 | occupied_share | 0.235 | 0.305 |
| Phengaris nausithous \| A411 \| Dopravní síť #1 | occupied_share | 0.396 | 0.39 |
| Phengaris nausithous \| B411 \| Rozptýlená zeleň #1 | occupied_share | 0.333 | 0.37 |
| Phengaris teleius \| B311 \| Degradovaný travní porost #1 | occupied_share | 0.077 | 0.063 |
| Phengaris teleius \| C111 \| Aluviální a vlhké louky #1 | occupied_share | 0.102 | 0.096 |
| Phengaris teleius \| C113 \| Mezofilní louky #1 | occupied_share | 0.096 | 0.106 |
| Phengaris teleius \| B111 \| Orná půda #1 | occupied_share | 0.008 | 0.006 |
| Phengaris teleius \| A411 \| Dopravní síť #1 | occupied_share | 0.024 | 0.036 |
| Phengaris teleius \| B411 \| Rozptýlená zeleň #1 | occupied_share | 0 | 0.027 |


# c03_habitat_decisions_legacy

Snapshot `20_habitat_legacy` compared with `21_decisions_legacy`.

102 files compared: 94 identical, 0 numerically identical (differences below 1e-9 relative), 7 changed, 1 added, 0 removed.

## Files that differ

| file | kind | status | rows_before | rows_after | rows_removed | rows_added | cols_removed | cols_added | cells_changed |
|---|---|---|---|---|---|---|---|---|---|
| data_lokal_kves.csv | processed | changed | 33839 | 75158 | 0 | 41319 | ID_LOKAL;KVES_ID;KOD;KATEGORIE;AREA_real;LENGTH_real | ID_LOKAL;KVES_VERSION;AREA_SOURCE;KVES_ID;KOD;KATEGORIE;AREA_real;LENGTH_real | 0 |
| data_lokal_vmb_current.csv | processed | changed | 21572 | 42470 | 0 | 20898 | ID_LOKAL;SEGMENT_ID;FSB;FSB_EVAL;BIOTOP_SEZ;ROK_AKT;DATUM;SHAPE_Area;NUTS3;NATURAL_PCT;B_ID;BIOTOP;STEJ_PR;HABITAT;REPRE;ZACH;KVALITA;DG;RB;AREA_real;LENGTH_real | ID_LOKAL;KVES_VERSION;AREA_SOURCE;SEGMENT_ID;FSB;FSB_EVAL;BIOTOP_SEZ;ROK_AKT;DATUM;SHAPE_Area;NUTS3;NATURAL_PCT;B_ID;BIOTOP;STEJ_PR;HABITAT;REPRE;ZACH;KVALITA;DG;RB;AREA_real;LENGTH_real | 0 |
| 05_current_vs_frozen.csv | tables | changed | 7 | 7 | 7 | 7 |  |  | 0 |
| 05_habitat_by_fsb_current.csv | tables | changed | 10 | 10 | 0 | 0 |  |  | 20 |
| 05_habitat_intersection_summary_current.csv | tables | changed | 4 | 4 | 0 | 0 |  |  | 2 |
| 05_habitat_sources.csv | tables | changed | 3 | 3 | 1 | 1 |  |  | 1 |
| 05_kves_dominant_class.csv | tables | changed | 36 | 45 | 4 | 13 | sites | site_areas | 0 |
| 05_site_areas.csv | tables | added |  |  |  |  |  |  |  |

## Changed numbers

The full list of 23 changed cells is in `c03_habitat_decisions_legacy.csv`.

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


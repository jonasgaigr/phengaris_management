# Step 05 - Habitat layer intersection

_Generated 2026-09-05 23:07:32_

Monitoring site geometries overlaid on the national habitat mapping layer. One row per site x habitat segment overlap, with the real area and length of each overlap.

> **Note.** The habitat mapping share //bali.nature.cz was not reachable, so the intersection was not recomputed. The previously written Data/Processed/data_lokal_vmb.csv is reused, so the habitat variables in step 06 may be out of date relative to the current site geometries.

**Table - Extent of the habitat intersection**

| measure | value |
|---|---|
| site x segment overlaps | 18816 |
| distinct sites covered | 8678 |
| median segments per site | 1 |
| largest number of segments on one site | 234 |

Full table: [`05_habitat_intersection_summary.csv`](../Tables/05_habitat_intersection_summary.csv)

**Table - Overlaps and area by habitat quality evaluation (FSB)**

| FSB | overlaps | area_ha |
|---|---|---|
| T | 5456 | 2746 |
| - | 4172 | 4472 |
| X | 3406 | 2451 |
| moz. | 2925 | 1661 |
| L | 1606 | 570.8 |
| K | 463 | 70.41 |
| M | 353 | 103.6 |
| V | 255 | 195 |
| R | 171 | 95.43 |
| S | 9 | 0.1813 |

Full table: [`05_habitat_by_fsb.csv`](../Tables/05_habitat_by_fsb.csv)


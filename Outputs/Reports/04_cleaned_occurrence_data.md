# Step 04 - Cleaned occurrence data

_Generated 2026-09-05 23:07:29_

Derivation of the analysis variables from the targeted monitoring records. Only records from the targeted monitoring campaigns are kept, only records with the host plant present, and the two years with insufficient coverage are dropped.

**Table - Records retained at each filtering stage**

| stage | records |
|---|---|
| records with imputed absences (step 02) | 5409 |
| targeted monitoring only | 5409 |
| host plant present, excluded years dropped | 4540 |

Full table: [`04_cleaning_summary.csv`](../Tables/04_cleaning_summary.csv)

**Table - Records flagged by each binary site attribute**

| variable | records_flagged | records_total | percent |
|---|---|---|---|
| EVL | 846 | 4540 | 18.6 |
| EVL_target | 372 | 4540 | 8.2 |
| MZCHU | 307 | 4540 | 6.8 |
| PROTECT | 928 | 4540 | 20.4 |
| SPEC_NUM | 105 | 4540 | 2.3 |

Full table: [`04_flag_counts.csv`](../Tables/04_flag_counts.csv)

**Table - Visits covering both species**

| description | visits |
|---|---|
| visits at which both species were monitored | 1415 |
| visits at which both species were found | 113 |

Full table: [`04_both_species_visits.csv`](../Tables/04_both_species_visits.csv)

## Corrections applied to the original code

> **Note.** MZCHU and SPEC_NUM were previously derived with `ID %in% <data frame>` rather than `ID %in% <vector of ids>`. Comparing against a data frame made both variables constant: in the previous `data_clean.csv` all 4540 records had MZCHU = 0 and SPEC_NUM = 0, and PROTECT was therefore an exact copy of EVL. Every model and figure using MZCHU, PROTECT or SPEC_NUM was fitted on a constant. Both now compare against the identifier vector.

The MZCHU flag now marks 307 of 4540 records as lying inside a small-scale specially protected area, and PROTECT is no longer identical to EVL.

> **Note.** SPEC_NUM keeps the original definition, which takes one ID_NALEZ per both-species visit, and so now flags 105 records. Defining it at the visit level instead, by matching site and date, would flag 239 records. Decide which of the two the co-occurrence model should use before the manuscript is finalised.

> **Note.** EVL_target keeps the original definition, under which a record is flagged when its site lies in a Natura 2000 site designated for either Phengaris species. It flags 372 records. Matching the designation to the species of the record instead would flag 312 records. This was left unchanged because it is an interpretation of the variable, not a coding error.


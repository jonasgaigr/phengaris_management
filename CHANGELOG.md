# Changelog

Every change that alters a number in the outputs. Two kinds are listed:

- **(b)** bug fixes, where the output changes for a documented reason;
- **(c)** analytical or ecological decisions.

Pure refactorings **(a)** are not listed, because each was checked to leave
every processed CSV and every result table unchanged. Each commit's comparison
report is in `changes/`, named after the commit's position in the series.

How the numbers were produced:

1. Before each change, `Data/Processed/**/*.csv` and `Outputs/**/Tables/*.csv`
   were snapshotted.
2. The full cascade was re-run from a clean worktree of the commit.
3. `R/tools/compare_snapshots.R` diffed the two snapshots.
4. `changes/<stem>.md` summarises each file; `changes/<stem>.csv` lists every
   changed cell, before and after.

Review item numbers refer to `REVIEW.md`.

---

## (b) Read the occurrence exports with "." as the decimal mark

- **Commit:** `3633071`
- **Review item:** 1
- **Full diff:** [`changes/b01_coordinates.md`](changes/b01_coordinates.md)

**Why.** `readr::read_csv2()` reads `.` as a grouping mark. The exports use it
as the decimal mark, so every coordinate was multiplied by 1, 10 or 100,
depending on how many decimals it had.

**What changed in the data.**

- `X` and `Y` changed in every row of `data_with_imputed.csv` (5409 rows),
  `data_clean.csv` and `data_analysis.csv` (4540 rows each).
- Example: the first X goes from −75 967 312 to −759 673.12.
- All coordinates now lie inside the S-JTSK bounding box of Czechia.
- Three free-text values that readr had dropped by guessing their column as
  logical are now kept, one each in `TAX_POZN`, `VAL_POZN` and `GAR_POZN`.

**What changed in the models.**

| model | term | before | after |
|---|---|---|---|
| `nau_spatial` | `X:Y` slope | −4.29e−17, p = 4.0e−4 | −3.66e−12, p = 1.0e−34 |
| `nau_spatial` | AIC | 4425.7 | 4280.4 |
| `nau_spatiotemporal` | `X:Y` slope | −5.31e−17, p = 2.2e−5 | −4.18e−12, p = 3.7e−41 |
| `nau_spatiotemporal` | AIC | 4279.4 | 4106.8 |
| `tel_spatial` | `X:Y` slope | −6.57e−18, p = 0.79 | 7.58e−13, p = 0.21 |
| `tel_spatial` | AIC | 1135.0 | 1133.5 |
| `tel_spatiotemporal` | `X:Y` slope | −8.69e−18, p = 0.73 | 4.14e−13, p = 0.50 |
| `tel_spatiotemporal` | AIC | 1128.3 | 1128.0 |

The `X:Y` slope remains the slope of a product of two coordinates. It is
uninterpretable either way (review item 10) and is kept only as part of the
legacy specification.

**Side effect on the mixed models.** The partition behind the `(1 | X:Y)`
random effect is unchanged, with the same 3201 groups. But lme4 orders the
group levels by value, and the order changed, so the optimiser takes a
slightly different path.

- Most `glmer` estimates move by less than 1e−3.
- The exceptions are the fits already known to be fragile (review item 13):

  | model | estimate shift | p-value shift |
  |---|---|---|
  | `both_mapped_habitat` | up to 0.32 | |
  | `nau_management_ttp` | | up to 0.41 |
  | `tel_fsb` | | up to 0.048 |
  | `both_cooccurrence` | | up to 0.43, for an estimate that moved by 1e−7 |

- `nau_management_ttp` no longer reports a degenerate Hessian.

---

## (b) Keep one row per record and species; give every row its own key

- **Commit:** `adeb720`
- **Review item:** 2
- **Full diff:** [`changes/b02_deduplicate.md`](changes/b02_deduplicate.md)

**Why.** The intersection with the 10 km reporting cells repeated a record
once per cell its site touches. In `data_analysis.csv`, 77 rows (75 polygon
sites) duplicated a record and species and inflated every model's n.

**What changed in the data.**

| table | before | after |
|---|---|---|
| `data_with_imputed.csv` | 5409 rows | 5300 rows |
| `data_analysis.csv` | 4540 rows | 4463 rows |

- `data_analysis.csv` by species after the change:
  - *P. nausithous*: 3153 records (3151 observed, 2 imputed), previously 3216;
  - *P. teleius*: 1310 records (528 observed, 782 imputed), previously 1324.
- New column `ID_RECORD` (`<ID_NALEZ>_<nau|tel>`), the unique key.
- The `range_Pnau`, `range_Ptel` and `range_both` columns, which recorded
  nothing, are gone.

**Key model changes.**

| model | term | before | after |
|---|---|---|---|
| `nau_method_timing` (n 1753 → 1711) | METHOD | 0.497 (SE 0.220) | 0.445 (0.210) |
| | TIMING | 1.162 (0.167) | 1.093 (0.158) |
| | METHOD × TIMING | −0.534 (0.258), p 0.038 | −0.479 (0.246), p 0.051 |
| `nau_evl_combined` (n 3216 → 3153) | EVL | 0.295 (0.128) | 0.301 (0.125) |
| `tel_evl` (n 1324 → 1310) | EVL | 1.53 (0.19) | 1.48 (0.17) |
| `tel_graze` | GRAZE | 21.6 (SE 14.1, separated) | 1.85 (0.53), p 0.0005 |

`tel_graze` moved from a degenerate fit, with a site variance of about 1745,
to a regular one.

**Descriptive tables.** For example, `07_occupancy_by_species`:

| | before | after |
|---|---|---|
| *P. nausithous* negative / positive | 1747 / 1469 | 1704 / 1449 |
| *P. teleius* negative / positive | 1122 / 202 | 1110 / 200 |

---

## (b) Empty the other species' observation fields on imputed absences

- **Commit:** `c1b1004`
- **Review items:** 4, 22
- **Full diff:** [`changes/b03_blank_imputed_fields.md`](changes/b03_blank_imputed_fields.md)

**Why.** Imputed absences carried the count, counting unit, behaviour and
taxon note of the other species' record they were copied from.

**What changed.** On imputed rows:

- `POCET`, `ODHAD`, `POCITANO`, `REL_POC`, `ETOLOGIE` and `TAX_POZN` are now
  NA;
- `CESKE_JMENO` names the imputed species.

**Counted specimens, *P. teleius*** (`07_abundance_by_species`):

| | before | after |
|---|---|---|
| records | 561 | 200 |
| mean | 10.6 | 17.7 |
| SD | 29.8 | 47.1 |

Of those 561 records, 361 were *P. nausithous* counts.

**`11_species_comparisons`, counts between the species:**

| test | before | after |
|---|---|---|
| Wilcoxon | p 0.686 | p 0.132 |
| Kruskal–Wallis H | 0.164 | 2.27 (p 0.132) |

The occupancy models are unaffected: they do not use these fields.

---

## (b) Flag "žádný vliv" as None and stop counting None as a threat

- **Commit:** `f731b1b`
- **Review item:** 5
- **Full diff:** [`changes/b04_none_threat.md`](changes/b04_none_threat.md)

**Why.** `None` was matched with "žádné", which occurs only in
"Přítomnost rostlin: žádné". Those records are filtered out, so `None` was 0
on every analysed record.

**What changed.**

- `None` is now 1 on 649 records of `data_analysis.csv`, previously 0.
- 12 of those also list a threat.
- `SUM_THREATS` no longer adds `None`. It was 0 everywhere before, so
  `SUM_THREATS` is unchanged.
- The legacy ordination (step 13) now has `None` among its variables (17 → 18
  variables), so its PCA and RDA tables change. No occupancy model uses
  `None`.

---

## (b) Keep unknown management, threats and habitat as NA, not 0

- **Commit:** `1523f3d`
- **Review item:** 6
- **Full diff:** [`changes/b05_unknown_is_na.md`](changes/b05_unknown_is_na.md)

**What changed in the data.**

| variable | before | after |
|---|---|---|
| `GRAZE` | 0 on all 2070 records of 2019–2020, when the form had no grazing entry | NA there |
| `GRAZE`, "Pastva: ne" (2 records) | 1 | 0 |
| `OVERALL` | 0 for the 2063 records without a mowing assessment | NA |
| threat flags, `None`, `SUM_THREATS` | 0 for the 609 records without any threat list | NA |
| `HET_INN` | 0 for the 26 records without a habitat description | NA |

From 2021 on, a missing grazing entry still counts as "not grazed". This
needs confirming (`TODO(Jonáš)` in `config.yml`).

**Key model changes.**

| model | n before → after | estimate before | estimate after |
|---|---|---|---|
| `nau_graze` | 3153 → 1702 | GRAZE 0.139 (SE 0.250) | 0.109 (0.251) |
| `tel_graze` | 1310 → 691 | GRAZE 1.85 (0.53), p 0.0005 | 1.73 (0.56), p 0.0018 |
| `both_management_type` | 4463 → 2393 | | |
| `nau_threats` | 1742 → 1720 | | |
| `nau_het_inner` / `tel_het_inner` | lose 21 / 5 records | | |

---

## (b) Count plain "příkop" as a ditch in PRIKOP

- **Commit:** `5c54b8c`
- **Review item:** 19
- **Full diff:** [`changes/b06_prikop.md`](changes/b06_prikop.md)

**Why.** `PRIKOP` matched only the form category "příkop u komunikace". The
form also has a plain "příkop".

**What changed in the data.**

- `PRIKOP` is now 1 on 1260 records, previously 554.
- `07_habitat_type_counts`, PRIKOP at occupied sites:
  - *P. nausithous*: 176 → 380;
  - *P. teleius*: 15 → 31.

**Key model changes.**

| model | term | before | after |
|---|---|---|---|
| `nau_het_inner` | HET_INN | −0.044 (SE 0.068), p 0.51 | −0.101 (0.066), p 0.13 |
| `tel_het_inner` | HET_INN | 0.038 (0.135), p 0.78 | −0.272 (0.148), p 0.066 |
| `both_recorded_habitat` | PRIKOP | −0.171 | −0.234 (SE 0.091), p 0.010 |

The previous `both_recorded_habitat` fit was degenerate: every SE was 0.0007.

---

## (b) Refit every AIC comparison set on one common data frame

- **Commit:** `ac8b90f`
- **Review item:** 9
- **Full diff:** [`changes/b07_common_data_aic.md`](changes/b07_common_data_aic.md)

Every `_aic_` table now has one n, with each model's own n in `n_own_data`.

**`09_nausithous_aic_grassland`.**

| model | n before | n after | ΔAIC after |
|---|---|---|---|
| `nau_ttp` | 3132 | 1710 | 68.7 |
| `nau_management_ttp` | 1710 | 1710 | 0 (best) |

ΔAIC was 1964 before, a sample-size artefact.

**`10_teleius_aic_habitat`.** The set used 699 and 1310 records; it is now
refitted on 699. `tel_resource_density`, which failed on log(0), now fits and
is best (ΔAIC 6.0 to the next).

**`10_teleius_aic_management`.** The set used 689 / 697 / 744 records; it is
now refitted on 689.

| model | ΔAIC before | ΔAIC after |
|---|---|---|
| `tel_timing` | 78.5 | 0 (best) |
| `tel_method_timing` | 0 | 3.1 |

**`11_both_species_aic`.** The set used 682 to 4463 records. Only 123 records
are complete on every variable of the set: grazing is known from 2021 only and
`BIOTOP` for 682 records. On them `both_cooccurrence` and `both_species` are
within 1.8 AIC of each other. This set is not a meaningful comparison
(`REVIEW.md`, item 9).

---

## (b) Diagnose every model fit and seed every permutation test

- **Commits:** `e77da9a`, refined in `17921b1`
- **Review items:** 13, 17
- **Full diff:** [`changes/b08_diagnostics_seeds.md`](changes/b08_diagnostics_seeds.md)

**What was added.** Three new tables, `09_nausithous_diagnostics`,
`10_teleius_diagnostics` and `11_both_species_diagnostics`. Each flags, per
model:

- singular fits;
- possible separation, from the largest non-intercept estimate or SE;
- inflated random-effect variances;
- fitting warnings;

and gives the DHARMa uniformity and dispersion p-values. No existing number
changed. Seeding the permutation tests left their p-values as they were,
because the four-row ordination enumerates every permutation.

**What the flags show** (legacy specification):

| step | flagged | singular | possible separation | other |
|---|---|---|---|---|
| 09 *P. nausithous* | 5 of 35 | | 4 | 1 with fitting warnings only |
| 10 *P. teleius* | 12 of 38 | 7 | 3 | `tel_resource_density` failed |
| 11 both species | 3 of 5 | | 2 | |

Fits worth a closer look:

- `tel_plant` and `tel_plant_poly` (site variance about 1734);
- `tel_mapping_field`;
- `both_mapped_habitat`;
- `both_cooccurrence`, whose SPEC_NUM coefficient is about 2 × 10⁵ (see
  `REVIEW.md`, item 26).

The `poly()` models can trip the separation threshold because orthogonal
polynomial coefficients are large; for them the flag means "check".

---

## (c) Visit-level imputation rule; option to keep out-of-range negatives

- **Commit:** `f0b4846`
- **Review item:** 4
- **Decisions:** Q1 and Q2 of the review

**What was added.** Two new options:

- `imputation_rule`: `legacy_cell` (the original), `visit` or `none`.
- `keep_outside_range`.

Under the visit rule a species is imputed absent at a visit (site geometry ×
date) where only the other species was recorded, and only inside its own
reporting range.

**No number changes yet.** The legacy specification keeps `legacy_cell` and
drops the out-of-range records. The new table `02_imputation_rules_compared`
shows what each rule gives:

| rule | *P. teleius* absences imputed | *P. nausithous* absences imputed |
|---|---|---|
| legacy_cell | 838 | 2 |
| visit | 1944 | 396 |
| none | 0 | 0 |

`02_outside_range` lists the explicit negatives outside their species' range:
85 *P. nausithous* and 280 *P. teleius*.

---

## (c) match_sites(): persistent sites across years

- **Commit:** `00c6ad5`
- **Review item:** 3

Groundwork for the revised random-effect structure; not used by any
specification yet. No number changes.

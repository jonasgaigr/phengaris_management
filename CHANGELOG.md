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

---

## (c) Phase 3: the revised specification

- **Decisions:** P3.1-P3.9 and Q1-Q5 of `REVIEW.md`, approved on 2026-10-09
- **Legacy specification:** unchanged; the comparison of its outputs before and
  after is in [`changes/c01_phase3_legacy.md`](changes/c01_phase3_legacy.md)

A second specification, **revised**, runs beside **legacy**. Every number it
produces is new: its tables are in `Outputs/revised/Tables`, its report in
`Outputs/REPORT.md`. New steps: 16 (common), 17-20 (revised only).

### Imputation and the reporting range (P3.2, Q1, Q2)

- Absences are imputed visit by visit (site geometry × date). They are
  imputed only inside the imputed species' own Article 17 reporting range.
  Step 02 now checks this on the site geometries and stops if it fails.
- The explicit negatives outside the range are kept: 85 *P. nausithous* and
  280 *P. teleius* records.

| | *P. nausithous* | *P. teleius* |
|---|---|---|
| range cells | 766 | 443 |
| cells with observed records | 489 | 159 |
| cells with imputed absences | 116 | 271 |
| absences imputed (step 02) | 396 | 1944 |

- Every model is fitted twice: with the imputed absences, and on the observed
  records only.
- **Finding.** In 2019 almost no *P. teleius* negative was entered
  explicitly: 5 observed negatives against 517 imputed ones and 40
  positives. On the observed records alone the year therefore separates the
  outcome. The *P. teleius* models of the observed sample are degenerate:
  the random-effect SDs of grid, year and observer are 3-5 on the logit
  scale, and the diagnostics flag them. For *P. teleius* the sample with
  the imputed absences is the one to interpret.

### Unit of analysis and survey design (P3.1, P3.3)

- **One record per species × site × visit.** 12 records were merged; a
  positive record was kept before a negative one.
- **Random intercepts:** mapping grid square, year, observer.
  - The observer comes from AUTOR, normalised: 82 spellings become 57
    observers.
  - A persistent site (`SITE_ID`, 50 m; 3324 sites, 4.8 % recurring) was
    added as a sensitivity check at 0, 50 and 200 m. It gains nothing: its
    variance is about 0 and ΔAIC is +1.5 to +2.
- **Flight season.** `DOY_DEV` = day of year − the median day of the year's
  positive records of the species, from every source; it enters as a
  quadratic.
  - The medians are 202-211 for *P. nausithous* and 197-206 for *P. teleius*.
  - 6 visits lie more than 45 days from the median. They are kept in the
    data, flagged, and left out of the models: the three of October and
    December 2024, and three September ones.
- **Field-form era.** `PROTOCOL_ERA` (2019-2020 against 2021-2024) is in every
  model that spans both eras.
- **Host plant.** The revised specification drops only the records with the
  host plant recorded absent: 276 *P. nausithous* records with 4 positives,
  66 *P. teleius* records with none. It keeps the records without host-plant
  information.

### Management (P3.4)

One multivariable model per species: mowing timing × method, host-plant
abundance, and an EVL designated for the record's own species
(`EVL_species`). It is fitted on the records with mowing recorded. Odds
ratios with 95 % CIs, from the sample with the imputed absences:

| term | *P. nausithous* (n 1924) | *P. teleius* (n 1348) |
|---|---|---|
| mowing timed appropriately | 2.16 (1.48-3.15) | 1.25 (0.55-2.85) |
| patches left uncut | 2.18 (1.34-3.56) | 1.51 (0.44-5.20) |
| timing × patches | 0.45 (0.25-0.80) | 1.52 (0.38-6.02) |
| host plant abundant (vs single) | 2.93 (2.23-3.86) | 1.70 (0.95-3.05) |
| host plant dominant | 2.98 (1.85-4.80) | 2.56 (1.04-6.26) |
| EVL designated for the species | 1.29 (0.73-2.30) | 6.10 (2.12-17.59) |

- *P. nausithous*: either appropriate timing or uncut patches raises the
  predicted probability of recording the species from 0.28 to about 0.46;
  both together add nothing more. The observed-only fit agrees.
- The 2021-2024 polygon model adds grazing and site area. Grazing has no
  clear effect for either species. Larger polygons hold *P. nausithous* less
  often (OR 0.41 per tenfold area, 0.30-0.57).
- Diagnostics: all 91 revised models converged; no fixed effect separates
  (detectseparation); the largest adjusted GVIF is 1.11. The *P. teleius*
  observed-only models are flagged (see above), and their bias-reduced
  refits are reported.

### Connectivity (P3.5)

S = Σ exp(−α d) A^0.5, over the sites occupied by the species in the same or
the previous year, as log10(S + 0.001). Added to the management model:

- *P. nausithous*: OR 1.97 (1.70-2.27) per tenfold S, ΔAIC −78. The best α
  is 1 per km.
- *P. teleius*: OR 2.71 (2.14-3.42), ΔAIC −60. The best α is 0.5 per km
  (ΔAIC −6 against 1 per km).
- The management estimates barely move.

### Spatial autocorrelation (P3.6)

With the grid random effect, none of the 8 models tested (baseline and
management, both species, both samples) has residual Moran's I at p < 0.05.
The smooth alternative is therefore not fitted. When it was tried on the
*P. nausithous* management model, its odds ratios matched the grid model's
within 0.4.

### Co-occurrence (item 26) and the species compared (item 22)

- On 586 visits with an observed record of both species, finding one species
  goes with finding the other:
  - *P. nausithous* given *P. teleius*: OR 4.28 (1.72-10.61);
  - *P. teleius* given *P. nausithous*: OR 3.32 (1.31-8.42).
- *P. teleius* is recorded far less often than *P. nausithous*:
  - observed records: 24 % against 45 %, OR 0.29 (0.22-0.37);
  - with the imputed absences: 8 % against 41 %.

### Threats (P3.7)

One model per threat class and species. The 2019-2020 threat list is mapped
to the current classes by a harmonised vocabulary, matched item by item
(proposed; `TODO(Jonáš)` in `config.yml`).

- For *P. nausithous*, fertiliser use (OR 0.35, 0.16-0.74) and harmful grazing
  (0.67) go with fewer records. Biomass accumulation, abandonment and native
  dominants go with more.
- Only biomass accumulation survives the Holm correction, on the observed
  records (OR 1.65, p_holm 0.014).
- Six *P. teleius* classes are too rare to model.

### Abundance (P3.8)

A zero-truncated negative binomial of the count at positive records:

- *P. nausithous* (794 records with mowing recorded): host plant abundant
  RR 2.40, dominant 3.43; mowing timing RR 1.37 (0.97-1.94).
- *P. teleius* (112): host plant dominant RR 3.52.

### Habitat (P3.9)

- The revised specification describes each site by the area-weighted rule.
  It runs on the frozen intersection and is not validated on real data.
- No revised model uses it.
- A point lying in a mosaic of several habitats now gets no dominant habitat,
  instead of the alphabetically first one.

### Fixes found on the way

None of these touches a legacy number.

- **`.regex_escape()` escaped nothing.** In R's default regex engine a
  backslash inside brackets is literal. Harmless for the keys of the field
  form, which have no special characters. It would have left every threat
  item with parentheses unmatched by the harmonised vocabulary. Now
  `perl = TRUE`.
- **Lost validation note.** readr read `VAL_POZN` as logical whenever its
  first 1000 rows were empty, and lost the note "zákres neodpovídá popisu"
  in the revised table. The intermediates are now read with every row used to
  guess the column types.
- **`match_sites()` is faster.** It took 4 minutes per distance (pairwise
  distances) and now takes seconds (spatial index plus exact distances), with
  identical sites.

---

## (c) Habitat from the current VMB layer and KVES 2025

- **Commit:** `0b24725`
- **Decision:** P3.9 (habitat), now run on real data. Two values are
  PROPOSED: the mosaic share column `STEJ_PR` and the 200 m margin kept
  around the sites.
- **Legacy specification:** unchanged; the comparison of its outputs before
  and after is in
  [`changes/c02_habitat_layers_legacy.md`](changes/c02_habitat_layers_legacy.md)
- **Full diff:** [`changes/c02_habitat_layers.md`](changes/c02_habitat_layers.md)

**Why.** The revised habitat description (P3.9) ran on the frozen
intersection of the original analysis. That intersection describes a segment
only by its habitats that have a Natura 2000 record:

- `BIOTOP` is filled in exactly the 7245 of 18 816 rows that have a `HABITAT`
  code, and in no other.
- Natural habitats outside Natura 2000 never appear: T1.5 (0 rows, 2104 in
  the current layer), T1.4 (0, 445). Neither do the X habitats or the
  unmapped parts ("-1").
- A segment with several Natura records is repeated once per record, so the
  area-weighted rule counted its overlap with a site more than once.

**What was done.**

- `R/tools/download_habitat_layers.R` was rewritten, following the
  alcon-spatial project. It logs in to data.nature.cz with the ISOP (NDOP)
  account and downloads the VMB (ds/21) and KVES 2025 (ds/145) archives of
  the kraje within 200 m of a site: all 14, 0.6 GB VMB and 4.5 GB KVES,
  cached in `Data/Cache/aopk/`.
  - It keeps the features within 200 m of a site, cut to that area, and puts
    the 480 segments split by kraj lines back together.
  - It writes `Data/Input/habitats/habitat_segments.gpkg` (55 814 segments,
    64 699 segment × habitat rows, the habitat input contract) and
    `kves.gpkg` (232 107 polygons). `RETRIEVED.yml` records the archives
    and their md5.
  - The old tool referred to `PATHS$temp`, which no longer exists, so it
    stopped before downloading anything; it fetched only VMB.
- Step 05 still copies the frozen intersection. It also intersects the sites
  with the current VMB layer (`data_lokal_vmb_current.csv`) and with KVES
  (`data_lokal_kves.csv`), and checks the current intersection against the
  frozen one.
- `habitat_source`, a new setting of each specification: legacy uses the
  frozen intersection, revised the current layers. Revised falls back to the
  frozen intersection, with a warning, when the VMB copy is missing.
- `habitat: share_column: STEJ_PR`. In the current layer `STEJ_PR` adds up to
  100 in every one of the 55 814 segments. It weights only the current
  layer; the frozen intersection has no such share.
- The revised analysis table gets the site's dominant KVES class, `KVES_KOD`
  and `KVES_KATEGORIE`.

**The current intersection against the frozen one** (`05_current_vs_frozen`).

| measure | value |
|---|---|
| frozen sites in the current intersection | 8678 of 8678 |
| frozen site × segment overlaps found again | 18 308 of 18 387 (99.6 %) |
| of these, segments not updated since | 17 914 (97.8 %) |
| unchanged segments: same FSB, same `BIOTOP_SEZ` | 100 %, 100 % |
| unchanged segments: overlap area within 1 % | 99.9 % |
| unchanged segments: same `FSB_EVAL` | 97.8 % |

`FSB_EVAL` differs by design in 389 overlaps. The new value is X for every
mapped segment less than half natural. The network share's table joins kept
`moz.` for mosaics of X habitats only (327 overlaps), and counted a natural
habitat with a Natura record twice (62). No model uses `FSB_EVAL`.

**What changed in the revised data** (`data_analysis.csv`, 6576 records).

| | before (frozen) | after (current) |
|---|---|---|
| records with a habitat code (`BIOTOP`) | 1896 | 6324 |
| records in a natural dominant habitat (`NATURAL` = 1) | 1896 | 2805 |
| records with a dominant habitat (`NATURAL` known) | 6538 | 6324 |
| median natural share of polygon sites (`NATURAL_SHARE`) | 0.019 | 0.410 |
| polygon records with `AREA_SITE` within 1 % of the site area | 95.7 % | 99.9 % |
| polygon records with `AREA_SITE` above the site area (> 1 %) | 4.3 % | 0 % |
| records with a Natura 2000 code (`HABITAT`) | 1896 | 0 |
| records with a KVES class | – | 6572 |

- 214 fewer records have a dominant habitat. These are points and lines
  lying in a mosaic segment, which now lists all its habitats, not only the
  Natura ones. A point site has no area to weigh them by (buffer widths are
  still `TODO(Jonáš)`).
- `HABITAT` is empty because the published VMB has no Natura 2000 code. No
  revised model uses it.
- Dominant habitats at occupied *P. nausithous* sites (`06_dominant_habitats`):

  | | before | after |
  |---|---|---|
  | none (NA) | 1117 | 62 |
  | unmapped ("-1") | 0 | 584 |
  | T1.1 | 332 | 307 |
  | T1.5 | 0 | 221 |
  | X5 | 0 | 110 |

- The commonest KVES classes at the sites (`05_kves_dominant_class`) are
  B311 degraded grassland (2624 sites), C111 alluvial and wet meadows (2146)
  and C113 mesophile meadows (1852).

**What changed in the models.** Nothing: no revised model uses the habitat
variables, and every table of steps 17-20 is identical.

---

## (c) Point sites over their KVES polygon; X and *Sanguisorba* shares

- **Decisions** (2026-10-15, `REVIEW.md`, "Status after the habitat
  download"):
  - `STEJ_PR` as the mosaic share column, and the 200 m margin, are decided;
  - a crosswalk to the Natura 2000 codes will be added later;
  - X habitats may be valuable for the species too;
  - a point site takes the KVES polygon it lies in, from the KVES version of
    the record's year, instead of a buffer;
  - the *Sanguisorba* meadows are T1.1, T1.4, T1.5, T1.7 and T1.10.
- **PROPOSED:** the KVES version of each record year.
- **Legacy specification:** unchanged; the comparison is in
  [`changes/c03_habitat_decisions_legacy.md`](changes/c03_habitat_decisions_legacy.md).
  Only the common outputs of step 05 change, and legacy reads none of them.
- **Full diff:** [`changes/c03_habitat_decisions.md`](changes/c03_habitat_decisions.md)

**KVES versions.** The download tool now also fetches KVES 2021, 2022 and
2023 (ds/101, 102, 108), all 14 kraje, and writes one layer per version:
259 978, 215 900, 197 227 and 232 107 (2025) polygons near the sites.

- A record takes its KVES information from the closest version
  (`habitat: kves_version_of_year`): 2019-2021 from 2021 (4201 records),
  2022 from 2022 (1171), 2023 from 2023 (559), and 2024 from 2025 (645). 2024
  is as close to 2023 as to 2025; the later version includes that year's
  habitat mapping.
- The versions before 2025 publish only the category name. Their `KOD` is the
  2025 code of the category of the same name. 13 of their 38 categories (16 in
  2021) have none, so 1720 records have a `KVES_KATEGORIE` without a
  `KVES_KOD`. These are the forests (split in 2025), the shrubs, the urban
  classes and, in 2021, "Hospodářské louky" (managed meadows). That class is
  the second commonest for *P. nausithous* records (786), since the records
  of 2019-2021 take KVES 2021.
- The intersections are keyed by `ID_LOKAL` and `KVES_VERSION`.

**Point and line sites.** Each is described over the KVES polygon it lies in
or crosses, from its version, cut to 200 m around it (`site_areas()`, written
to `Data/Processed/common/site_areas.gpkg`). Every one of them lies in a KVES
polygon. The median area is 1.1-1.5 ha by version for a point, 3.8-7.5 ha
for a line (`05_site_areas`). `point_buffer_m` and `line_buffer_m` are
removed from `config.yml`.

**New variables** (revised, area-weighted rule): `X_SHARE`, the share of the
site in X habitats, kept apart from the unmapped area; `SANGUISORBA_SHARE`
beside `SANGUISORBA_AREA`; `AREA_SOURCE` and `KVES_VERSION`.

**What changed in the revised data** (`data_analysis.csv`, 6576 records).

| | before | after |
|---|---|---|
| records with a site area (`AREA_SITE` > 0) | 3447 | 6576 |
| records with a dominant habitat (`NATURAL` known) | 6324 | 6576 |
| records in a natural dominant habitat (`NATURAL` = 1) | 2805 | 2985 |
| median natural share, all records with an area | 0.410 | 0.288 |
| records with a *Sanguisorba* meadow (`SANGUISORBA_AREA` > 0) | NA | 3272 |
| records with some X habitat (`X_SHARE` > 0) | – | 2866 (median share 0.15) |

- The median natural share falls because the points now count, and their
  KVES polygon is less natural than a polygon site: a median of 0.09
  (*P. nausithous*) and 0.02 (*P. teleius*), against 0.41 for polygon sites.
- At occupied against unoccupied sites (`06_habitat_shares_by_occupancy`),
  descriptive only:

  | | natural share | records with a *Sanguisorba* meadow |
  |---|---|---|
  | *P. nausithous* | 0.39 against 0.30 | 52 % against 50 % |
  | *P. teleius* | 0.80 against 0.16 | 63 % against 47 % |

- The polygon sites are unchanged: `05_current_vs_frozen`, now on the 3068
  polygon sites, agrees as before (FSB 100 %, overlap area 99.9 % for the
  unchanged segments).

**What changed in the models.** Nothing: every table of steps 17-20 is
identical.

---

## (c) P4.1: the flight season from a flight curve

- **Decision:** P4.1 of `REVIEW.md` (Phase 4, the literature review). The
  5 % cut-off and the 28-day sensitivity window are PROPOSED.
- **Legacy specification:** unchanged (no legacy file in the diff).
- **Full diff:** [`changes/c04_flight_curve.md`](changes/c04_flight_curve.md)

**Why.** Phase 3 kept the visits within 45 days of the year's median
positive record. The review found that about twice the documented flight
period of 6-8 weeks, and proposed a flight curve, as the butterfly
monitoring schemes estimate it, with the season cut where it falls below
5 % of its peak, and the log relative activity of the curve in place of the
quadratic season term.

**What was done.**

- An elevation model of Czechia, `Data/Input/reference/elevation.tif`
  (RCzechia `vyskopis("actual")`, 98 m, whole metres, 6 MB). The reference
  tool can now refresh single layers (`refresh_reference_layers(layers =
  "elevation")`), which left every other copy as it was. Step 16 gives each
  site its elevation (`ELEVATION` in `site_design.csv`): 159-923 m for the
  points, 160-1019 m for the polygons, median about 410 m.
- Step 16 fits a flight curve per species (`R/functions/flight_curve.R`): a
  negative-binomial GAM of the count at the targeted visits with an exact
  date within 45 days of the median (0 at a negative visit), s(DOY_DEV, k =
  5) with random intercepts for the year and the grid square, and a smooth of
  the elevation. The interaction ti(DOY_DEV, ELEVATION) enters when it
  lowers the AIC: for *P. nausithous* it does (ΔAIC 5.3), for *P. teleius*
  not (ΔAIC 1.6 against). The relative activity is the expected count
  divided by the peak within the range of the visits, capped at 1 outside
  it. New `flight_curve.csv`, tables `16_flight_curve_models`,
  `16_flight_curve_visits`, `16_flight_season`, figure `16_flight_curve`.
- Step 04 gives every record `FLIGHT_ACTIVITY` and `LOG_ACTIVITY`, and
  `IN_FLIGHT_WINDOW` is now "activity at least 5 % of the peak, and within
  45 days" (`design: flight_window: cutoff: flight_curve`). `IN_WINDOW_28`
  and `IN_WINDOW_45` flag the fixed windows.
- Step 17 compares the season terms (`17_<species>_aic_season`) and refits
  the management model on the fixed windows (`17_<species>_window_sensitivity`).

**The flight season the curve gives** (`16_flight_season`, DOY_DEV):

| | peak | first day ≥ 5 % | last day ≥ 5 % | visits |
|---|---|---|---|---|
| *P. nausithous*, 225 m | +1 | −36 | +45 | −25 to +43 |
| *P. nausithous*, 425 m | −6 | before −45 | +37 | |
| *P. nausithous*, 625 m | −4 | before −45 | +43 | |
| *P. teleius* | −17 (first visit) | before −45 | +31 | −17 to +40 |

The pooled curve of the monitoring visits is wider than the 6-8 weeks of a
single site: the sites and years differ in phenology even after centring on
the year's median. The visits barely reach the early season: 30 *P.
nausithous* and 12 *P. teleius* visits lie more than 10 days before the
median, and the *P. teleius* counts already decline from the first visit.

**What changed in the data.** 12 more *P. teleius* records are outside the
season, all negatives (9 imputed, 3 observed), dated 33-43 days after the
median: 14 instead of 2 (`04_outside_flight_window`). *P. nausithous* is
unchanged (4 records, all more than 45 days from the median). No positive
record is outside.

**Season term.** The quadratic in DOY_DEV fits better than the log relative
activity for *P. nausithous* (baseline model, ΔAIC 6.1 on 3842 records); for
*P. teleius* the activity is marginally better (ΔAIC 1.8, one parameter
fewer). The quadratic is kept (`revised_models: season_term: quadratic`);
the management estimates hardly differ between the two.

**What changed in the models.** Only the *P. teleius* models and those
using its records, through the 12 records:

| model | term | before | after |
|---|---|---|---|
| `tel_management` | EVL_species | 6.10 (2.12-17.59) | 6.12 (2.13-17.60) |
| | host plant dominant | 2.56 (1.04-6.26) | 2.62 (1.07-6.43) |
| `tel_cooccurrence` | other species found | 3.32 (1.31-8.42) | 3.32 (1.31-8.41) |
| `nau_cooccurrence` | other species found | 4.28 (1.72-10.61) | 4.27 (1.72-10.60) |
| species proportions | *P. teleius* share, with imputed | 0.0781 | 0.0784 |

**Window sensitivity** (`17_<species>_window_sensitivity`). The management
odds ratios on the season of the curve, and within 28 and 45 days of the
median, differ by at most 0.05 for *P. nausithous* (e.g. timing 2.17, 2.15,
2.17) and 0.06 for *P. teleius*. The 28-day window drops 10 *P. nausithous*
and 13 *P. teleius* management records.

---

## (c) P4.2: alpha and b of the connectivity by profile likelihood

- **Decision:** P4.2 of `REVIEW.md`. The choice by profile likelihood, the
  grids and the fixed pair alpha = 1, b = 1 are PROPOSED.
- **Legacy specification:** unchanged.
- **Full diff:** [`changes/c05_connectivity_profile.md`](changes/c05_connectivity_profile.md)

**Why.** Phase 3 fixed alpha = 1 per km (mean dispersal 1 km) and b = 0.5,
with alpha = 0.5 and 2 as a sensitivity check. The review found alpha = 1
4-10 times flatter than the mark-recapture estimates of open Czech
landscapes (mean dispersal 80-270 m, alpha 4-12; Nowicki & Vrabec 2011), but
defensible at the scale of colonisation (forested landscapes 0.5-1.5 km),
and proposed to estimate alpha and b from the occupancy data by profile
likelihood, with b raised towards 1 (immigration area exponent 0.5-2.5).

**What was done.**

- Step 16 computes the connectivity at every pair of alpha in {0.25, 0.5,
  1, 2, 3, 5, 8, 12} per km and b in {0, 0.25, 0.5, 1, 1.5}
  (`CONN_A<alpha>_B<b>`, 40 pairs), with sources up to 25 km (15 km before,
  too short for alpha = 0.25). `CONN` is the fixed pair, now alpha = 1,
  b = 1. The review's grid was alpha 0.5-12, b 0.5-1.5; *P. teleius* had its
  best pair at its corner (alpha 0.5, b 0.5), so alpha 0.25 and b 0 and 0.25
  were added to see whether the profile turns.
- Step 17 refits the management model with the connectivity at each pair,
  per species and sample (`17_<species>_connectivity_profile`, table and
  figure), and takes the pair with the lowest AIC on the sample with the
  imputed absences as `LOG_CONN` for every sample (`connectivity: choice:
  profile`). The choice is written to `connectivity_selected.csv` for the
  later steps. The sensitivity models at alpha = 0.5 and 2 are gone: the
  profile replaces them.

**What the profile shows** (sample with the imputed absences):

| | best alpha | mean distance | best b | next best | Phase 3 pair (alpha 1, b 0.5) | fixed pair (alpha 1, b 1) |
|---|---|---|---|---|---|---|
| *P. nausithous* | 1 per km | 1 km | 0 | alpha 1, b 0.25: +2.7 AIC | +6.2 | +16.5 |
| *P. teleius* | 0.5 per km | 2 km | 0 | alpha 0.5, b 0.25: +4.3 | +16.7 | +24.2 |

- The kernel the occupancy pattern supports is flat: a mean distance of 1-2
  km, against 80-270 m in the mark-recapture studies. Kernels of 200-330 m
  (alpha 3-5) fit 12-22 AIC worse for *P. nausithous*. Occupancy follows the
  occupied sites over several kilometres, the scale of colonisation and of
  regional clustering rather than of daily movement.
- The area of the occupied sites does not help: b = 0, the number of
  occupied sites weighted by distance alone, fits best for both species,
  against the b of 0.5-2.5 of the mark-recapture immigration. The polygon
  areas measure the surveyed area rather than the habitat, and the point
  sites count with a median area.
- alpha is inside the grid for both species (*P. teleius*: alpha 0.25 is 4.4
  AIC worse, alpha 1 is 11.2 worse); b sits at its natural bound, 0.

**What changed in the models.**

| model | term | before | after |
|---|---|---|---|
| `nau_management_conn` | connectivity (per tenfold) | 1.97 (1.70-2.27) | 2.01 (1.74-2.32) |
| | ΔAIC against the management model | −78.4 | −84.7 |
| | EVL_species | 1.08 (0.64-1.83) | 1.10 (0.65-1.85) |
| `tel_management_conn` | connectivity | 2.72 (2.15-3.44) | 4.13 (2.97-5.75) |
| | ΔAIC against the management model | −60.8 | −77.4 |
| | EVL_species | 3.31 (1.42-7.75) | 2.38 (1.08-5.27) |
| `tel_management_conn_obs` | EVL_species | 12.23 (1.44-104.19) | 5.14 (0.71-37.12) |

The management estimates move by at most 0.2. For *P. teleius* the EVL
association weakens further once connectivity is measured at the scale the
data prefer: the EVLs lie where the populations cluster.

**Isolated sites.** With sources up to 25 km, fewer records have no occupied
site within reach (`04_design_flags`): 19 *P. nausithous* records instead of
118, 400 *P. teleius* instead of 912.

---

## (c) P4.3: plausible imputed absences, and the 2012-2018 records

- **Decision:** P4.3 of `REVIEW.md`. The 3 km radius is PROPOSED.
- **Legacy specification:** unchanged.
- **Full diff:** [`changes/c06_plausible_imputation.md`](changes/c06_plausible_imputation.md)

**Why.** A record of the other species shows that someone surveyed, not that
the imputed species is absent (Phillips et al. 2009). The review proposed to
impute an absence only where the imputed species could have been found: in
its flight season, and near a known population of it.

**What was done.**

- Step 01 reads the 2012-2018 exports again, with their sites
  (`records_2012_2018.csv`, 4436 records; `sites_2012_2018.gpkg`). No model
  is fitted on them; they say where each species was known.
- Step 16 gives every surveyed site of 2019-2024 the distance to the nearest
  positive record of each species, from any source in 2012-2024, and whether
  a site with a positive record of 2012-2018 lies within 50 m
  (`site_history.csv`, `16_site_history`).
- Step 04 flags an imputed absence as plausible within 3 km of such a
  record (`IMPUTED_PLAUSIBLE`; the longest recorded *P. teleius* move is
  2.94 km). The flight season of the imputed species is already required of
  every record in the models (P4.1).
- Every revised model is fitted on a third sample, "with plausible imputed"
  (model ids ending `_pls`), beside "with imputed" and "observed only".

**The imputed absences** (`04_imputed_plausible`, in the flight season):

| | imputed | plausible | median distance to a positive record |
|---|---|---|---|
| *P. nausithous* | 310 | 216 (70 %) | 0.5 km |
| *P. teleius* | 1821 | 673 (37 %) | 5 km |

2357 of the 4206 surveyed sites lie within 1 km of a positive *P.
nausithous* record, but only 738 within 1 km of a *P. teleius* one; 431 and
190 sites had the species in 2012-2018.

**What changed.** No number of the existing samples: the diff adds rows and
tables only. In the new sample:

| | with imputed | with plausible imputed | observed only |
|---|---|---|---|
| *P. teleius* records (management model) | 1339 | 745 | 398 |
| `tel_management`: EVL_species | 6.12 (2.13-17.60) | 3.75 (1.44-9.78) | 880 (1.3-594 082) |
| `tel_management`: host plant dominant | 2.62 (1.07-6.43) | 3.06 (1.23-7.58) | 14.5 (0.8-262) |
| `tel_management_conn`: EVL_species | 2.38 (1.08-5.27) | 1.96 (0.93-4.14) | 5.14 (0.71-37.12) |
| `nau_management`: timing | 2.16 (1.48-3.15) | 2.17 (1.50-3.16) | 2.40 (1.59-3.62) |
| share of positive *P. teleius* records | 7.8 % | 13.6 % | 23.8 % |

- For *P. teleius* the imputed absences far from any known population
  strengthen the association with the EVLs. Without them it falls from 6.1
  to 3.8, and with connectivity it is no longer clear (1.96, 0.93-4.14). The
  management estimates barely move.
- *P. nausithous* is unchanged within 0.1.
- In the plausible sample the random-effect variance of the observer or the
  year is at zero in most *P. teleius* models (flagged in the diagnostics).

---

## (c) P4.4: detection sensitivity of the occupancy models

- **Decision:** P4.4 of `REVIEW.md`. The fixed detection probabilities, the
  prior on p and the MCMC settings are PROPOSED.
- **Legacy specification:** unchanged. No existing number of the revised
  specification changes: the new step 23 adds tables only.
- **Full diff:** [`changes/c07_detection.md`](changes/c07_detection.md)

**Why.** The revised models estimate the probability of recording the
species, occupancy times detection. A reviewer will ask how much of the
management estimates is detection. The review advised against a
single-visit occupancy model as the headline, since its detection is
identified only by the model's form (Knape & Korner-Nievergelt 2015), and
proposed two sensitivity analyses instead.

**What was done** (new step 23, `R/functions/detection.R`).

- **Known detection.** The management model refitted with p fixed at 0.9,
  0.7 and 0.5: P(record) = p × occupancy. lme4 accepts the custom link but
  its numerical Hessian breaks down, so the refit is a GLM (the year as a
  fixed factor), its likelihood maximised directly, beside the same GLM at
  p = 1.
- **Mixed design.** A Bayesian occupancy model (spOccupancy `PGOcc`) of the
  persistent site × year: management in the occupancy part, with grid-square
  and year random intercepts; era and the season quadratic in the detection
  part, with an observer random intercept. The site-years visited more than
  once identify the detection. The prior on p at the median day is
  logit-normal around 0.7 (variance 1), and the model is refitted with a
  vague prior. Fitted on the three samples. A fit whose largest Rhat exceeds
  1.1 is rerun with chains four times longer; the three *P. teleius* fits
  with imputed absences needed it, and all fits end with Rhat ≤ 1.07.

**The data that identify the detection** (`23_<species>_occupancy_design`):
*P. nausithous* has 1629 site-years, 224 visited more than once, 71 of them
with the species found at some visits and not at others; *P. teleius* has
1100, 175 and 46 (with the imputed absences), but only 23 and 4 on the
observed records alone.

**Detection probability at the median day** (posterior mean, 95 % CrI):

| | older form | newer form |
|---|---|---|
| *P. nausithous*, with imputed | 0.78 (0.69-0.87) | 0.69 (0.59-0.78) |
| *P. nausithous*, vague prior | 0.79 (0.70-0.87) | 0.69 (0.58-0.79) |
| *P. teleius*, with imputed | 0.30 (0.16-0.51) | 0.24 (0.13-0.38) |
| *P. teleius*, plausible imputed | 0.33 (0.19-0.52) | 0.30 (0.18-0.44) |
| *P. teleius*, observed only | 0.82 (0.49-0.97) | 0.79 (0.16-0.99) |

- For *P. nausithous* the repeated visits identify p: the vague prior gives
  the same posterior.
- For *P. teleius* the model with the imputed absences puts p at 0.3: an
  imputed absence at a site where the species was found on another visit is,
  to the model, a missed detection. On the observed records alone, p is
  0.8, as for *P. nausithous*. The imputed absences of *P. teleius* behave
  partly as non-detections, not as absences.

**Occupancy odds ratios** (occupancy part against the naive GLMM, with
imputed):

| term | *P. nausithous* naive | occupancy | *P. teleius* naive | occupancy |
|---|---|---|---|---|
| timing appropriate | 2.16 (1.48-3.15) | 6.61 (2.75-18.12) | 1.24 (0.54-2.83) | 2.05 (0.29-14.78) |
| patches left uncut | 2.18 (1.34-3.56) | 3.92 (1.39-11.79) | 1.50 (0.43-5.19) | 1.15 (0.14-11.46) |
| host plant abundant | 2.93 (2.23-3.86) | 16.89 (6.28-56.12) | 1.73 (0.96-3.11) | 3.20 (0.53-23.69) |
| EVL for the species | 1.29 (0.73-2.30) | 1.03 (0.32-3.33) | 6.12 (2.13-17.60) | 6.50 (0.41-83.21) |
| timing × patches | 0.45 (0.25-0.80) | 0.27 (0.07-1.00) | 1.50 (0.38-5.96) | 4.70 (0.45-47.08) |

- For *P. nausithous* every management effect keeps its direction and grows
  on the occupancy scale, as it must when detection dilutes the naive
  estimate. The intervals widen.
- For *P. teleius* the occupancy model is too weakly identified to say more
  than the naive one; every interval spans 1.

**Known detection** (`23_<species>_known_detection`). For *P. nausithous*
the odds ratios grow as p falls: the timing OR is 1.75 in the GLM at p = 1,
1.83 at 0.9 and 2.22 at 0.7. p = 0.5 cannot be fitted: in some classes of
records more than half the visits found the species. For *P. teleius*
(with imputed) every p from 1 to 0.5 can be fitted, and the estimates move
little (EVL 4.2 to 5.2).

---

## (c) P4.5-P4.7: mowing-timing check, habitat proxies, landscape at several scales

- **Decisions:** P4.5, P4.6 and P4.7 of `REVIEW.md`. The wet-meadow codes,
  the 25 m edge zone, the land-cover groups, the radii and the 2 AIC
  threshold are PROPOSED.
- **Legacy specification:** unchanged. No existing number of the revised
  specification changes: the new columns and the new steps 21 and 22 add to
  it.
- **Full diff:** [`changes/c08_habitat_landscape.md`](changes/c08_habitat_landscape.md)

**What was done.**

- **Landscape rasters** (`R/tools/build_landscape_layers.R`, 28 minutes from
  the cache). KVES 2025 and VMB are rasterised for all of Czechia: the
  land-cover group of every 25 m cell (grassland B311 and C11x; arable land
  B111; forest D1xx; water and wetlands E, F; settlements A), the per cent of
  each cell's VMB segment in natural habitats and in *Sanguisorba* meadows,
  and the same aggregated to 100 m. KVES covers 99.9996 % of the cells inside
  the border; the 500 m grassland share from the 25 m and the 100 m rasters
  correlate at 0.9993. Not tracked (48 MB); `BUILT.yml` is.
- **Step 21** gives each of the 4206 surveyed sites the shares of the five
  groups and of the natural and *Sanguisorba* habitats in circles of 0.2,
  0.5, 1, 2, 3 and 4 km (`landscape.csv`; 42 columns). Step 04 joins them.
- **Step 05** gives each polygon site its share within 25 m of its edge
  (`site_area_shape.csv`, `EDGE_SHARE`), and the area-weighted rule gives each
  site its share of wet meadows (`WET_SHARE`: T1.4, T1.5, T1.6, T1.7, T1.9,
  T1.10).
- **Step 22**: the mowing-timing check, the host plant as linear or
  unordered, the local habitat added to the management model and to the
  2021-2024 polygon model, the radius of each landscape variable chosen by AIC
  (sample with the imputed absences), and a combined model with the
  landscape variables that improve the management model by more than 2 AIC.

**Mowing timing** (`22_timing_check`). The form records no cut dates, so the
AOPK rule (no cut 1 July - 1 September) cannot be applied. The 9 observed
*P. nausithous* records whose note says the site was freshly mown at a visit
inside that window all have their timing recorded as inappropriate; 2 more
have no timing recorded. The surveyors' judgement agrees with the only
objective evidence there is.

**Host plant** (`22_<species>_host_plant_aic`). For *P. nausithous* the
unordered factor fits better than a linear term (ΔAIC 9.8): abundant and
dominant host plant go with the same odds (quadratic contrast 0.65,
0.51-0.83). For *P. teleius* a linear term is enough (ΔAIC −2.0, OR per
class 1.5-2).

**Local habitat** (`22_<species>_local_estimates`, per 10 percentage
points):

- *P. nausithous*: the *Sanguisorba* share is weakly positive (OR 1.03,
  1.00-1.07; 1.06, 1.00-1.11 on the 2021-2024 polygons, p 0.03); the wet,
  X and edge shares show nothing (OR 0.99-1.02). ΔAIC −1.9.
- *P. teleius*: none of them (OR 0.99-1.05).
- The edge share varies little: half the polygon records lie wholly within
  25 m of their edge (median 1, lower quartile 0.78). The monitored polygons
  are small (median 0.6-0.9 ha) and often strips along ditches and roads, so
  the proxy of the review, made for large uniformly mown meadows, does not
  fit them. 35 % of the records have some wet meadow.

**Landscape scales** (`22_<species>_landscape_best`; odds ratio per 10
percentage points; ΔAIC against the management model):

| variable | *P. nausithous* radius | OR | ΔAIC | *P. teleius* radius | OR | ΔAIC |
|---|---|---|---|---|---|---|
| arable land | 4 km | 0.81 | −21.2 | 4 km | 0.61 | −21.7 |
| grassland | 3 km | 1.18 | −3.6 | 1 km | 1.50 | −17.0 |
| forest | 4 km | 1.18 | −12.2 | 4 km | 1.24 | −3.1 |
| settlements | 1 km | 1.26 | −8.8 | 4 km | 0.27 | −9.5 |
| water and wetlands | 4 km | 0.31 | −8.6 | 4 km | 4.56 | −8.1 |
| natural habitats (VMB) | 4 km | 1.12 | −1.0 | 4 km | 1.52 | −9.4 |
| *Sanguisorba* meadows (VMB) | 0.2 km | 1.07 | −1.5 | 3 km | 3.26 | −10.8 |

- Arable land around the site is the strongest landscape variable for both
  species, at the largest radius, and grassland within 1 km for *P.
  teleius*, close to the 3-4 km and 0.3-0.4 km of Kajzer-Bonk et al. (2016)
  for grassland and arable cover.
- Most radii chosen are the largest, 4 km: the scale of effect may lie
  beyond it, or the variable may stand for the region.
- In the combined models the landscape variables share their effect
  (adjusted GVIF up to 3.3, flagged) and the management estimates hold:
  *P. nausithous* timing 2.19 (1.50-3.21), patches left uncut 2.10
  (1.29-3.43); *P. teleius* EVL_species 4.31 (1.57-11.84), with water
  within 4 km 5.99 (1.77-20.29).

---

## (c) P4.8-P4.9: the Natura 2000 association after matching, and persistence

- **Decisions:** P4.8 and P4.9 of `REVIEW.md`. The matching covariates are
  PROPOSED.
- **Legacy specification:** unchanged. No existing number changes: the new
  step 24 adds tables only.
- **Full diff:** [`changes/c09_natura2000.md`](changes/c09_natura2000.md)

**Why.** The EVLs designated for a species were chosen where its populations
and habitat were known, so a comparison inside and outside them mixes
protection with selection. Across six butterfly monitoring schemes, the
positive relationships with Natura 2000 mostly vanished once land cover was
accounted for (Pellissier et al. 2020).

**What was done** (new step 24, `R/functions/natura2000.R`).

- **Matching.** The records inside an EVL designated for their species are
  matched to those outside on the site's natural and *Sanguisorba* shares,
  its elevation, the grassland within 3 km, the connectivity (at the alpha
  and b of step 17) and the position, exactly within the field-form era:
  full matching on the propensity score (MatchIt, optmatch). Then the
  occupancy model with the matching weights (GLMM), and a quasi-binomial GLM
  with the year as a fixed factor and a CI robust to clustering within the
  matching subclasses, as MatchIt recommends; both with the covariates
  again. Shown beside the unmatched and the covariate-adjusted GLMM. The
  covariates are standardised, which leaves the matching and the estimate
  unchanged but keeps the GLMMs well conditioned.
- **Persistence.** The visits of 2019-2024 to sites where the species was
  found in 2012-2018 (within 50 m): the EVL association there, and on all
  visits whether it differs between sites with and without a known earlier
  population. A dynamic occupancy model (P4.9) would need sites revisited
  across years; 4.8 % are, so this is the version the data support.

**Balance.** Matching removes the differences: for *P. nausithous* every
standardised mean difference falls below 0.1 (connectivity 0.52 → 0.09,
natural share 0.40 → −0.02). For *P. teleius* the differences were larger
(connectivity 2.64, elevation 0.82, position −0.89 and −0.66) and three stay
above 0.1 after matching (position 0.14 and −0.18, grassland −0.11); the
effective number of control records is 90.

**The EVL association** (odds ratio, 95 % CI, sample with the imputed
absences; `24_<species>_designation`):

| | *P. nausithous* | *P. teleius* |
|---|---|---|
| GLMM, unmatched | 1.40 (0.88-2.22) | 4.86 (2.03-11.64) |
| GLMM, covariate-adjusted | 1.09 (0.71-1.67) | 1.82 (1.00-3.32) |
| GLMM, matching weights | 1.10 (0.67-1.78) | 4.42 (1.90-10.27) |
| GLM, cluster-robust | 1.16 (0.86-1.56) | 1.47 (0.78-2.80) |

- *P. nausithous* has no association with its EVLs once the sites are
  comparable.
- For *P. teleius* the association falls from 4.9 to 1.5-1.8 with the
  covariates, in line with selection of good sites for designation. The
  weighted GLMM disagrees (4.4): its random intercepts absorb part of what
  the matching balances, and the imperfect balance in position leaves room
  for it. The two estimators that adjust for the covariates directly agree.
  In the plausible sample: 3.68 unmatched, 1.61 adjusted, 1.47 matched GLM.

**Persistence** (`24_<species>_persistence_counts`, `_estimates`):

| | *P. nausithous* | *P. teleius* |
|---|---|---|
| visits to sites known in 2012-2018 | 414 (83 inside an EVL) | 169 (57 inside) |
| species found again, outside / inside | 68 % / 61 % | 25 % / 40 % |
| odds ratio inside against outside | 1.42 (0.65-3.11) | 2.85 (0.98-8.31) |
| known earlier population, all visits | 3.04 (2.22-4.15) | 3.12 (1.67-5.83) |
| EVL × known earlier population | 1.13 (0.49-2.63) | 0.61 (0.19-1.97) |

A population known in 2012-2018 makes a record in 2019-2024 three times as
likely. Whether the site lies in an EVL changes that little for *P.
nausithous*; for *P. teleius* persistence inside the EVLs is higher, but the
interval reaches 1, and the interaction shows no difference from the sites
without a known population. Kajzer-Bonk & Nowicki (2022) found the declines
mainly outside Natura 2000; these data are too few to confirm or refute it.

---

## (c) P4.10: counts standardised by the flight curve, and a hurdle model

- **Decision:** P4.10 of `REVIEW.md`.
- **Legacy specification:** unchanged. No existing number changes: step 20
  adds two models per species.
- **Full diff:** [`changes/c10_counts.md`](changes/c10_counts.md)

**Why.** A single count depends on where in the season the visit fell.
The butterfly monitoring schemes standardise counts by the flight curve
(Dennis et al. 2016), and the review proposed to model the zeros of the
negative visits and the counts of the positive ones together.

**What was done.** `fit_tmb()` takes a zero part (`ziformula`), and the
coefficient tables list it with the prefix `zi:`. Step 20 adds, per species
(observed records with the management recorded; `hurdle_specs()`):

- `abundance_offset`: the zero-truncated negative binomial of the count at
  the positive visits, with the log relative activity of the flight curve
  (P4.1) as its offset in place of the season terms, i.e. the count the
  visit would have given at the peak of the season;
- `hurdle`: every visit, 0 at a negative one; the zero part (the chance of a
  negative record) has the management terms, the era and the season
  quadratic; the count part is the truncated count with the offset.

**What the models show** (`20_<species>_hurdle_estimates`):

| | *P. nausithous* | *P. teleius* |
|---|---|---|
| count part: host plant abundant | 2.44 (1.92-3.09) | 2.61 (1.22-5.60) |
| count part: host plant dominant | 3.48 (2.38-5.10) | 3.54 (1.38-9.10) |
| count part: timing appropriate | 1.35 (0.95-1.91) | 2.14 (0.76-6.00) |
| zero part: timing appropriate | 0.47 (0.35-0.63) | 0.84 (0.37-1.92) |
| zero part: host plant abundant | 0.34 (0.27-0.42) | 0.26 (0.15-0.47) |
| zero part: EVL for the species | 0.53 (0.36-0.79) | 0.19 (0.08-0.48) |

- The count part is the same as the offset model: the two parts of a hurdle
  separate. With the offset the rate ratios match those of the model with
  the season quadratic within 0.05 (host plant dominant 3.43 → 3.48 for *P.
  nausithous*): the counts already analysed were not distorted by the
  timing of the visits.
- The zero part restates the occupancy models on the observed records (an
  odds ratio of a negative record below 1 is an odds ratio of a record
  above 1): appropriate timing and abundant host plant make a record more
  likely, and for *P. nausithous* the EVL association appears here (0.53),
  as in the observed-only occupancy model, without the grid-square random
  effect, which the zero part does not have.
- Where the species is found, more host plant means more adults; mowing
  goes with whether the species is found rather than with how many adults
  are counted. The DHARMa tests of the hurdle models pass (uniformity p 0.74
  and 0.91).

---

## (c) P4.11: threats with their Article 17 codes, a stacked model, Holm and BH

- **Decision:** P4.11 of `REVIEW.md`. The Article 17 codes and the list of
  confirmatory threats are PROPOSED.
- **Legacy specification:** unchanged. No odds ratio or p-value of the
  per-threat models changes; their adjusted p-values do.
- **Full diff:** [`changes/c11_threats.md`](changes/c11_threats.md)

**What was done.**

- Each threat class carries its Article 17 pressure code
  (`strukt_pozn: threat_article17`: A02, A06, A08, A09, A14, A19/A20, B01,
  I01/I02, I04, L01-L04).
- The per-threat p-values are adjusted within each species and sample in
  two families: Holm for the threats with a direction expected from the
  management literature (harmful mowing, abandonment, harmful grazing,
  fertiliser use, encroachment), Benjamini-Hochberg for the others. Before,
  Holm across all classes (`p_holm` → `p_adjusted`, `adjustment`).
- A stacked model of all modelled classes per species and sample
  (`19_threat_stacked`, `R/functions/threats.R`): every record × class is a
  row, the threat being recorded the response, each class with its own
  intercept, its own term for the species being found and its own era
  term, and random intercepts for the visit, the grid square and the
  observer. Its class × occupancy odds ratios are adjusted the same way.

**Per-threat models, adjusted p** (*P. nausithous*; the odds ratios are
unchanged):

| sample | threat | odds ratio | before (Holm, all) | after |
|---|---|---|---|---|
| with imputed | fertiliser use (A19, A20) | 0.35 (0.16-0.74) | 0.077 | 0.030 (Holm) |
| with imputed | native dominants (I04) | 1.57 (1.11-2.21) | 0.126 | 0.077 (BH) |
| plausible imputed | native dominants | 1.62 (1.15-2.29) | 0.073 | 0.048 (BH) |
| observed only | biomass accumulation (L03) | 1.65 (1.22-2.24) | 0.014 | 0.009 (BH) |
| observed only | fertiliser use | 0.34 (0.16-0.75) | 0.085 | 0.036 (Holm) |

**Stacked model** (odds ratio of the threat being recorded where the species
was found against where it was not; sample with the imputed absences):

- *P. nausithous*: fertiliser use 0.32 (0.18-0.59, Holm 0.001), harmful
  grazing 1.40 (1.09-1.79, Holm 0.03), native dominants 1.38 (BH 0.047),
  abiotic processes 0.69 (BH 0.047), eutrophication 1.28 (BH 0.047).
- *P. teleius*: harmful grazing 3.20 (2.03-5.04, Holm < 0.001), effects of
  livestock farming 3.76 (BH 0.02), encroachment 0.59 (Holm 0.04),
  eutrophication 0.44 (BH 0.01).

Fertiliser use is the consistent result: the species is recorded less often
where it is noted, and it is noted less often where the species was found.
Harmful grazing turns round between the two (per-threat 0.67, stacked
1.40): in the stacked model the visit intercept absorbs how many threats a
surveyor lists at a visit, more at the visits without the species, so its
class × occupancy terms compare the threats with each other. The stacked
models are flagged for possible separation (some classes are rare in one
field-form era), and the *P. nausithous* observed-only one did not converge;
read them as a check on the per-threat models.

---

## (c) P4.12: an SPDE spatial field and spatial+

- **Decision:** P4.12 of `REVIEW.md`. The mesh cutoff and the spatial+
  settings are PROPOSED.
- **Legacy specification:** unchanged. No existing number changes: step 18
  adds three tables.
- **Full diff:** [`changes/c12_spatial.md`](changes/c12_spatial.md)

**Why.** Step 18 found no residual spatial autocorrelation with the grid
random effect (P3.6). A reviewer may still ask whether a continuous spatial
field fits better than squares of the mapping grid, and whether the
management estimates are confounded with space: a spatial random effect can
pull a fixed effect when the covariate varies smoothly in space (Hodges &
Reich 2010).

**What was done** (step 18, `R/functions/spatial.R`), on the management
models of every species and sample:

- **SPDE.** The grid random effect replaced by a Gaussian random field over
  the sites (sdmTMB; a triangulation with vertices at least 2 km apart),
  the year and observer intercepts kept. Compared by marginal AIC (both
  Laplace approximations on the same records) and by the estimates
  (`18_spde`, `18_grid_vs_spde`).
- **spatial+** (Dupont et al. 2022). Each column of the management and
  protection terms replaced by its residual from a thin-plate smooth of the
  coordinates (k = 60), and the model refitted; an estimate moving by more
  than one standard error is flagged (`18_spatial_plus`).

**SPDE against the grid** (sample with the imputed absences):

| | ΔAIC (SPDE − grid) | range | timing | patches uncut | EVL for the species |
|---|---|---|---|---|---|
| *P. nausithous*, grid | | | 2.16 (1.48-3.15) | 2.18 (1.34-3.56) | 1.29 (0.73-2.30) |
| *P. nausithous*, SPDE | −33.8 | 34 km | 2.25 (1.53-3.29) | 2.34 (1.42-3.85) | 1.48 (0.79-2.76) |
| *P. teleius*, grid | | | 1.24 (0.54-2.83) | 1.50 (0.43-5.19) | 6.12 (2.13-17.60) |
| *P. teleius*, SPDE | −13.3 | 48 km | 1.39 (0.61-3.17) | 2.05 (0.60-7.02) | 3.56 (1.21-10.42) |

- The spatial field fits better in every sample (ΔAIC −31 to −34 for *P.
  nausithous*, −3 to −13 for *P. teleius*), with a range of 34-55 km: a
  regional structure that the 10 × 10 km squares of the mapping grid cut
  up. The residuals of the grid models were not autocorrelated either way
  (Moran's p 0.05-0.94).
- The management estimates hold. The *P. teleius* EVL association falls
  from 6.1 to 3.6 once space is continuous. sdmTMB's sanity check flags the
  *P. nausithous* model with the imputed absences (the others pass).

**spatial+.** No estimate moves by more than one standard error. The
largest shifts are the *P. teleius* EVL association (6.12 → 3.73, −0.92 SE;
plausible sample 3.75 → 2.83, −0.57 SE) and the host-plant terms of *P.
nausithous* (2.93 → 2.74, −0.48 SE). The management estimates are not
confounded with space; the *P. teleius* EVL association partly is, as the
matching (P4.8) and the plausible sample (P4.3) also found.

---

## Phase 4: how its numbers were produced, and the legacy check

- Each Phase 4 commit re-ran the steps its change affects in the working
  tree (not a clean worktree; the run of each commit is listed in its
  entry), snapshotted the outputs and diffed them against the snapshot of
  the commit before.
- After the last commit the whole cascade was run from scratch, in 93
  minutes: every processed CSV and result table is identical to the
  step-by-step results (`changes/a09_restyle_phase4.md`, 346 files).
- The legacy specification is unchanged across Phase 4: its 81 processed
  files and tables are identical to those before P4.1
  ([`changes/c13_phase4_legacy.md`](changes/c13_phase4_legacy.md)).

---

## (b) Threat classes off the 2019-2020 threat list are unknown there, not absent

- **Specification:** revised only (the harmonised threat vocabulary); legacy
  unchanged.
- **Full diff:** [`changes/b09_threats_off_form.md`](changes/b09_threats_off_form.md)

**Why.** The stacked threat model of P4.11 was flagged for separation, with
era terms of about ±25 and standard errors in the thousands. The cause: three
classes of the current threat list have no item on the 2019-2020 list. In
2019-2020 they were recorded at 0, 0 and 2 of 1893 records:

| class | records in 2019-2020 | in 2021-2024 (*P. nausithous*) |
|---|---|---|
| abandonment (A06) | 0 | 384 |
| effects of livestock farming (A14) | 0 | 49 |
| native dominants (I04) | 2 | 337 |

Their 0 in 2019-2020 means "not on the form", not "not seen": the problem
of review item 6, here in the harmonised vocabulary. The comment in
`config.yml` that "no class exists in one era only" was wrong.

**What changed.**

- Step 04 sets a class to NA in an era where it was recorded at fewer than 5
  records (`strukt_pozn: threat_min_records_per_era`;
  `mask_unlisted_threats()`, table `04_threat_classes_off_form`). 5679 cells
  of `data_analysis.csv` change from 0 (or 1, the two native-dominant
  records) to NA.
- The per-threat models of these classes are fitted on 2021-2024 alone,
  without the era term.
- The stacked model gives an era term only to the classes on both forms, and
  has no rows for a class in an era it was not on.

**What changed in the models.**

- The stacked models no longer separate, and the *P. nausithous*
  observed-only one now converges. Their class × occupancy odds ratios hardly
  move (*P. nausithous*: fertiliser use 0.32, harmful grazing 1.40, native
  dominants 1.38 → 1.39).
- Per-threat models, *P. nausithous*, with imputed: abandonment 1.55
  (1.07-2.24) → 1.48 (1.01-2.18); native dominants 1.57 → 1.55; effects of
  livestock farming 0.83 → 0.81. The *P. teleius* livestock-farming model (5
  positive records with the threat) now returns no standard error (0.78,
  NaN), with its random-effect variance at zero.

---

## (c) Follow-ups of P4.8 and P4.12: the matched GLMM without the grid square; SPDE convergence

- **Legacy specification:** unchanged. No existing number changes.
- **Full diff:** [`changes/c14_spde_matching.md`](changes/c14_spde_matching.md)

**The matched GLMM of *P. teleius*** (P4.8). After matching, the weighted
GLMM gave 4.42 against 1.47 for the weighted GLM. Refitting it piece by piece
shows why:

| outcome model on the matched records | *P. teleius*, with imputed |
|---|---|
| weighted, grid square + year + observer | 4.42 (1.90-10.27) |
| weighted, year + observer | 2.47 (1.24-4.92) |
| weighted, no random intercepts (year as factor) | 1.47 (0.94-2.30) |
| unweighted, grid square + year + observer | 1.82 (1.00-3.32) |

The grid-square intercept, under the matching weights, turns the comparison
into one within the 19 squares that hold a designated site: inside the EVL
against outside it in the same 10 km square. That is a different, local
estimand, not the one the matching targets. Step 24 now also reports the
weighted GLMM without the grid square (`24_<species>_designation`): 2.47
(with imputed), 1.09 (0.54-2.22, plausible sample), 1.97 (observed only)
for *P. teleius*; for *P. nausithous* 0.69-0.81.

**SPDE convergence** (P4.12). sdmTMB's `sanity()` had flagged the
*P. nausithous* model with the imputed absences, and in other runs other
models, through nlminb's convergence message, which can read "false
convergence" at a flat optimum. `18_spde` now reports the criteria
themselves, in place of `sanity_ok`: the largest gradient of the fixed
effects, whether the Hessian is positive definite, and nlminb's message.
Five of the six fits have gradients below 1e-3 and a positive-definite
Hessian. The *P. nausithous* model with the imputed absences stops with a
gradient of 0.0018 at AIC 2084.28. Started differently it reaches a
stationary point at 2084.71 with the same estimates (timing 0.805 against
0.810 on the logit scale): two nearby optima, between which the year
variance and the spatial field trade off.

---

## (c) Landscape radii extended to 6 and 8 km

- **Legacy specification:** unchanged. No existing number changes.
- **Full diff:** [`changes/c15_landscape_radii.md`](changes/c15_landscape_radii.md)

In P4.7, four of the seven landscape variables of *P. nausithous* and five
of *P. teleius* fitted best at 4 km, the largest radius, so the scale of
effect could lie further out. `landscape: radii_m` now adds 6 and 8 km. A
circle of 8 km around a site near the border lies partly outside Czechia;
its shares count only the part inside. The shares at 8 km correlate with
those at 4 km at 0.77 (water) to 0.94 (arable).

**Scale profiles** (`22_<species>_landscape_scales`, with imputed; ΔAIC of
the management model with the one variable added):

| variable | best radius before | ΔAIC there | best radius now | ΔAIC there | odds ratio per SD |
|---|---|---|---|---|---|
| *P. nausithous* arable | 4 km | −21.2 | 8 km | −23.6 | 0.81 → 0.77 |
| *P. nausithous* forest | 4 km | −12.2 | 8 km | −26.5 | 1.18 → 1.36 |
| *P. nausithous* water | 4 km | −8.6 | 8 km | −10.4 | 0.31 → 0.15 |
| *P. nausithous* natural | 4 km | −1.0 | 8 km | −7.0 | 1.12 → 1.28 |
| *P. nausithous* grassland | 3 km | −3.6 | 8 km | −4.2 | 1.18 → 1.26 |
| *P. teleius* arable | 4 km | −21.7 | 6 km | −25.1 | 0.61 → 0.56 |
| *P. teleius* forest | 4 km | −3.1 | 8 km | −9.0 | 1.24 → 1.50 |

The other variables keep their radius: urban at 1 km (*P. nausithous*) and
4 km (*P. teleius*), *Sanguisorba* at 200 m and 3 km, and for *P. teleius*
grassland at 1 km and natural and water at 4 km. The *P. teleius* profiles
of arable, urban, natural and water land now have their optimum inside the
range, at 4-6 km. For *P. nausithous* only urban land does: the arable
profile flattens past 4 km, and forest still improves at 8 km. A scale of
effect at 8 km and beyond is wider than the species move. It points to
regional gradients, forested uplands against arable lowlands, more than to
the landscape a population draws on.

**Combined models** (`22_<species>_habitat_landscape_estimates`):

- *P. nausithous*, with imputed:
  - forest 1.17 (0.94-1.45) at 4 km → 1.49 (1.14-1.96) at 8 km;
  - water 0.38 → 0.26 (0.08-0.81);
  - arable 0.95 → 1.12 (0.82-1.52), still absorbed by the other land covers;
  - natural land at 8 km now enters (0.99).
  - The management terms do not move (timing 2.19 → 2.18, method 2.10 →
    2.15), and EVL designation goes 1.46 (0.82-2.60) → 1.68 (0.93-3.02).
- *P. teleius*, with plausible imputed:
  - arable 0.82 → 0.79 (0.53-1.18) at 6 km;
  - forest 0.98 → 0.97 at 8 km;
  - water 6.00 → 6.71;
  - EVL designation 2.96 → 2.86 (1.12-7.26).

---

## (c) Step 25: the robustness of the key estimates

- **Legacy specification:** unchanged. No existing number changes; step 25
  only adds tables.
- **Full diff:** [`changes/c16_synthesis.md`](changes/c16_synthesis.md)

Steps 17-24 estimate the same few associations in many ways. Step 25
(`R/25_synthesis.R`, `R/functions/synthesis.R`) reads their result tables
and refits nothing. It puts the odds ratios of the management terms, the
host plant and the EVL designation side by side, per sample:

- the management model;
- with connectivity;
- on the 2021-2024 polygons;
- on the 28- and 45-day windows;
- with the habitat and the landscape;
- with the SPDE field and under spatial+;
- with the detection fixed at 0.7, and as the occupancy part of the
  mixed-design model;
- for the EVL, after covariate adjustment and after matching.

It writes `25_<species>_robustness_summary`, `25_<species>_robustness`, a
forest plot per species and `25_robustness_all`.

A fit whose upper CI limit is 10 000 or more times its lower one is a
separated term, not an estimate (`synthesis: max_interval_ratio`). The
summary counts it in `not_estimable` and leaves it out of the ranges, and
the figure leaves it out. That affects only *P. teleius* observed only: 3 of
its 9 fits per term, and 7 of its 11 for the EVL.

**What it shows.**

- *P. nausithous*, all three samples:
  - Appropriate timing, uncut patches and abundant or dominant host plant
    are above 1 in all 10 analyses.
  - Their intervals exclude 1 in all 10, except uncut patches with the
    detection fixed at 0.7: 1.66 (0.96-2.88).
  - The timing × patches interaction is below 1 in all 10 (0.27-0.58).
  - The EVL ranges from 0.97 to 1.99 (with imputed). Its interval excludes 1
    only with the detection fixed at 0.7, and after matching it is 1.16
    (0.86-1.56).
- *P. teleius*, plausible sample:
  - Every estimate of timing, uncut patches and host plant is above 1; the
    interaction goes both ways (0.28-4.97). Appropriate timing (1.18-3.25)
    never has an interval excluding 1. Uncut patches excludes 1
    once, on the 2021-2024 polygons.
  - Dominant host plant excludes 1 in 8 of 10.
  - The EVL is above 1 in all 12 analyses (1.12-3.87) and excludes 1 in 5. It
    is 1.61 (0.88-2.96) adjusted and 1.47 (0.79-2.72) matched.

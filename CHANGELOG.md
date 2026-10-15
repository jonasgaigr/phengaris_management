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

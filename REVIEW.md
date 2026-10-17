# Review of `phengaris_management`

Phase 1 of the revision: a critical review of the `clean-up` branch at commit
`b8d3297`. No pipeline code was changed for this review.

Every number below was computed from the raw exports in `Data/Input` and the
processed tables in `Data/Processed`. `review/review_evidence.R` recomputes all
of them, and its output is saved as `review/review_evidence.txt`.

**Severity**

- **blocks**: the results that depend on it are wrong or cannot be interpreted.
- **biases**: the results shift in a way that can change a conclusion.
- **cosmetic**: readability or robustness only; no effect on today's numbers.

**Category of the fix**

- **(a)** refactoring that gives identical outputs.
- **(b)** a bug fix; the outputs change for a documented reason.
- **(c)** an analytical or ecological decision, which waits for approval.

The status box is ticked when an item is resolved on the `revision` branch:
[x] resolved, [~] partly resolved, [ ] open. bNN refers to the bug fixes in
`CHANGELOG.md`, P3.n and P4.n to the Phase 3 and Phase 4 proposals below,
implemented in the `revised` specification.

## Baseline

The unchanged cascade was re-run from a clean worktree of `b8d3297`, with
today's WFS, RCzechia and Natura 2000 layers. It ran end to end in 7.1 minutes.

- Every file in `Outputs/Tables` is byte-identical to the committed one.
- The four processed CSVs are identical cell for cell.
- The reports differ only in their timestamps.
- The ordination figures differ, because the permutations are unseeded (17).

The committed outputs are therefore a valid baseline for the before/after
diffs required for (a) and (b) changes.

The environment works:

- R 4.6.1 is installed; `renv.lock` was written under 4.6.0.
- `renv::restore()` reports "The library is already synchronized with the
  lockfile."
- `//bali.nature.cz` is not reachable from this machine, so step 05 reuses the
  committed `data_lokal_vmb.csv`, exactly as it was designed to.

## Where this differs from the brief

| # | The brief says | What the repository shows |
|---|---|---|
| 2 | 861 duplicated `ID_NALEZ`, caused by sites intersecting several range polygons | 784 of the 861 are imputed absences that reuse the `ID_NALEZ` of the *other* species' record. Only **77** rows (75 groups) are true duplicates from polygon sites straddling two 10 km cells. |
| 4 | A visit-level rule would replace the coarse imputation | It would impute **more** absences, not fewer: 1944 *P. teleius* (now 843) and 396 *P. nausithous* (now 2). Whether that is right depends on the field protocol (Q1). |
| 7 | `as.numeric(YEAR)` on a factor returns the level index | `YEAR` is a factor only inside step 04. Steps 09–11 re-read `data_analysis.csv`, where `YEAR` is numeric, so `as.numeric(YEAR)` *is* the calendar year there. Not a bug today, but it depends on the CSV round-trip. |
| 14 | Pin `rn2kcz` to a commit | It is already pinned: `renv.lock` records `RemoteSha f928680…`, which `restore()` installs. Only `RemoteRef` still says `HEAD`. |
| 16 | Step 14 depends on objects left in the session by step 01 | Steps **02 and 03** do too: `phengaris_lokal_new`, `range_*`, `evl`, `mzchu`, `sites_subjects`. |
| — | "`{targets}` if I approve it; see the questions below" | The brief has no questions section. This is Q3 below. |
| 3 | Reconstructing persistent sites makes `(1 \| site)` identifiable | Only 3.5–5.4 % of reconstructed sites recur across years at any threshold from 0 to 200 m. The design mostly visits *different* sites each year, so a site random effect stays weakly identified. The mapping grid is the more useful grouping level. |

## Summary

| # | Issue | Severity | Fix | Status |
|---|---|---|---|---|
| 1 | Coordinates inflated 1×/10×/100× by `read_csv2()` | blocks | b | [x] b01 |
| 2 | Duplicated records; imputed rows share the source record's ID | biases | b | [x] b02 |
| 3 | No persistent site ID; `(1 \| X:Y)` is nearly observation-level | blocks | c | [x] P3.1 (revised) |
| 4 | Imputation per 10 km cell; species-specific fields inherited | blocks | b + c | [x] b03; P3.2 (revised) |
| 5 | `STRUKT_POZN` parsed with `grepl()` on the whole string | biases | a + b | [x] parser (a), b04 |
| 6 | Unknown coded as 0 (`GRAZE`, `OVERALL`, `MOW`, threats) | biases | b | [x] b05 |
| 7 | Dead filters, unused sources, `ZDROJ` year ≠ record year | cosmetic | a | [x] (a) |
| 8 | Habitat segment rule keeps the least natural, smallest segment | biases | c | [x] area-weighted on the current VMB layer, point sites over their KVES polygon (revised, P3.9) |
| 9 | AIC tables compare models fitted on different n | blocks | b | [x] b07 |
| 10 | `X:Y` fixed effect; `(1 \| YEAR)` entered twice | blocks | c | [x] P3.1, P3.6 (revised) |
| 11 | Single visits: naive occupancy, no detection covariates | biases | c | [x] P3.3 (revised); detection sensitivity P4.4 |
| 12 | Ordination on 4 rows, 1 residual df | blocks | c | [x] P3.7 (revised) |
| 13 | No diagnostics; separation and degenerate fits go unnoticed | biases | a + c | [x] b08; P3.4 (revised) |
| 14 | `00_setup.R` installs packages, bypassing renv | cosmetic | a | [x] (a) |
| 15 | Remote layers fetched live on every run | biases (drift) | a | [x] (a) |
| 16 | Steps 02, 03, 14 depend on session objects | cosmetic | a | [x] (a) |
| 17 | Hard-coded paths, stale outputs, tracked intermediates, no seeds | cosmetic | a | [x] (a), b08 |
| 18 | Two threat codebooks; legacy patterns miss 46 % of the older one | biases | c | [~] mapping proposed (revised), TODO check; Article 17 codes P4.11 |
| 19 | `PRIKOP` misses plain "příkop"; `HET_INN` = 0 when unmatched | biases | b | [x] b06 |
| 20 | The field form changed in 2021; geometry and keys confounded with year | biases | c | [x] P3.4 (revised) |
| 21 | The host-plant filter silently drops 869 records | biases | a (report) + c | [x] reported (a); revised keeps the unknown-plant records |
| 22 | Inherited counts contaminate the abundance comparisons | biases | b + c | [x] b03; P3.8 (revised) |
| 23 | Step 02 silently drops 365 records outside the species' own range | biases | a (report) + c | [x] reported (a); kept in revised (Q2) |
| 24 | `EVL_target` is species-blind, contradicting its own documentation | biases | c | [x] `EVL_species` (revised); matching P4.8 |
| 25 | Small data oddities: out-of-season dates, wrong `.prj`, candidate sources | cosmetic | a + TODO | [~] out-of-season flagged (revised); TODOs in config.yml |
| 26 | SPEC_NUM is 1 only on positive records: separation by construction | blocks | c | [x] co-occurrence reformulated (revised) |

---

## Data integrity

### 1. Coordinates are corrupted by `read_csv2()`

- **Location.** `R/01_load_source_data.R:129-134` (`read_occurrence_csv()`).
- **Severity.** Blocks the `X:Y` fixed effects, every distance, every spatial
  autocorrelation test and the planned connectivity index.

**Evidence.**

- `read_csv2()` reads `.` as a grouping mark, but the exports use `.` as the
  decimal mark.
- Across both 2019–2024 exports, X has 8260 values with 2 decimals, 824 with 1
  and 1028 with 0. The *P. nausithous* file alone has 6519 / 635 / 768, which
  matches the brief.
- The ratio of the parsed value to the correct one is exactly 100, 10 and 1 for
  those three groups. Y behaves the same way: 8189 / 893 / 1030.
- Parsed X spans −88 220 276 to −464 448, against a correct range of
  −882 203 to −450 487.
- All 4540 analysis rows have at least one coordinate outside the S-JTSK
  bounding box of Czechia.
- The `(1 | X:Y)` grouping is *not* affected. The corruption is a
  deterministic function of the string, so there are 3201 levels either way.
- The site geometries come from the shapefiles, so protection, imputation and
  the habitat intersection are unaffected too.
- The four spatial GLMs are affected: `nau_spatial`, `nau_spatiotemporal`,
  `tel_spatial` and `tel_spatiotemporal`. For example, the `nau_spatial` slope
  is −4.3 × 10⁻¹⁷ with p = 4 × 10⁻⁴, an artefact.
- Every other column `read_csv2()` parses as numeric holds integers and parses
  correctly: `POCET`, `ODHAD`, `ID_*`, `DATUM_*`, `SITMAP`, `CXPRESNOST` and
  the rest. One `GAR_POZN` value is lost because readr guesses that column as
  logical (cosmetic).

**Fix (b).**

- Read with `readr::read_delim(delim = ";", locale(decimal_mark = ".",
  grouping_mark = "", encoding = "Windows-1250"))` and explicit `col_types`.
- Add a validation that X and Y lie inside the S-JTSK bounding box of Czechia.
- Expected changes: X and Y in every processed CSV, and the coefficients of the
  four spatial GLMs.

### 2. Duplicated records

- **Location.** `R/02_impute_absences.R:44-91`, the intersection with the
  range cells; `:116-172`, where imputed rows keep `ID_NALEZ` through
  `everything()`.
- **Severity.** Biases: n is inflated and records are pseudoreplicated.

**Evidence.** There are 3679 distinct `ID_NALEZ` among the 4540 rows of
`data_analysis.csv`, i.e. 861 duplicates. They have two causes:

- **784** are imputed absences that share the `ID_NALEZ` of the source record
  of the *other* species. They are distinct species-level rows, but they have
  no identifier of their own.
- **77** rows in 75 groups are true duplicates: the same record and species,
  repeated because a polygon site straddles two 10 km reporting cells. All 75
  are polygon sites.

The cells themselves are clean: 766 *P. nausithous* and 443 *P. teleius*
cells, no identical twins, no overlaps.

After deduplicating on (`ID_NALEZ`, `DRUH`):

- *P. nausithous*: 3151 rows, previously 3216.
- *P. teleius*: 1310 rows, previously 1324.

**Fix (b).**

- Deduplicate on (`ID_NALEZ`, `DRUH`) right after the range intersection. Keep
  one cell deterministically: the one containing the site's representative
  point.
- Give imputed rows their own key `ID_RECORD` and keep `SOURCE_ID_NALEZ` for
  traceability.

### 3. No persistent site identifier

- **Location.** Every `(1 | X:Y)` in `R/09_*.R`, `R/10_*.R` and `R/11_*.R`.
- **Severity.** Blocks: the random-effect structure is close to
  observation-level and barely identifiable in a Bernoulli GLMM.

**Evidence.**

- `ID_LOKAL` has 3401 levels, each in exactly one year.
- `NAZ_LOKAL` has 2851 levels; 123 of them (4.3 %) occur in more than one year.
- `X:Y` has 3201 levels for 4540 rows, of which 1943 are singletons. Per
  species:
  - *P. nausithous*: 3102 levels for 3216 rows, 2995 singletons.
  - *P. teleius*: 1290 levels for 1324 rows, 1260 singletons.
- The fits show it. In `tel_plant`, `tel_plant_poly`, `tel_graze` and
  `tel_plant_area_subset` the `X:Y` variance runs to 1575–1745 on the logit
  scale, against 0.41 in `tel_null`. These are degenerate optima, and none of
  them raised a warning (see 13).
- Reconstructing persistent sites helps little. Site geometries were grouped by
  overlap or a distance threshold, taking connected components:

  | threshold | sites | in > 1 year | sites with a single record |
  |---|---|---|---|
  | 0 m (touching) | 2979 | 104 (3.5 %) | 1636 |
  | 25 m | 2859 | 113 (4.0 %) | 1521 |
  | 50 m | 2793 | 112 (4.0 %) | 1460 |
  | 100 m | 2670 | 127 (4.8 %) | 1341 |
  | 200 m | 2496 | 136 (5.4 %) | 1199 |

- The mapping grid `SITMAP` is a better-populated level:
  - 364 squares, of which 188 were surveyed in more than one year;
  - by number of years surveyed: 176 / 114 / 55 / 12 / 7 squares for 1 to 5
    years;
  - a median of 9 records per square.

**Fix (c).**

- `match_sites()` builds `SITE_ID` from overlap or distance, with
  `site_match_distance_m` in the config (`TODO(Jonáš)`; 50 m proposed).
- Revised random effects: `(1 | SITMAP) + (1 | YEAR)`, with `(1 | SITE_ID)` as
  an option, and a sensitivity check over 0 / 50 / 200 m.

### 4. The absence imputation (step 02)

- **Location.** `R/02_impute_absences.R:116-172`.
- **Severity.** Blocks for *P. teleius*: 70 % of its negatives are imputed.

**Evidence.**

- The unit is the 10 × 10 km EEA reporting cell (`row_n`). All 1209 cells are
  exactly 100 km².
- An absence is imputed for every targeted record of the other species in a
  cell where the target species has no targeted record in *any* year
  2019–2024. Visit, date and year are ignored.
- 845 rows are imputed: 843 *P. teleius* and 2 *P. nausithous*. 789 survive to
  `data_analysis.csv`.
  - *P. teleius*: 787 of 1122 negatives are imputed (70.1 %).
  - *P. nausithous*: 2 of 1747 (0.1 %).
- Imputed rows inherit every column of the source record, including the
  species-specific ones:
  - `POCET` is non-NA in 397 of 845 imputed rows (363 of 789 in the analysis
    table). These are counts of the other species.
  - `CESKE_JMENO` is wrong in every imputed row; imputed *P. teleius* rows say
    "modrásek bahenní".
  - `ID_NALEZ` is shared with the source record (see 2).
  - The other species' sex ratio (`Poměr pohlaví`) sits inside `STRUKT_POZN`.
- Source records of both outcomes are used: 396 positive and 447 negative
  *P. nausithous* records became *P. teleius* absences.
- Site-level fields are inherited legitimately, since they describe the site:
  mowing, plants, threats, `POP_BIOT`.

A visit-level rule was checked. The rule: same `ID_LOKAL` and same date, the
other species recorded, this species not, and the site inside this species'
range. Under it:

- *P. teleius* candidates: 1944 visits. On only 689 of them did the same
  observer enter a *P. teleius* record anywhere that day.
- *P. nausithous* candidates: 396 visits.
- Explicit negatives are common: 772 *P. teleius* and 2242 *P. nausithous*
  targeted negatives were entered. A missing record may therefore mean "not
  looked for" rather than "absent".

**Fix (b), clear-cut.**

- Blank the species-specific fields of imputed rows: `POCET`, `ODHAD`,
  `POCITANO`, `REL_POC`, the sex-ratio key, and `CESKE_JMENO` set from `DRUH`.
- Give imputed rows their own ID (see 2).
- This changes only the count summaries, the tests and the figures (see 22).

**Fix (c).**

- `config: imputation$rule ∈ {legacy_cell, visit, none}`. `TODO(Jonáš)`: see
  Q1.
- Every model is fitted with and without imputed absences.

### 5. `STRUKT_POZN` parsing

- **Location.** `R/04_clean_occurrence.R:198-266`.
- **Severity.** Biases the threats and `None`. Host plant, timing and method
  are currently correct, but only by luck.

**Evidence.**

The keys that occur, counted over targeted records:

| key | records | note |
|---|---|---|
| `Perspektiva lokality` | 4100 | |
| `Negativní vlivy na lokalitě` | 2935 | |
| `Seč vhodně načasovaná` | 2502 | |
| `Přítomnost rostlin` | 2410 | |
| `Seč celoplošná` | 2409 | |
| `sta_pritomnostrostlin` | 1876 | 2019–2020 only; a second host-plant key |
| `vliv` | 894 | 2021 onwards |
| `Poměr pohlaví` | 605 | |
| `Pastva` | 120 | 2021 onwards |
| `jiný vliv` | 44 | free text |
| `Procento mikrolokalit s vyhovujícím stavem živných rostlin` | 1 | |

66 non-empty strings contain no key at all.

The values per key:

| key | values |
|---|---|
| timing (`Seč vhodně načasovaná`) | {ano, ne} |
| method (`Seč celoplošná`) | {ano, ne} |
| grazing (`Pastva`) | extenzivní 81, intenzivní 37, ne 2 |
| `Perspektiva lokality` | neznámá 1644, špatná 1395, dobrá 1061 |
| host plant | jednotlivě, hojně, žádné, dominantně |

Against a key–value parse, on all 4540 analysis rows:

- `PLANT_QUANT`, `TIMING` and `METHOD` agree exactly (0 disagreements). The
  `grepl()` approach is fragile but not wrong today.
- `None` uses the pattern `"žádné"`. That pattern matches only
  `Přítomnost rostlin: žádné` (no host plant), and those rows are filtered
  out. So `None` ≡ 0 in all 4540 rows, while the real phrase `"žádný vliv"`
  occurs in 671 of them. `None` is also summed into `SUM_THREATS`.
- `Perspektiva lokality` is not used.
- Free text under `jiný vliv:` (92 rows) is searched by every `grepl()`.
- Threat items contain commas outside parentheses, for example
  `…hrází rybníků, kanálů, mokřadů, nádrží, zpevňování stok`. Items therefore
  have to be matched against a vocabulary; splitting on commas is not enough.
  The two codebooks are covered in 18.

**Fix.**

- **(a)** `parse_strukt_pozn()`, a tested key–value parser that returns one
  column per key. Threat items are matched against a vocabulary table in the
  config. With the legacy mapping it reproduces today's flags exactly.
- **(b)** `None` ← `"žádný vliv"`, and removed from `SUM_THREATS`.

### 6. NA versus 0

- **Location.** `R/04_clean_occurrence.R:124-129` (`flag_note`), `:248-266`.
- **Severity.** Biases.

**Evidence.**

- **`GRAZE`** is never NA, and it is wrong in two ways:
  - The `Pastva` key only exists from 2021. In 2019–2020, 0 of 2073 rows
    carry it; grazed records in 2021–2024 number 35 / 34 / 6 / 21. So
    `GRAZE = 0` in 2019–2020 means "not recorded".
  - `Pastva: ne` (2 rows) is coded `GRAZE = 1`.
- **`MANAGEMENT_HET`** ≡ `!is.na(METHOD)` on all 4540 rows.
- **`OVERALL`** is 0 for the 2089 rows where mowing was not assessed.
- **`MOW`** is the presence of a `Seč` key; it is 0 in 1935 rows. Whether the
  form omits the `Seč` fields when a site is not mown is `TODO(Jonáš)`.
- **Threat flags.** 610 rows have no threat key at all, yet every threat flag
  and `SUM_THREATS` is 0 for them.

**Fix (b).**

- Unknown stays NA.
- `GRAZE` comes from the `Pastva` key: extenzivní or intenzivní → 1, ne → 0,
  missing → NA.
- `OVERALL` is NA when `METHOD` or `TIMING` is NA.
- The threat flags are NA when no threat key is present.
- `MANAGEMENT_HET` is kept as legacy, with its real definition documented.
- Expected change: the grazing models lose 2019–2020, and their n is reported.

### 7. Dead filters and stale code

- **Location.**
  - `R/00_setup.R:132-150`, `:150`;
  - `R/04_clean_occurrence.R:309`;
  - `R/10_models_teleius.R:56-58`;
  - `R/01_load_source_data.R:35` (`LOAD_HISTORICAL`), `:209-245`.
- **Severity.** Cosmetic.

**Evidence.**

- `EXCLUDED_YEARS = c("2014", "2017")` and `YEAR != "2018"` remove 0 rows.
- 4 of the 10 entries in `target_mon_zdroj` match 0 rows:
  - the 2017 and 2018 "Monitoring totenových modrásků" sources, which occur
    only in the 2012–2018 exports;
  - the 2023 and 2024 "Monitoring motýlů" sources.
- The `ZDROJ` year differs from the record year:
  - "Kolektiv autorů (2022) Monitoring motýlů." holds 5 records from 2021, 761
    from 2022, 453 from 2023 and 565 from 2024;
  - the 2021 sources hold 11 records from 2019 and 17 from 2020.
  - Whether this is intended is `TODO(Jonáš)`.
- The 2012–2018 data are loaded on every run; no step uses them.
- `YEAR`: see the table at the top.

**Fix (a).**

- Remove the dead filters and the historical load.
- Move the source list into the config.
- Make `YEAR` an explicit integer.

### 8. Habitat segment choice (step 06)

- **Location.** `R/06_habitat_join.R:91-124`.
- **Severity.** Biases every habitat model.

**Evidence.**

- There are 18 816 site × segment overlaps for 8678 sites.
- `arrange(NATURAL, AREA_SITE) |> slice(1)` keeps the least natural, smallest
  overlap. Only 684 of 4540 rows keep a `BIOTOP` value, although 1952 rows
  belong to sites with at least one natural segment.
- `AREA_SITE` is the overlap area of the chosen segment, not the site area. On
  polygon sites, the median of `AREA_SITE` / site area is 0.108, and 63 % of
  rows are below 0.5.
- Point sites (2022 rows) have `AREA_SITE = 0`. Because of 20, `AREA_SITE > 0`
  effectively means "polygon sites, 2021–2024". There are no line sites in the
  analysis table.
- `BIOTOP` comes from `HAB_BIOTOP` only, so NA means "no Natura habitat record
  for the segment", not "no habitat".

**Fix (c).**

- Area-weighted summaries per site:
  - overlap with a configurable *Sanguisorba*-meadow habitat group
    (`TODO(Jonáš)`: code list);
  - the share of natural habitat;
  - the dominant habitat by area.
- Point and line sites get a buffer (`TODO(Jonáš)`: width).
- The legacy rule stays available as an option.
- Habitat input contract: see the plan.

## Statistics

### 9. AIC tables compare models fitted on different data

- **Location.**
  - `R/09_models_nausithous.R:329-374`;
  - `R/10_models_teleius.R:360-402`;
  - `R/11_models_both_species.R:84-93`;
  - `R/functions/model_helpers.R:203-213`.
- **Severity.** Blocks: these ΔAIC values mean nothing.

**Evidence.** Comparison sets that mix n:

| table | models (n) | ΔAIC |
|---|---|---|
| `09_nausithous_aic_grassland` | `nau_management_ttp` (1752), `nau_ttp` (3195) | 1991 |
| `10_teleius_aic_habitat` | `tel_area_null` / `tel_area` (713), `tel_plant` / `tel_plant_poly` (1324) | |
| `10_teleius_aic_management` | `tel_method_timing` (698), `tel_method` (706), `tel_timing` (754) | |
| `11_both_species_aic` | `both_mapped_habitat` (684), the others (4514–4540) | ≈ 4400 |

Other mismatches:

- `nau_threats` is fitted on `d_nau_area` (polygon sites only), with no stated
  reason.
- `nau_graze` uses n = 3216, while its null `nau_graze_null` uses n = 77.
- No comparison set mixes `glm` with `glmer`. The fit-statistics tables do list
  both kinds of AIC side by side.

**Fix (b).**

- Each comparison set is fitted on one data frame, filtered to complete cases
  on the union of its variables.
- `aic_comparison()` stops when n differs within a set.
- The legacy individual fits stay as they are; only the comparison tables
  change.

### 10. `X:Y` fixed effect and duplicated `(1 | YEAR)`

- **Location.**
  - `R/09_models_nausithous.R:112,118,296`;
  - `R/10_models_teleius.R:117,123,320`.
- **Severity.** Blocks the "spatial" models.

**Evidence.**

- The slope is per unit of X·Y, a product of about 10¹⁵ with the corrupted
  coordinates: −4.3 × 10⁻¹⁷ for *P. nausithous* and −6.6 × 10⁻¹⁸ for
  *P. teleius*.
- `nau_het_inner` and `tel_het_inner_dup_year` estimate a second, arbitrary
  year variance: `YEAR.1` = 0.060 and 0.0021.

**Fix (c).**

- Legacy: kept, reproducible.
- Revised: no `X:Y` term. Space enters as a grid random effect or a smooth
  (see 3 and P3.6).

### 11. Single visits: these are naive occupancy models

- **Severity.** Biases: detection is confounded with occupancy.

**Evidence.**

- There are 4433 site (`ID_LOKAL`) × year × species combinations. 4330 have
  one record (4405 after deduplication), and only 22 have more than one date.
- With `NAZ_LOKAL` as the site, 110 of 4156 combinations have more than one
  date. That is too few for a detection-corrected occupancy model.
- Day of year runs from 177 to 347; the 1–99 % range is 191–234. The median for
  positive records is 209 for *P. nausithous* and 204 for *P. teleius*.
- 80 observers, with a median of 14.5 records each and a maximum of 772.

**Fix (c).**

- Say in every model report that these are naive occupancy models.
- Add day of year (quadratic, relative to the flight window) and
  `(1 | observer)`. `TODO(Jonáš)`: the flight window.

### 12. The ordination cannot be interpreted

- **Location.** `R/13_threats_ordination.R:48-107` (`group_by(DRUH, POSITIVE)`).
- **Severity.** Blocks.

**Evidence.**

- The 2518 records with `SUM_THREATS > 0` are collapsed into 4 rows. Both the
  PCA and the RDA (`DRUH + POSITIVE`) run on those 4 rows.
- The RDA has 1 residual df.
- The permutation p-values are 0.333 overall and 0.5 / 0.167 by term. These
  are the coarse values that 4 rows allow.
- The VIFs are 1 by construction.

**Fix (c).** Replace it with an analysis at record or site level (P3.7).

### 13. No diagnostics

- **Location.** `R/functions/model_helpers.R:40-82`.
- **Severity.** Biases: broken fits are reported as `ok`.

**Evidence.**

- `fit_binomial()` captures warnings only, and several broken fits raise none:
  - `tel_graze`: intercept −11.8, slope 21.6, SE 14.1. This is separation:
    9 of 18 grazed records are positive.
  - `tel_plant` and its relatives: `X:Y` variance about 1745.
- Fits that do warn:
  - `tel_fsb`: degenerate Hessian;
  - `nau_evl_year`: "nearly unidentifiable";
  - `nau_management_ttp`: degenerate Hessian.
- `tel_resource_density` fails on `log(0)`: 611 *P. teleius* rows are point
  sites.
- There are no residual checks, no VIF, no confidence intervals, and dozens of
  univariate models are reported with p-values.

**Fix.**

- **(a)** Additive diagnostics columns:
  - singular fit;
  - maximum |coefficient| and SE as a separation flag;
  - random-effect variance above a threshold;
  - DHARMa uniformity and dispersion tests.
- **(c)** The multivariable revised models (P3.4).

## Reproducibility

### 14. `00_setup.R` installs packages

- **Location.** `R/00_setup.R:36-90`: `install.packages()`, `remove.packages()`
  and `remotes::install_github()`.
- **Severity.** Cosmetic. renv works (see the baseline).

**Notes.**

- `rn2kcz` is already pinned (see the table at the top).
- Packages the revision needs that are installed but not in `renv.lock`:
  DHARMa, glmmTMB, testthat, lintr, styler, checkmate, yaml.
- Not installed: marginaleffects, brglm2.

**Fix (a).**

- Setup checks `renv::status()` and `requireNamespace()` for each package, and
  stops with a message that says to run `renv::restore()`.
- Set `RemoteRef` to the commit SHA.
- `renv::snapshot()` once the new packages are in use.

### 15. Remote layers are fetched live

- **Location.** `R/01_load_source_data.R:44-99`, `R/14_maps.R:59`.
- **Severity.** Biases, through silent drift between runs. Today's layers still
  reproduce the committed outputs.

**Evidence.** Four layers are fetched on every run:

- the WFS layers EVL, MZCHU and the mapping grid, over `http`. `https` works
  too; it was checked;
- `rn2kcz::load_n2k_sites()`, which downloads
  `seznam_predmetolokalit_Natura2000_2_2025.xlsx` from webgis.nature.cz and
  assigns the table into the global environment;
- `RCzechia::republika()`;
- `RCzechia::vyskopis()`.

**Fix (a).**

- `R/tools/refresh_reference_layers.R` (opt-in) writes GeoPackage, CSV and TIF
  copies to `Data/Input/reference/`, together with `RETRIEVED.yml` recording
  the date and URL.
- The cascade reads only those local copies.

### 16. Steps depend on session objects

- **Location.**
  - `R/02_impute_absences.R:44-172`;
  - `R/03_protected_areas.R:17,71-121`;
  - `R/14_maps.R:36-51`;
  - `run_all.R` (one shared session).
- **Fix (a).**
  - Step 01 writes the joined records-with-geometry and the range layers to
    disk.
  - Every step reads its declared inputs.
  - `run_all.R` checks that a step's inputs exist before running it.

### 17. Paths, leftovers and seeds

**Hard-coded paths.**

- `//bali.nature.cz` in `R/functions/habitat_layers.R:23,40-55,154-164,225-237`;
- `S:/Složky uživatelů/…` at `:132,309`.

**Stale or untidy files.**

- `Outputs/Plots/` (22 files), 6 `*.docx` files and 15 legacy CSVs in the
  root of `Outputs/`.
- `Outputs/data_pca.csv` and `Outputs/data_data_export.csv` are *still written*
  by `R/13_threats_ordination.R:141-152`, into the root of `Outputs/`.
- `Data/Processed/data_new_with_imputed.csv`: no step writes it.
- 4 ArcGIS `*.sr.lock` files.

**Tracked intermediates.**

- `Data/Processed` (8 files, about 38 MB) and `Outputs` (178 files) are
  tracked in git.
- `data_lokal_vmb.csv`, the legacy habitat intersection, **cannot be
  regenerated** without the habitat layer (Q4).

**Unseeded permutations.** `R/13_threats_ordination.R:305-306,455,527-537`.

**Also cosmetic.** `sf::st_crs("+init=epsg:5514")` is deprecated syntax
(`R/01_load_source_data.R:50,72,79,86`).

**Fix (a).**

- Move both paths into the config, as an optional loader.
- Delete the leftovers and move the step 13 exports.
- `.gitignore`: see Q4.
- Set seeds from the config.

## Found during the review

### 18. Two threat codebooks

- **Location.** `R/04_clean_occurrence.R:220-233`.
- **Severity.** Biases: the threats are under-coded in 2019–2021, which
  confounds them with year.

**Evidence.**

- Threats are entered under `Negativní vlivy na lokalitě` (2019–2023, the older
  list) or `vliv` (2021–2024, the newer list).
- The legacy patterns capture 1298 of 1561 item occurrences in the newer list.
  The ones missed are mostly `žádný vliv` (236) and `jiný vliv` (22).
- In the older list they capture only 3352 of 6214 (54 %). Missed items
  include:

  | item | occurrences |
  |---|---|
  | `sečení` | 857 |
  | `vývoj biocenózy` | 540 |
  | `žádný vliv` | 442 |
  | `eutrofizace` | 335 |
  | `pastva` | 101 |
  | `vysychání (mokřadního ekosystému)` | 94 |
  | `hnojení` | 68 |
  | `invaze nějakého druhu (neofyty)` | 36 |
  | `meliorace` | 33 |
  | `odvodňování` | 33 |
  | `sešlapávání` | 32 |
  | `zaplavování` | 31 |

- In some places the older list is hierarchical. For example,
  `sukcese všeobecně` and `vývoj biocenózy` both occur 540 times.

**Fix (c).**

- A vocabulary table in the config maps each item of both lists to a threat
  class.
- The legacy mapping reproduces today's flags.
- The proposed mapping of the older items is `TODO(Jonáš)`.

### 19. Habitat-description flags (`POP_BIOT`)

- **Location.** `R/04_clean_occurrence.R:212-215,297-303`.
- **Severity.** Biases `PRIKOP` and `HET_INN`.

**Evidence.**

- The controlled `POP_BIOT` items are, by count:

  | item | count |
  |---|---|
  | `TTP s pravidelným managementem` | 2876 |
  | `zarůstající louka bez managementu` | 1137 |
  | `příkop` | 695 |
  | `příkop u komunikace` | 554 |
  | `jiný` | 270 |
  | `okraj vodní plochy` | 263 |
  | `jiný typ biotopu[-text]` | 105 |
  | `pastvina` | 13 |

  Free text accounts for the rest.
- The pattern `"příkop u komunikace"` misses plain `příkop`, so 722 rows that
  mention `příkop` have `PRIKOP = 0`.
- `okraj vodní plochy` and `pastvina` are not mapped to any flag.
- `HET_INN = 0` in 380 rows: 26 have no description, and in 354 the
  description holds only unmapped categories.

**Fix.**

- **(b)** `PRIKOP` matches `příkop` (the variable, and the figure label "road
  verge, ditch", cover ditches).
- **(c)** The mapping of the other categories goes into the config with
  `TODO(Jonáš)`.

### 20. The field form changed in 2021

- **Severity.** Biases every variable recorded only in some years, because it
  is confounded with `YEAR`.

**Evidence.**

- Site geometry type by year:
  - 2019: 938 points, 44 polygons;
  - 2020: 1084 points, 7 polygons;
  - 2021–2024: polygons only.
- The same year brings `sta_pritomnostrostlin` → `Přítomnost rostlin`, the new
  `Pastva` key and the switch to the `vliv` codebook.
- The *P. nausithous* year variance is 0.28 on all rows and 3.7 in the
  `AREA_SITE > 0` subset.

**Fix (c).**

- Add a `PROTOCOL_ERA` indicator (2019–2020 vs 2021–2024).
- Restrict each variable to the years in which it was recorded.
- Report the restriction.

### 21. The host-plant filter drops 869 records

- **Location.** `R/04_clean_occurrence.R:306-310`.
- **Severity.** Biases coverage.

**Evidence.**

- 5409 → 4540 rows: 389 records with `žádné` (no host plant) and 480 with no
  plant information.
- 331 of the 480 are from 2021, mainly the "2021 Monitoring totenových
  modrásků" source, whose `STRUKT_POZN` is empty (335 rows).

**Fix.**

- **(a)** Report the filter.
- **(c)** Keep dropping `žádné`: a site without *Sanguisorba* cannot host the
  species, so these are structural zeros. Report the unknown-plant records as a
  coverage gap.

### 22. Inherited counts contaminate the abundance comparisons

- **Location.**
  - `R/functions/summary_tables.R:108` (`summarise_abundance()`);
  - `R/11_models_both_species.R:113-145`;
  - `R/12_model_figures.R:262-292`.
- **Severity.** Biases.

**Evidence.**

- 362 of the 564 non-NA *P. teleius* counts in `data_analysis.csv` are
  inherited *P. nausithous* counts.
- The Wilcoxon and Kruskal–Wallis tests on `POCET`, the step 07 abundance
  table and the step 12 count figure all use them.
- A Wilcoxon test on the binary `POSITIVE` (step 11) is not a meaningful
  comparison of occupancy.

**Fix.**

- **(b)** Follows from 4 (b); the numbers are reported.
- **(c)** A proportion test or the both-species GLMM.

### 23. Records outside the species' own range are dropped

- **Location.** `R/02_impute_absences.R:44-78`.
- **Severity.** Biases, because the drop is undocumented.

**Evidence.**

- 365 targeted records (85 *P. nausithous*, 280 *P. teleius*) fall outside the
  species' own reporting range and vanish in step 02.
- All 365 are explicit negatives. So 280 real *P. teleius* absences are
  dropped while 787 imputed ones are added.

**Fix.**

- **(a)** Report the drop.
- **(c)** `config: keep_outside_range` (`TODO(Jonáš)`).

### 24. `EVL_target` is species-blind

- **Location.** `R/04_clean_occurrence.R:171-176`. The header of
  `R/03_protected_areas.R:12` says "designated for that species".
- **Severity.** Biases the protection models.

**Evidence.** Records flagged:

| species | species-blind (current) | species-specific |
|---|---|---|
| *P. nausithous* | 252 | 244 |
| *P. teleius* | 120 | 68 |

`protected_area_id.csv` also carries 1033 duplicate rows. They are harmless,
because the IDs are passed through `unique()`.

**Fix (c).** Build the species-specific flag from `PA_SPECIES` (P3.4). The
legacy flag stays.

### 25. Small data oddities

- **Out-of-season dates.** Three rows are dated 2024-10-29 and 2024-12-12
  (day of year 303 and 347). All are negatives, one of them imputed.
  `TODO(Jonáš)`: drop them, or flag them via the flight window.
- **Wrong `.prj`.** The site shapefiles' `.prj` declares ETRS89/LAEA, but the
  coordinates are S-JTSK. `st_set_crs(5514)` is therefore correct, and the
  comment "carry no CRS" is wrong.
- **A mislocated record.** One record's validation note, recovered by the
  b01 reader fix, reads "špatná lokalizace, na místě není vhodný biotop ani
  živná rostlina" (wrong location; no suitable habitat or host plant on the
  spot). `TODO(Jonáš)`: exclude it?
- **Candidate sources.** These sources are not on the list but may be targeted
  monitoring:
  - "Kolektiv autorů (2024) Síťové mapování denních motýlů v letech 2024-2029"
    (156 rows);
  - "FIŠER M. (2020/2021) Monitoring EV druhů" (129 / 65);
  - "FIŠER M. (2023) Mapováni EV druhů (2023)" (52).
  - `TODO(Jonáš)`.

### 26. SPEC_NUM separates occupancy by construction

- **Location.** `R/functions/derive_variables.R` (`SPEC_NUM`), and the
  `both_cooccurrence` model in `R/functions/model_specs_legacy.R`.
- **Severity.** Blocks the co-occurrence model.
- **Found by** the diagnostics added in b08.

**Evidence.**

- SPEC_NUM is 1 when a record's visit had *both species found*. It is built
  from positive records only, so a record with SPEC_NUM = 1 is positive by
  definition.
- The model `as.factor(POSITIVE) ~ as.factor(SPEC_NUM) * as.factor(DRUH)`
  therefore separates. The SPEC_NUM coefficient is about 2.3 × 10⁵, with an SE
  of about 6.6 × 10⁶.
- The co-occurrence question needs a different formulation: does the
  presence of one species predict the other at the same visit? That is a
  model of one species' occupancy with the other species' *recorded*
  outcome as a covariate, which exists only at visits where both were
  surveyed.

**Fix (c).** P3.4.

---

## Status after Phase 2

**Done.** Phase 2 is done: every (a) refactoring and every clear-cut (b) fix
listed in the plan below.

**How it was checked.**

- Each (a) commit was checked to leave every processed CSV and result table
  unchanged.
- Each (b) commit is documented in `CHANGELOG.md`, with its full cell-by-cell
  diff in `changes/`.

**Approved decisions, implemented as switchable options** (legacy unchanged):

- Q1: the visit-level imputation rule, which stays inside the imputed
  species' range;
- Q2: keeping explicit negatives outside the range.

**Open, awaiting approval:** the Phase 3 proposals P3.1-P3.9 below, and with
them the revised specification.

**Open, needing domain input.** These are the `TODO(Jonáš)` entries in
`config.yml`:

- candidate sources;
- the meaning of a missing grazing or mowing entry;
- the older threat list;
- unmapped habitat categories;
- the site-matching distance;
- buffer widths;
- *Sanguisorba* codes;
- the mosaic share column;
- out-of-season dates;
- the `ZDROJ` year.

---

## Status after Phase 3

**Decisions** (2026-10-09):

| | Answer | Implemented as |
|---|---|---|
| Q1 | Impute only inside the cells of the reporting range; never outside the species' expected occurrence | Every rule imputes inside the imputed species' range only. Step 02 checks this on the site geometries and stops if it fails. The revised specification uses the visit rule. |
| Q2 | Drop no true records; impute nothing outside the range | `keep_outside_range: true` in the revised specification (85 + 280 explicit negatives kept) |
| Q3 | `run_all.R` | Kept, with model-set-specific steps |
| Q4 | Option A | Already in place (`Data/Input/habitats/legacy/`) |
| Q5 | As in P3.1-P3.9 | Domain values set in `config.yml`; the ones not explicitly decided are marked PROPOSED |
| P3.1-P3.9 | Approved | The `revised` specification, steps 16-20; P3.3 with the flight season as the deviation from the year's median positive record of the species |

**Done.** The `revised` specification runs beside `legacy`, whose outputs
are unchanged (`changes/c01_phase3_legacy.md`: 86 files identical, 0
changed). Results and numbers are in `CHANGELOG.md` and
`Outputs/REPORT.md`.

**Found during Phase 3.**

- **27. *P. teleius* negatives were barely entered in 2019.** There are 5
  explicit negatives against 517 imputed ones. Models on the observed
  *P. teleius* records alone are therefore degenerate, and the imputed
  absences are what makes *P. teleius* modelable.
- **28. `.regex_escape()` escaped nothing**, because a TRE bracket expression
  takes the backslashes literally. It had no effect on the parser keys. It is
  fixed, and the fix matters for the harmonised threat items.
- **29. The threat lists by year.** The 2019-2020 threat list is the older
  one. From 2021 the current list is used, under either key, so the threat
  codebook coincides with `PROTOCOL_ERA`.
- **30. The host-plant filter drops structural zeros.** The records with the
  host plant recorded absent are structural zeros: 4 positives out of 342.
  The revised specification drops only those.

**Open, needing domain input** (`TODO(Jonáš)` / PROPOSED in `config.yml`):

- the mapping of the 2019-2020 threat items;
- the connectivity alpha (1 per km);
- the 45-day flight-season cut-off;
- the observer alias "Valda Slávek" = "Slavomír Valda";
- the meaning of a missing grazing or mowing entry;
- candidate sources;
- unmapped habitat categories;
- buffer widths;
- *Sanguisorba* codes;
- the mosaic share column;
- the `ZDROJ` year.

## Status after the habitat download (2026-10-15)

**Done.** The VMB and KVES 2025 layers were downloaded from data.nature.cz
for all 14 kraje, following the alcon-spatial project
(`R/tools/download_habitat_layers.R`). The revised specification now
describes the habitat from them (P3.9 on real data); legacy keeps the frozen
intersection and its outputs are unchanged
(`changes/c02_habitat_layers_legacy.md`). Numbers are in `CHANGELOG.md`.

The current intersection reproduces the frozen one where the segments have
not been updated since: 99.6 % of the frozen overlaps are found again, and for
the unchanged segments FSB and `BIOTOP_SEZ` agree in 100 % and the overlap
area in 99.9 % (`05_current_vs_frozen`).

**Found while downloading the habitat layers.**

- **31. The frozen intersection sees only Natura 2000 habitats.** Its
  `BIOTOP` is filled in exactly the 7245 of 18 816 rows with a `HABITAT`
  code. T1.5 (2104 rows in the current layer), T1.4 (445), every X habitat and
  the unmapped parts never appear. Under the legacy rule this is part of why
  only 684 of 4540 rows kept a `BIOTOP` (item 8). Under the area-weighted rule
  the median natural share of polygon sites was 0.019; on the current layer
  it is 0.410.
- **32. The frozen intersection counts a segment once per Natura record.**
  Summed over a site, the overlap area exceeded the site's own area for 4.3 %
  of polygon records; with each habitat weighted by `STEJ_PR` it matches the
  site area within 1 % for 99.9 %.
- **33. The published VMB has no Natura 2000 code.** `HABITAT` is empty in
  the downloaded layer, and `FSB_EVAL` has to be derived. The network share's
  derivation kept `moz.` for mosaics of X habitats only and counted natural
  habitats with a Natura record twice (389 overlaps differ); the new one is
  X for every mapped segment less than half natural.

**Decisions** (2026-10-15):

| | Answer | Implemented as |
|---|---|---|
| Mosaic share column | `STEJ_PR` | `habitat: share_column` |
| Margin around the sites | 200 m | `habitat: margin_m` |
| Natura 2000 code (`HABITAT`) | A crosswalk from `BIOTOP` will be added later | `TODO(Jonáš)` in `config.yml`; `HABITAT` stays empty |
| X habitats | May be valuable for the species too | `X_SHARE`, the share of a site in X habitats, kept apart from the unmapped area |
| Buffer widths for point sites | None: a point site takes the KVES polygon it lies in, from the KVES version of the record's year | `site_areas()`; KVES 2021, 2022, 2023 and 2025 downloaded; the year → version map is PROPOSED |
| *Sanguisorba* habitat codes | T1.1, T1.4, T1.5, T1.7, T1.10 | `habitat: sanguisorba_meadow_codes`; `SANGUISORBA_AREA`, `SANGUISORBA_SHARE` |

The results are in `CHANGELOG.md`.

**Open, needing domain input**, in addition to the list above:

- the KVES version of each record year: the closest version, and for 2024,
  equally close to 2023 and 2025, the later one (PROPOSED);
- the crosswalk from `BIOTOP` to the Natura 2000 habitat codes;
- a crosswalk between the KVES versions. The versions before 2025 publish
  only the category name, and 13 of their 38 categories (16 in 2021) have no
  2025 category of the same name: the forests, which 2025 splits, the
  shrubs, the urban classes and, in 2021 only, "Hospodářské louky" (managed
  meadows). `KVES_KATEGORIE` holds the name for every record; `KVES_KOD` is
  NA for these (`RETRIEVED.yml`).

## Status after Phase 4 (2026-10-17)

**Done.** The twelve proposals of the literature review (P4.1-P4.12, see
the plan below) are implemented in the `revised` specification, one commit
each, with their numbers in `CHANGELOG.md` and their cell-by-cell diffs in
`changes/c04`-`c12`. New steps: 21 (landscape, common), 22 (habitat and
landscape models), 23 (detection sensitivity), 24 (Natura 2000 and
persistence). Every revised model is now fitted on a third sample, with only
the plausible imputed absences. Legacy is unchanged.

Follow-ups, each in `CHANGELOG.md` with its diff:

- threat classes that were not on the 2019-2020 threat list are unknown
  there, not absent (bug fix, `changes/b09`);
- the matched GLMM without the grid square, and the SPDE convergence
  criteria (`c14`);
- landscape radii up to 8 km (`c15`);
- a new step 25 that collects the key estimates of every analysis into one
  table and one forest plot per species (`c16`).

**What Phase 4 found.**

- **The management results hold.** For *P. nausithous*, appropriate mowing
  timing and patches left uncut each roughly double the odds of recording
  the species (2.16 and 2.18), and together add no more. Abundant host
  plant triples them. This holds:
  - in each of the three samples;
  - on the windows of 28 and 45 days;
  - with connectivity at its best kernel;
  - with the habitat and the landscape;
  - with a spatial random field in place of the grid, and under spatial+;
  - on the occupancy scale of the mixed-design model, where the effects grow
    (timing 6.6), as they must when detection dilutes them.

  Step 25 sets the ten analyses side by side
  (`25_<species>_robustness_summary`). For *P. nausithous*, every interval
  of timing, uncut patches and host plant excludes 1, with one exception:
  uncut patches with the detection fixed at 0.7, 1.66 (0.96-2.88).

  For *P. teleius* no management term is clear in any analysis. Every point
  estimate of timing and uncut patches lies above 1 (plausible sample), and
  only dominant host plant has an interval excluding 1 in most analyses (8
  of 10).
- **Detection (P4.4).** For *P. nausithous* the 224 site-years visited more
  than once identify a detection probability at the median day of 0.78
  (older form) and 0.69 (newer form), the same under a vague prior. A
  detection of 0.5 is incompatible with the data. For *P. teleius* the
  imputed absences behave as non-detections (p 0.3, against 0.8 on the
  observed records).
- **The *P. teleius* imputed absences (P4.3).** Only 37 % lie within 3 km
  of any *P. teleius* record of 2012-2024; the median distance is 5 km. Read
  them with care, and the *P. teleius* results of the sample with all
  imputed absences above all.
- **The Natura 2000 association (P4.8, P4.12).** For *P. nausithous* there
  is none once comparable sites are compared (1.09-1.16).

  For *P. teleius* it is 4.9 unmatched and falls under every analysis that
  accounts for where the sites are:
  - 3.8 in the plausible sample;
  - 2.4 with connectivity;
  - 1.5-1.8 after matching or covariate adjustment;
  - 3.6 with a spatial field.

  The matched GLMM with the grid square as a random effect gives 4.4. Under
  the matching weights, that random effect turns the comparison into one
  within the 19 squares that hold a designated site, a different estimand.
  Without it the matched GLMM gives 2.5, and 1.1 in the plausible sample.

  The EVLs lie where the populations cluster. Populations known in
  2012-2018 persisted about equally inside and outside them.
- **Flight season (P4.1).** The pooled flight curve of the monitoring visits
  is wider than the 6-8 weeks of a single site, so the cut-off at 5 % of
  its peak moves only 12 *P. teleius* records, and the estimates are the
  same on windows of 28 and 45 days. The visits barely sample the early
  season. The quadratic season term fits *P. nausithous* better than the
  flight curve and is kept.
- **Connectivity (P4.2).** The occupancy pattern supports a flat kernel
  (mean distance 1 km for *P. nausithous*, 2 km for *P. teleius*) and no
  area weighting (b = 0). The mark-recapture scales the review proposed
  (80-270 m) fit 12-22 AIC worse: occupancy follows colonisation and
  regional clustering, not daily movement.
- **Landscape (P4.7).** The strongest landscape variables, each on its
  own, are:
  - for *P. nausithous*, forest within 8 km (odds ratio 1.36 per 10
    percentage points) and arable land within 8 km (0.77);
  - for *P. teleius*, arable land within 6 km (0.56) and grassland within
    1 km (1.50).

  When most radii sat at 4 km, the largest tested, the range was extended
  to 8 km. Most *P. nausithous* variables then moved to 8 km, and forest
  still improves there. A scale of effect that wide points to regional
  gradients (forested uplands against arable lowlands) more than to the
  landscape a population draws on. In the combined models arable land is
  absorbed by the other land covers (1.12 and 0.79). The management
  estimates do not move with any radius.
- **Habitat proxies (P4.5, P4.6).** The wet-meadow, X and edge shares show
  nothing. The edge share hardly varies on the small, strip-like monitored
  polygons. The 9 visits noted as freshly mown inside the no-cut window
  all have their timing recorded as inappropriate.
- **Threats (P4.11).** Fertiliser use is the robust association: the
  species is recorded less often where it is noted, and it is noted less
  often where the species was found. It passes Holm in its family.
  Abandonment, the native dominants and the effects of livestock farming
  were not on the 2019-2020 threat list. They are now unknown in those
  years rather than absent, which removed the separation of the stacked
  models.
- **Counts (P4.10) and space (P4.12).** Standardising the counts by the
  flight curve changes no estimate. The spatial field fits better than the
  grid (ΔAIC −13 to −34, range 34-55 km), and spatial+ flags no estimate.
  Five of the six SPDE fits meet the convergence criteria (`18_spde`). The
  sixth stops on a flat optimum with a gradient of 0.002 and the same
  estimates.

**What to report as what.**

- The occupancy estimates are associations with the probability of
  recording the species (Guillera-Arroita et al. 2014). Step 23 bounds how
  far detection could move them.
- Report the Natura 2000 association as an association. Its matched and
  covariate-adjusted versions are the ones to cite.
- For *P. teleius*, the plausible sample is the one to read beside the
  observed one.

**The results as presented** (`CHANGELOG.md`, "The figures and the
descriptive results of the revised analysis"):

- The revised descriptive results (steps 07 and 08) open with the records
  the models use: in the flight season, by outcome (observed and imputed
  negatives, plausible or not), by position in the season, and the share of
  records with the species at each value of the management model's
  variables in the three samples. The legacy figures (the Natura 2000
  designation for either species, the combined management classes) are
  drawn under legacy only.
- One figure style for the common and the revised steps
  (`R/functions/figures.R`):
  - the three samples in a fixed order with fixed marks;
  - species in italics;
  - readable labels for the terms, threat classes and land covers;
  - log axes for odds ratios and distances.
- New figures for the detection sensitivity (step 23) and the Natura 2000
  association and persistence (step 24). The predictions of step 17 combine
  mowing, host plant and EVL in one figure per species.
- Fits with a separated key term are left out of the forest plots and the
  predictions, by the rule of step 25, and the captions say so.
- `REPORT.md` puts the revised specification before the legacy one and
  lists the figures of the revised analysis after the contents.

**Recommendations for the monitoring,** from the review and from what the
analyses could not do:

- A second visit to 5-10 % of the sites each year: it would identify the
  detection directly (von Hirschheydt et al. 2023).
- Record the dates of the cuts. The objective timing rule (no cut 1 July -
  1 September) needs them.
- Record the cover of *Sanguisorba* in per cent, and, where possible,
  Myrmica nests.
- Record explicit *P. teleius* negatives at every visit where it was
  searched for. That would make the imputation unnecessary.

**Open, needing domain input** (PROPOSED or `TODO(Jonáš)` in `config.yml`),
in addition to the lists above:

- the flight-season cut-off at 5 % of the curve, and the season term;
- the connectivity grids, and their choice by profile likelihood;
- the 3 km radius of a plausible imputed absence;
- the detection priors (p around 0.7);
- the wet-meadow codes and the 25 m edge zone;
- the land-cover groups and the radii;
- the matching covariates;
- the Article 17 codes of the threat classes and the confirmatory threats.

---

## Plan

### Phase 2: categories (a) and (b), proceeding unless you object

Each commit is labelled `(a)` or `(b)`. Before the first change,
`Data/Processed/*.csv` and `Outputs/Tables/*.csv` are snapshotted;
`R/tools/compare_snapshots.R` then diffs them after every (a) and (b) commit.
An (a) commit must give an empty diff. A (b) commit gets a `CHANGELOG.md` entry
listing every changed number before and after.

1. **(a) Snapshot and compare tools.**
2. **(a) Configuration.** `config.yml`, read by `R/functions/config.R`:
   - years, sources, CRS and bounding box, paths, seeds;
   - the imputation rule, site-matching distance and buffer widths;
   - the habitat code groups, the threat vocabulary and the `POP_BIOT` mapping;
   - the model switches (`legacy` / `revised`).
   - Every domain choice carries a `TODO(Jonáš)` note.
3. **(a) Reference snapshots.** The layers in `Data/Input/reference/` plus the
   opt-in refresh script (15).
4. **(a) Steps read from disk.** Step 01 writes `records_sites.gpkg` and the
   ranges. `run_all.R` declares each step's inputs and outputs, checks that
   the inputs exist, and stops with a clear message if they do not (16).
5. **(a) Setup.** No installs; check renv and stop clearly (14).
6. **(a) Logic into functions.** Documented functions in `R/functions/`, so
   each step reads, calls and writes:
   - `read_occurrence_csv()`;
   - `impute_absences()`;
   - `protected_area_flags()`;
   - `parse_strukt_pozn()` with the legacy vocabulary;
   - `derive_analysis_variables()`;
   - `select_habitat_segment()`.
7. **(a) Clean-up.**
   - Remove the dead filters and the historical load (7).
   - `https` WFS, `st_crs(5514)`.
   - Remove the leftovers; move the step 13 exports; seeds (17).
8. **(a) Validation.** `checkmate` (installed) at the end of every data step:
   - columns and types;
   - unique keys;
   - no unexpected NA;
   - S-JTSK bounding box;
   - row counts consistent with the previous step.
9. **(a) Tests.** `testthat` on small fixtures that include the edge cases
   found here:
   - the coordinate reader;
   - the `STRUKT_POZN` parser, including `sta_pritomnostrostlin`, `jiný vliv:`
     free text, and threat items with commas;
   - the imputation rules;
   - the protection flags;
   - deduplication;
   - site matching;
   - the segment selection, on a synthetic `sf` fixture.
10. **(a) Style.** `styler`, `lintr` with `.lintr`, and the README.
11. **(b) fixes**, one commit each, each diffed:
    - the coordinate reader (1);
    - deduplication and imputed-row IDs (2);
    - blanked species-specific fields in imputed rows (4, 22);
    - `None` ← `žádný vliv` (5);
    - NA for unknown in `GRAZE`, `OVERALL` and the threats (6);
    - `PRIKOP` (19);
    - comparison sets on common data (9);
    - diagnostics columns (13a).

### Phase 3: category (c), approved and implemented (see "Status after Phase 3")

Each change is a switch in `config.yml`. Results are reported under `legacy`
and `revised` side by side.

- **P3.1 Unit of analysis.** One record per species × site × visit after
  deduplication. Random effects: `(1 | SITMAP) + (1 | YEAR)`, with optional
  `(1 | SITE_ID)` and `(1 | observer)` (3).
- **P3.2 Imputation.** `none` / `legacy_cell` / `visit`. Every model is fitted
  with and without imputed absences (4).
- **P3.3 Detection nuisance.** Day of year, quadratic, centred on the flight
  window (`TODO(Jonáš)`), plus `(1 | observer)` (11).
- **P3.4 Management.** One multivariable model per species: mowing timing ×
  method, grazing, host-plant abundance, site area, and species-specific
  protection, with `PROTOCOL_ERA` (20). Fitted with `glmmTMB`; VIF and
  separation checks; a bias-reduced fallback (`brglm2`, needs installing) for
  separated terms. Marginal predicted probabilities with 95 % CIs from
  `marginaleffects` (needs installing).
- **P3.5 Connectivity.** Sᵢ = Σ exp(−α dᵢⱼ) Aⱼ, over occupied sites of the same
  species in the same or the previous year. α comes from the config with a
  sensitivity range (`TODO(Jonáš)`). Only after fix 1.
- **P3.6 Spatial autocorrelation.** DHARMa on residuals aggregated by site; if
  autocorrelation is present, compare a grid random effect against a smooth.
- **P3.7 Threats.** Per-threat binomial GLMMs of occupancy on each threat class,
  at record level with the same random effects, reported as odds ratios with
  CIs. Recommended over a PCA of binary profiles because it answers the
  question directly and handles NA, era and imputation.
- **P3.8 Abundance (optional).** `POCET` is usable for observed positives
  only: ≥ 1, median 3, up to 500, no zeros. A zero-truncated negative-binomial
  model as a supplement.
- **P3.9 Habitat.** The input contract, `select_habitat_segment()` with the
  area-weighted default and the legacy option, a test on a synthetic fixture,
  and a clean skip when the layer is missing. Marked **not validated on real
  data**.

### Phase 4: the literature review, category (c), implemented (see "Status after Phase 4")

A literature review of the twelve gaps a reviewer would raise against the
revised analysis (methods, parameter values and published precedents; the
report `gaps_prompt.md`) proposed the changes below. They are implemented in
the `revised` specification as switches in `config.yml`; `legacy` stays
unchanged. Values taken from the literature, or chosen here, are marked
PROPOSED.

- **P4.1 Flight curve (gap 3).** A pooled flight curve per species, as the
  butterfly monitoring schemes estimate it (Dennis et al. 2013; Schmucki et
  al. 2016): a negative-binomial GAM of the counts at the targeted visits
  against DOY_DEV, with year and grid-square random intercepts, and an
  elevation interaction when it lowers the AIC. The flight season is cut
  where the curve falls below 5 % of its peak, instead of 45 days from the
  median, which the review found about twice the documented 6-8-week flight
  period. 28 and 45 days as sensitivity windows. The log relative activity
  is offered as the season term instead of the quadratic, and compared with
  it.
- **P4.2 Connectivity (gap 4).** alpha and b chosen per species by profile
  likelihood over alpha in {0.5, 1, 2, 3, 5, 8, 12} per km and b in {0.5,
  1, 1.5}, on the management model. The Czech mark-recapture estimates
  (Nowicki & Vrabec 2011: mean dispersal 80-270 m, alpha 4-12; immigration
  area exponent 0.5-2.5) and the forested landscapes (0.5-1.5 km, alpha
  0.7-2) bracket the grid. b = 1 instead of 0.5 where alpha is fixed.
- **P4.3 Plausible imputed absences (gap 2).** A record of the other species
  shows effort, not absence (Phillips et al. 2009). An imputed absence is
  kept in a third sample only when the visit falls in the imputed species'
  flight season (P4.1) and a positive record of it (2012-2024, any source)
  lies within 3 km (the longest recorded P. teleius move, 2.94 km, Nowicki
  et al. 2014). The 2012-2018 exports are read again for this and for P4.8.
- **P4.4 Detection sensitivity (gap 1).** Not a single-visit occupancy
  model as the headline (Knape & Korner-Nievergelt 2015), but two
  sensitivity analyses: the management model with the detection fixed at
  p = 0.9, 0.7, 0.5; and a Bayesian mixed-design occupancy model of the
  persistent site x year (spOccupancy), in which the site-years visited
  more than once identify the detection, with a weakly informative prior on
  p (0.7) and a vague one (von Hirschheydt et al. 2023; Doser et al. 2024).
- **P4.5 Management coding (gap 5).** An objective timing rule (no cut 1
  July - 1 September, AOPK) needs cut dates, which the form does not record.
  Checked instead: the visits noted as freshly mown in July-August against
  the surveyors' timing. METHOD 1 is "partially mown" (Kajzer-Bonk et al.
  2016); grazing stays in the 2021-2024 model.
- **P4.6 Host ants and host plant (gap 6).** Proxies for Myrmica habitat:
  the share of a polygon site within 25 m of its edge, and the share of wet
  meadow habitats (VMB). Host-plant abundance tested as linear against
  unordered.
- **P4.7 Landscape at several scales (gap 7).** Shares of grassland, arable
  land, forest, water and settlements (KVES 2025) and of natural habitats
  and Sanguisorba meadows (VMB) in circles of 0.2-4 km; the radius of each
  chosen by AIC (Kajzer-Bonk et al. 2016; Jackson & Fahrig 2015).
- **P4.8 Natura 2000 (gap 8).** Full propensity-score matching of the
  records inside an EVL designated for their species on covariates
  designation cannot have changed, and the persistence of the 2012-2018
  populations inside and outside the EVLs (Kajzer-Bonk & Nowicki 2022;
  Pellissier et al. 2020). Reported as associations.
- **P4.9 Temporal dynamics (gap 9).** A dynamic occupancy model needs sites
  revisited across years: 4.8 % are. The persistence analysis of P4.8 is the
  version the data support.
- **P4.10 Counts (gap 10).** A hurdle model on all visits (the zeros of the
  negative visits and the truncated counts of the positive ones), with the
  flight curve as the offset of the counts (Dennis et al. 2016).
- **P4.11 Threats (gap 11).** The threat classes mapped to the Article 17
  pressure codes; one stacked model of all classes; Holm within the threats
  with an expected direction and Benjamini-Hochberg within the others.
- **P4.12 Spatial structure (gap 12).** An SPDE spatial field (sdmTMB) in
  place of the grid random effect, and spatial+ (Dupont et al. 2022) on the
  management and protection terms.

Not taken up, with the reason:

- a two-species occupancy model (gap 2): 15 P. teleius site-years were
  visited more than once, 4 of them with mixed outcomes;
- a single-visit occupancy model (`detect::svocc`, gap 1): the mixed-design
  model covers it, and the review advised against it as the headline;
- NIMBLE (gaps 1 and 9): no compiler on this machine; spOccupancy fits the
  same model class from CRAN binaries.

## Questions

**Q1. Imputation (blocks P3.2).** On a targeted visit where only
*P. nausithous* is recorded, was *P. teleius* searched for?

- If yes, the visit rule adds 1944 *P. teleius* absences.
- If not reliably, I would make `none` (explicit records only) the revised
  default, with `legacy_cell` and `visit` as sensitivity analyses.

**Q2. Records outside the reporting range** (365, all negatives). Keep dropping
them?

**Q3. `{targets}` or `run_all.R`?** Your brief refers to questions that are not
included. I recommend keeping `run_all.R` with declared inputs and outputs and
input checks. It is lighter, needs no new dependency, and the cascade runs in
7 minutes.

**Q4. Git tracking.** I will ignore `Data/Processed` and `Outputs` as you
suggest. Two files cannot be regenerated without the habitat layer:
`data_lokal_vmb.csv` and `lokal_vmb.gpkg`, the legacy habitat intersection, 19
MB together.

- **Option A (proposed).** Move `data_lokal_vmb.csv` to
  `Data/Input/habitats/legacy/`, tracked, so the legacy habitat models stay
  reproducible.
- **Option B.** Drop it.

**Q5. Domain parameters (not blocking Phase 2).** These will appear as
`TODO(Jonáš)` in `config.yml`:

- the flight window;
- the site-matching distance;
- the buffer widths;
- α for connectivity;
- the *Sanguisorba*-meadow habitat codes;
- the mapping of the older threat list;
- the `POP_BIOT` categories;
- the meaning of a missing `Seč` key;
- the candidate sources;
- the out-of-season dates.

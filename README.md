# 💙🦋 phengaris_management

Study of management effects on <i>Phengaris nausithous</i> and <i>Phengaris
teleius</i> in Czechia. It explores how management and habitat use relate for
the two umbrella species, using data from site monitoring between 2019 and
2024.

## Laymen summary

Researching the impacts of habitat management on the dusky large blue
(*Phengaris nausithous*) and the scarce large blue (*Phengaris teleius*) is
crucial for the effective conservation of these endangered butterfly species.

Both depend on specific ecological interactions for their survival, primarily
with the great burnet (*Sanguisorba officinalis*) and certain ant species.
Changes in mowing regimes, grazing intensity, or abandonment of traditional
practices can disrupt these interactions, leading to local population
declines.

Understanding how different management actions influence their occurrence and
persistence helps refine conservation strategies. It supports the long-term
viability of their populations within semi-natural grasslands.

## State of the analysis

The cascade runs two specifications side by side:

- **legacy** keeps the original analysis choices (models, data subsets,
  derived variables) with the bug fixes applied. Its outputs are unchanged by
  the revised analysis.
- **revised** is the analysis approved in the Phase 3 review (`REVIEW.md`,
  P3.1-P3.9):
  - one record per species × site × visit;
  - absences imputed visit by visit, and only inside the imputed species'
    reporting range;
  - explicit negatives outside the range kept;
  - observer, flight-season timing and the field-form era as covariates;
  - multivariable glmmTMB models fitted with and without the imputed
    absences, with diagnostics;
  - connectivity, spatial autocorrelation, per-threat models and an
    abundance supplement.

The models of both specifications are *naive occupancy* models. The
monitoring is essentially one visit per site and year, so detection is not
separated from occupancy (`REVIEW.md`, item 11).

- [`REVIEW.md`](REVIEW.md) is the critical review of the code and the data,
  with what has been resolved.
- [`CHANGELOG.md`](CHANGELOG.md) lists every change that altered a number,
  with before/after values.
- `changes/` holds the full cell-by-cell diffs.

## Running the analysis

```r
renv::restore()              # once: installs the package versions in renv.lock
source("run_all.R")          # everything, in dependency order
```

```sh
Rscript run_all.R            # everything (about 20 minutes)
Rscript run_all.R 07 08 15   # selected steps only
Rscript R/09_models_nausithous.R   # one step on its own
PHENGARIS_SPEC=revised Rscript R/07_describe_tables.R   # under a chosen specification
```

**Requirements.**

- R ≥ 4.6 and the packages in `renv.lock`. `R/00_setup.R` checks for them and
  stops with an instruction when one is missing; it never installs anything
  itself.
- **No internet connection is needed.** The remote reference layers are read
  from the local copies in `Data/Input/reference/` (see *Data* below).

**How the steps run.**

- Every step reads its inputs from disk and runs in a fresh environment.
- A step whose input is missing stops with a message naming the file and the
  step that writes it. Any step can therefore be run on its own once the
  earlier steps have written their outputs.
- The steps and the files each reads and writes are declared in
  `R/functions/pipeline.R`.

## The cascade

| Step | Script | Scope | Reads | Writes |
|---|---|---|---|---|
| 01 | `01_load_source_data.R` | common | occurrence exports, site shapefiles | `records.csv`, `sites.gpkg` |
| 03 | `03_protected_areas.R` | common | 01, reference EVL / MZCHU / Natura 2000 subjects | `protected_area_id.csv` |
| 05 | `05_habitat_intersect.R` | common | 01, habitat layer (optional) | `data_lokal_vmb.csv` |
| 16 | `16_site_design.R` | common | 01 | `site_design.csv`, `flight_window.csv`, `connectivity.csv` |
| 02 | `02_impute_absences.R` | spec | 01, reporting ranges | `data_with_imputed.csv` |
| 04 | `04_clean_occurrence.R` | spec | 02, 03, 16 | `data_clean.csv` |
| 06 | `06_habitat_join.R` | spec | 04, 05 | `data_analysis.csv`, the table every model uses |
| 07 | `07_describe_tables.R` | spec | 06 | descriptive tables |
| 08 | `08_describe_figures.R` | spec | 06 | descriptive figures |
| 09 | `09_models_nausithous.R` | legacy | 06 | occupancy models, *P. nausithous* |
| 10 | `10_models_teleius.R` | legacy | 06 | occupancy models, *P. teleius* |
| 11 | `11_models_both_species.R` | legacy | 06 | both-species models, species comparisons |
| 12 | `12_model_figures.R` | legacy | 06 | figures accompanying the models |
| 13 | `13_threats_ordination.R` | legacy | 06 | the original ordination of threats |
| 17 | `17_occupancy_models.R` | revised | 06 | occupancy models of both species, co-occurrence, species comparison |
| 18 | `18_spatial_autocorrelation.R` | revised | 06 | residual spatial autocorrelation; grid against smooth |
| 19 | `19_threat_models.R` | revised | 06 | occupancy against each threat class |
| 20 | `20_abundance_models.R` | revised | 06 | counts at occupied sites |
| 14 | `14_maps.R` | spec | 01, 06, reference border / grid / relief | maps |
| 15 | `15_compile_report.R` | final | every step report | `Outputs/REPORT.md` |

**Scope.**

- *Common* steps do not depend on the analytical choices. They run once and
  write to `Data/Processed/common` and `Outputs/common`.
- *Spec* steps run once per specification listed in `config.yml`
  (`specifications: run:`). They write to `Data/Processed/<spec>` and
  `Outputs/<spec>`.
- *Legacy* and *revised* steps are spec steps that run only under the
  specifications with that model set (`model_set` in `config.yml`).
- `run_all.R` runs the common steps first, then the spec steps of each
  specification, then step 15.

What each step does:

1. **Load source data.** Parses the occurrence exports with the correct
   decimal mark and joins each record to its site geometry.
2. **Impute absences.** Targeted records of each species inside its own
   reporting range, plus absences imputed from the other species' records,
   under the specification's imputation rule.
3. **Protected areas.** Which sites lie in a Natura 2000 site (EVL), in one
   designated for a Phengaris species, or in a small-scale reserve (MZCHU).
4. **Clean occurrence data.** Derives the analysis variables:
   - occupancy and year;
   - protection;
   - host plant, mowing, grazing and threats, parsed from the structured
     notes `STRUKT_POZN` key by key;
   - habitat type from `POP_BIOT`.

   It then applies the specification's host-plant filter. Unknown values stay
   NA. Under the revised specification it also adds the survey-design
   variables (observer, flight-season timing, field-form era, persistent site,
   site area, protection designated for the record's own species,
   connectivity). It keeps one record per species × site × visit.
5. **Habitat intersection.** Overlays the sites on the habitat mapping layer.
6. **Habitat attributes.** Describes each record by the habitat segments its
   site overlaps, using the specification's segment rule.
7. and 8. Descriptive tables and figures.
9. to 13. *Legacy:* the original models and the ordination.
14. **Maps.**
15. **Compile the report.**
16. **Persistent sites, flight season and connectivity.**
    - Reconstructs persistent sites across years, at 50 m and at 0 and 200 m.
    - Finds the median day of the positive records per species and year.
    - Computes each surveyed site's connectivity to the occupied sites
      around it.
17. to 20. *Revised:*
    - multivariable occupancy models with diagnostics and marginal
      predictions, with co-occurrence;
    - residual spatial autocorrelation;
    - per-threat models;
    - zero-truncated models of the counts.

## Outputs

```
Outputs/
  REPORT.md                 all step reports collected, with a table of contents
  common/  Tables/ Figures/ Reports/     steps 01, 03, 05, 16
  legacy/  Tables/ Figures/ Reports/     steps 02, 04, 06-14 of the legacy specification
           Exports/                      data exports for Excel (step 13)
  revised/ Tables/ Figures/ Reports/     steps 02, 04, 06-08, 14, 17-20 of the revised specification
Data/Processed/
  common/  records.csv, sites.gpkg, protected_area_id.csv, data_lokal_vmb.csv,
           site_design.csv, flight_window.csv, connectivity.csv
  legacy/  data_with_imputed.csv, data_clean.csv, data_analysis.csv
  revised/ data_with_imputed.csv, data_clean.csv, data_analysis.csv
```

Start at `Outputs/REPORT.md`.

**Table and figure files.**

- Every table in a report is also a CSV in `Tables/`, in Windows-1250 for
  Czech Excel. Every figure is a PNG in `Figures/`.
- File names start with the step number.

**What the model tables hold**, per step (`<NN>_<species>_…`):

| Table | Contents |
|---|---|
| `_fit_statistics` | n, AIC, BIC, status of every model |
| `_coefficients` | fixed effects |
| `_random_effects` | variances |
| `_diagnostics` | singular fits, signs of separation, inflated random-effect variances, DHARMa residual tests |
| `_fitting_issues` | warnings and failures |
| `_specifications` | the formulas as fitted |
| `_aic_*` | AIC comparisons. Every model of a comparison is refitted on the same records, so the AIC values are comparable; `n_own_data` gives each model's own n. |

The revised steps (`17_<species>_…` and so on) add:

| Table | Contents |
|---|---|
| `_key_estimates` | odds (or rate) ratios with 95 % CIs of the terms of interest |
| `_imputation_sensitivity` | the management model with and without the imputed absences |
| `_predictions_*` | marginal predicted probabilities (or counts) with 95 % CIs |
| `_gvif` | generalised variance inflation factors |
| `_bias_reduced` | bias-reduced refits (brglm2) of the models flagged for separation |

`Data/Processed` and `Outputs` are not tracked by git; `Rscript run_all.R`
regenerates them.

## Configuration

**Every tunable choice is in [`config.yml`](config.yml):**

- years, species, CRS and the bounding box of Czechia;
- the data sources counted as targeted monitoring;
- the keys of the structured notes, the threat classes and the habitat
  categories;
- the imputation rule, the host-plant filter and the threat vocabulary of
  each specification;
- the habitat input contract;
- the site-matching distance;
- the flight season, the observer aliases and the connectivity parameters;
- the random effects and thresholds of the revised models;
- seeds and diagnostic thresholds;
- file paths;
- the specifications.

Choices that need a decision on the ecology or the field protocol are marked
**`TODO(Jonáš)`**. Values marked **PROPOSED** are used by the revised
specification until they are confirmed. The open ones are:

- which other sources are targeted monitoring;
- the meaning of a missing grazing or mowing entry;
- the mapping of the older (2019-2020) threat list (proposed);
- the observer aliases (proposed);
- the connectivity alpha, 1 per km (proposed);
- the flight-season cut-off, 45 days (proposed);
- the unmapped habitat categories;
- the buffer widths for point and line sites;
- the *Sanguisorba*-meadow habitat codes;
- the mosaic share column.

## Data

```
Data/Input/
  Phengaris_*_2019_2024.csv        AOPK occurrence exports (Windows-1250, ';', '.' decimal mark)
  Phengaris_*_2019_2024/           site geometries (points, polygons, lines); coordinates in S-JTSK
  Reporting_range/                 Article 17 reporting ranges, 10 x 10 km cells
  reference/                       local copies of remote layers, with RETRIEVED.yml
  habitats/legacy/                 the frozen site x habitat intersection of the original analysis
```

**Reference layers.** `Data/Input/reference/` holds copies of the remote
layers the cascade needs:

- EVL, MZCHU and the mapping grid (AOPK WFS);
- the border and the shaded relief (RCzechia);
- the Natura 2000 site-subject table (rn2kcz).

`RETRIEVED.yml` records when and where each copy was fetched. To bring them
up to date, run `R/tools/refresh_reference_layers.R` by hand. Then re-run the
cascade and diff the outputs with `R/tools/compare_snapshots.R`.

**Habitat layer: NOT VALIDATED ON REAL DATA.** The habitat mapping layer is
expected as a GeoPackage at `Data/Input/habitats/habitat_segments.gpkg`. It
must be in EPSG:5514 and have the columns listed in `config.yml`
(`habitat: required_columns`); `read_habitat_layer()` checks this contract.

- The contract and the area-weighted segment rule have been tested only on a
  synthetic fixture (`tests/testthat/test-habitat.R`).
- Until the file is supplied, step 05 tries the AOPK network share, then the
  frozen intersection of the original analysis.
- When neither is available it skips cleanly. The habitat models then report
  themselves as skipped.

## Development

```sh
Rscript R/tools/check_code.R        # tests, styler, lintr
Rscript -e 'testthat::test_dir("tests/testthat")'
```

**Tests.** `tests/testthat` tests every parsing and derivation function on
small fixtures, which contain the edge cases found in the real exports.

**Proving a change.** `R/tools/compare_snapshots.R` snapshots the processed
data and result tables and lists every cell a change alters:

- a refactoring must give an empty diff;
- a bug fix is documented in `CHANGELOG.md`.

**Other tools.** `R/tools/download_habitat_layers.R` (credentials from the
environment variables `AOPK_USER` and `AOPK_PASSWORD`, never in a script)
fetches the habitat layers over HTTP.

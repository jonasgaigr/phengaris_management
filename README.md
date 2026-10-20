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
    abundance supplement;
  - habitat described from the current habitat mapping (VMB) and the
    ecosystem layer KVES (2021-2025), downloaded from data.nature.cz; point
    sites described over the KVES polygon they lie in.
- Phase 4 (`REVIEW.md`, P4.1-P4.12) adds to **revised** what a literature
  review of the reviewers' likely objections proposed:
  - the flight season cut by a flight curve fitted to the counts, with
    fixed windows of 28 and 45 days as a sensitivity check;
  - alpha and b of the connectivity chosen by profile likelihood;
  - a third sample with only the plausible imputed absences, near a
    population known from 2012-2024;
  - detection sensitivity: known detection probabilities and a Bayesian
    mixed-design occupancy model (spOccupancy);
  - habitat proxies for the host ants, the landscape at 0.2-8 km with the
    radius chosen by AIC, and a check of the recorded mowing timing;
  - matching on the propensity score for the Natura 2000 association, and
    the persistence of the 2012-2018 populations;
  - counts standardised by the flight curve and a hurdle model;
  - threats with their Article 17 codes, a stacked model, Holm and
    Benjamini-Hochberg;
  - an SPDE spatial field and spatial+;
  - the key estimates of every analysis side by side (step 25).
- The main findings (step 26) open `Outputs/REPORT.md`: the variables
  significantly associated with each species, how far each holds across
  the samples and the analyses, and what Phase 4 adds, with the figures.

The models of both specifications are *naive occupancy* models. The
monitoring is essentially one visit per site and year, so detection is not
separated from occupancy (`REVIEW.md`, item 11). Step 23 shows how far the
management estimates could owe to that (P4.4).

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
Rscript run_all.R            # everything (about 95 minutes; the MCMC of step 23 takes 40)
Rscript run_all.R 07 08 15   # selected steps only
Rscript R/09_models_nausithous.R   # one step on its own
PHENGARIS_SPEC=revised Rscript R/07_describe_tables.R   # under a chosen specification
```

**Requirements.**

- R ≥ 4.6 and the packages in `renv.lock`. `R/00_setup.R` checks for them and
  stops with an instruction when one is missing; it never installs anything
  itself.
- **No internet connection is needed.** The remote reference layers and the
  habitat layers are read from the local copies in `Data/Input/reference/`
  and `Data/Input/habitats/` (see *Data* below).

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
| 01 | `01_load_source_data.R` | common | occurrence exports 2019-2024 and 2012-2018, site shapefiles | `records.csv`, `sites.gpkg`, `records_2012_2018.csv`, `sites_2012_2018.gpkg` |
| 03 | `03_protected_areas.R` | common | 01, reference EVL / MZCHU / Natura 2000 subjects | `protected_area_id.csv` |
| 05 | `05_habitat_intersect.R` | common | 01, frozen intersection, VMB and KVES copies (each optional) | `data_lokal_vmb.csv`, `data_lokal_vmb_current.csv`, `data_lokal_kves.csv`, `site_areas.gpkg`, `site_area_shape.csv` |
| 16 | `16_site_design.R` | common | 01, elevation model (optional) | `site_design.csv`, `flight_window.csv`, `flight_curve.csv`, `connectivity.csv`, `site_history.csv` |
| 21 | `21_landscape.R` | common | 01, 16, landscape rasters (optional) | `landscape.csv` |
| 02 | `02_impute_absences.R` | spec | 01, reporting ranges | `data_with_imputed.csv` |
| 04 | `04_clean_occurrence.R` | spec | 02, 03, 16, 21 | `data_clean.csv` |
| 06 | `06_habitat_join.R` | spec | 04, 05 | `data_analysis.csv`, the table every model uses |
| 07 | `07_describe_tables.R` | spec | 06 | descriptive tables |
| 08 | `08_describe_figures.R` | spec | 06 | descriptive figures |
| 09 | `09_models_nausithous.R` | legacy | 06 | occupancy models, *P. nausithous* |
| 10 | `10_models_teleius.R` | legacy | 06 | occupancy models, *P. teleius* |
| 11 | `11_models_both_species.R` | legacy | 06 | both-species models, species comparisons |
| 12 | `12_model_figures.R` | legacy | 06 | figures accompanying the models |
| 13 | `13_threats_ordination.R` | legacy | 06 | the original ordination of threats |
| 17 | `17_occupancy_models.R` | revised | 06, 16 | occupancy models of both species, connectivity profile, window sensitivity, co-occurrence, species comparison; `connectivity_selected.csv` |
| 18 | `18_spatial_autocorrelation.R` | revised | 06 | residual spatial autocorrelation; grid against smooth and SPDE field; spatial+ |
| 19 | `19_threat_models.R` | revised | 06 | occupancy against each threat class; stacked threat model |
| 20 | `20_abundance_models.R` | revised | 06 | counts at occupied sites; hurdle model with the flight-curve offset |
| 22 | `22_habitat_models.R` | revised | 06 | mowing-timing check, host plant, local habitat, landscape scales |
| 23 | `23_detection_models.R` | revised | 06 | known detection, mixed-design occupancy model |
| 24 | `24_natura2000.R` | revised | 06, 17 | matching for the Natura 2000 association, persistence of the 2012-2018 populations |
| 25 | `25_synthesis.R` | revised | the result tables of 17, 18, 22-24 | the key odds ratios of every analysis side by side, forest plots |
| 26 | `26_main_findings.R` | revised | the result tables and figures of 04, 17-25 | the variables associated with each species, the main findings |
| 14 | `14_maps.R` | spec | 01, 06, reference border / grid / relief | maps |
| 15 | `15_compile_report.R` | final | every step report | `Outputs/REPORT.md`, the main findings first |

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
5. **Habitat intersection.** Copies the frozen site × segment intersection of
   the original analysis, and overlays the sites on the current habitat
   mapping (VMB) and on KVES. It checks the current intersection against
   the frozen one.
6. **Habitat attributes.** Describes each record by the habitat segments its
   site overlaps, using the specification's habitat source and segment rule.
   With the current layers it adds the site's dominant KVES class.
7. and 8. **Descriptive tables and figures.** Under *revised* they open
   with the records as the models use them: in the flight season, by
   outcome (positive, observed negative, imputed negative, plausible or
   not), by position in the season, and the share of records with the
   species at each value of the management model's variables in the three
   samples. The figures of the original analysis (the Natura 2000
   designation for either species, the combined management classes) are
   drawn under *legacy* only.
9. to 13. *Legacy:* the original models and the ordination.
14. **Maps.**
15. **Compile the report.** The main findings (step 26) first, then the
    common steps, then the specifications with the revised model set, then
    the others, with a list of the figures of the revised analysis at the
    top.
16. **Persistent sites, flight season and connectivity.**
    - Reconstructs persistent sites across years, at 50 m and at 0 and 200 m,
      and gives each site its elevation.
    - Finds the median day of the positive records per species and year, and
      fits the flight curve of each species (P4.1).
    - Computes each surveyed site's connectivity to the occupied sites
      around it, at every alpha x b pair of the profile (P4.2).
    - Finds the populations known around each site: the nearest positive
      record of 2012-2024, and a record of 2012-2018 at the site (P4.3, P4.8).
17. to 20. *Revised:*
    - multivariable occupancy models with diagnostics and marginal
      predictions, the connectivity profile, the window sensitivity and
      co-occurrence;
    - residual spatial autocorrelation, an SPDE spatial field and spatial+;
    - per-threat models and a stacked threat model;
    - zero-truncated models of the counts and a hurdle model.
21. **The landscape around the sites** (P4.7): shares of grassland, arable
    land, forest, water and settlements (KVES 2025) and of natural habitats
    and *Sanguisorba* meadows (VMB) in circles of 0.2-8 km.
22. to 24. *Revised:*
    - habitat and landscape models, with the radius of each landscape
      variable chosen by AIC, and a check of the recorded mowing timing
      (P4.5-P4.7);
    - detection sensitivity (P4.4);
    - the Natura 2000 association after matching, and the persistence of the
      2012-2018 populations (P4.8, P4.9).
25. **Robustness of the key estimates:** the odds ratios of the management
    terms, the host plant and the EVL from every analysis of steps 17-24,
    in one table and one forest plot per species. Nothing is refitted.
26. **Main findings:** every variable of the models, each read from the
    model it is the subject of, in the three samples. Each species is read
    from one sample (`synthesis: primary_sample`): with all imputed absences
    for *P. nausithous*, with the plausible ones for *P. teleius*. For each
    variable significant there (p < 0.05; the threat classes after Holm or
    Benjamini-Hochberg), the step gives:
    - in how many samples it is significant (a fit flagged for possible
      separation is not counted);
    - in how many analyses its interval excludes 1.

    The step then states, in numbers, what the detection, imputation,
    Natura 2000, connectivity, spatial, threat, count and mowing analyses
    add, with their figures. Step 15 puts it at the top of the report.
    Nothing is refitted.

## Outputs

```
Outputs/
  REPORT.md                 all step reports collected, with a table of contents
  REPORT.pdf, REPORT.html   the same, rendered by R/tools/render_report_pdf.R
  common/  Tables/ Figures/ Reports/     steps 01, 03, 05, 16, 21
  legacy/  Tables/ Figures/ Reports/     steps 02, 04, 06-14 of the legacy specification
           Exports/                      data exports for Excel (step 13)
  revised/ Tables/ Figures/ Reports/     steps 02, 04, 06-08, 14, 17-20, 22-26 of the revised specification
Data/Processed/
  common/  records.csv, sites.gpkg, records_2012_2018.csv, sites_2012_2018.gpkg,
           protected_area_id.csv,
           data_lokal_vmb.csv (frozen), data_lokal_vmb_current.csv, data_lokal_kves.csv,
           site_areas.gpkg, site_area_shape.csv,
           site_design.csv, flight_window.csv, flight_curve.csv, connectivity.csv,
           site_history.csv, landscape.csv
  legacy/  data_with_imputed.csv, data_clean.csv, data_analysis.csv
  revised/ data_with_imputed.csv, data_clean.csv, data_analysis.csv,
           connectivity_selected.csv
```

Start at `Outputs/REPORT.md`: it opens with the main findings (step 26).
For a PDF of it (A4, page numbers, bookmarks, figures at print size), run
`Rscript R/tools/render_report_pdf.R` after step 15. It needs Chrome, Edge
or Chromium (or `CHROME_PATH`), which the cascade itself does not.

**Table and figure files.**

- Every table in a report is also a CSV in `Tables/`, in Windows-1250 for
  Czech Excel. Every figure is a PNG in `Figures/`.
- File names start with the step number.
- Every figure, of either specification, shares one palette, theme and
  print size (`R/functions/plot_style.R`); the plot functions of the common
  steps and the revised specification are in `R/functions/figures.R`. The
  palette is sampled from photographs of *P. teleius*
  ([`changes/figures_style.md`](changes/figures_style.md)): *P. teleius*
  in its lilac blue, *P. nausithous* in a cinnamon brown, records with the
  species in deep blue and records without it in the buff of the underside.
  Figures are written at journal column widths (8.5 or 17.5 cm) with
  8-9 pt type.
- The three samples always appear in the same order and with the same
  marks: near-black circles with all imputed absences, deep-blue triangles
  with the plausible ones, lilac-blue squares on the observed records only.
  Odds ratios and distances are on log scales. Each figure plots a table
  its step also writes.
- Fits without a finite interval, or whose interval spans four orders of
  magnitude or more (separated terms, `synthesis: max_interval_ratio`), are
  left out of the forest plots and the predictions, never out of the
  tables. The captions say so.

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
| `_imputation_sensitivity` | the management model with all, with the plausible and without the imputed absences |
| `_predictions_*` | marginal predicted probabilities (or counts) with 95 % CIs |
| `_gvif` | generalised variance inflation factors |
| `_bias_reduced` | bias-reduced refits (brglm2) of the models flagged for separation |
| `_connectivity_profile` | the management model at every alpha x b of the connectivity (step 17) |
| `_window_sensitivity` | the management model on fixed windows of 28 and 45 days (step 17) |
| `_known_detection`, `_occupancy_*`, `_detection_probability` | the detection sensitivity (step 23) |
| `_significant_variables`, `_variables`, `_landscape` | the variables significant in the species' sample, every variable, and the landscape variables alone and combined (step 26) |

Every revised model is fitted on three samples: with the imputed absences
(ids without a suffix), with only the plausible ones (`_pls`) and on the
observed records only (`_obs`).

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
- the habitat input contract, the habitat source of each specification and
  the download of the VMB and KVES layers;
- the site-matching distance;
- the flight season, the observer aliases and the connectivity parameters;
- the random effects and thresholds of the revised models;
- the sample each species' results are read from, and the significance
  level of the main findings;
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
- the unmapped habitat categories;
- the KVES version of each record year, the closest one (proposed);
- the crosswalk from the habitat codes to the Natura 2000 habitat codes;
- from Phase 4 (proposed): the flight-season cut-off at 5 % of the flight
  curve, the season term, the connectivity grids and their choice by
  profile likelihood, the 3 km radius of a plausible imputed absence, the
  detection priors, the wet-meadow codes and the 25 m edge, the land-cover
  groups and radii, the matching covariates, the Article 17 codes of the
  threat classes and the confirmatory threats.

## Data

```
Data/Input/
  Phengaris_*_2019_2024.csv        AOPK occurrence exports (Windows-1250, ';', '.' decimal mark)
  Phengaris_*_2019_2024/           site geometries (points, polygons, lines); coordinates in S-JTSK
  Phengaris_*_2012_2018.csv, Phengaris_*_2012_2018/
                                   the earlier exports and their sites: where the species were
                                   known before (P4.3, P4.8); no model is fitted on them
  landscape/                       KVES land cover and VMB shares at 25 m and 100 m, for all of
                                   Czechia (not tracked; made by build_landscape_layers()), with BUILT.yml
  Reporting_range/                 Article 17 reporting ranges, 10 x 10 km cells
  reference/                       local copies of remote layers, with RETRIEVED.yml
  habitats/legacy/                 the frozen site x habitat intersection of the original analysis
  habitats/habitat_segments.gpkg   VMB near the sites (not tracked; made by the download tool)
  habitats/kves_<version>.gpkg     KVES 2021, 2022, 2023, 2025 near the sites (not tracked; made by the download tool)
  habitats/RETRIEVED.yml           the archives the layers were made from, with their md5
Data/Cache/aopk/                   the VMB and KVES kraj archives and per-kraj extracts (not tracked, ~20 GB)
```

**Reference layers.** `Data/Input/reference/` holds copies of the remote
layers the cascade needs:

- EVL, MZCHU and the mapping grid (AOPK WFS);
- the border, the shaded relief and the elevation model (RCzechia);
- the Natura 2000 site-subject table (rn2kcz).

`RETRIEVED.yml` records when and where each copy was fetched. To bring them
up to date, run `R/tools/refresh_reference_layers.R` by hand
(`refresh_reference_layers(layers = ...)` for some of them only). Then
re-run the cascade and diff the outputs with `R/tools/compare_snapshots.R`.

**Landscape rasters.** The landscape around a site reaches 8 km, beyond the
200 m the habitat copies keep. `R/tools/build_landscape_layers.R`
rasterises the KVES 2025 and VMB kraj archives of the cache for all of
Czechia (about 30 minutes; needs the cache of the download tool):

```r
source("R/00_setup.R")
source("R/tools/download_habitat_layers.R")
source("R/tools/build_landscape_layers.R")
build_landscape_layers()
```

Without the rasters, step 21 writes no landscape table and step 22 skips
the landscape models.

**Habitat layers.** The habitat mapping (VMB, data.nature.cz ds/21) and the
ecosystem layer KVES of AOPK ČR in its versions 2021, 2022, 2023 and 2025
(ds/101, 102, 108, 145), CC BY 4.0. They are too large for git, so they are
made by a tool, following the alcon-spatial project:

```r
source("R/00_setup.R")
source("R/tools/download_habitat_layers.R")
download_habitat_layers()    # about 2 hours the first time; needs step 01
```

- It needs an ISOP login. The NDOP account is one: `NDOP_USER` and `NDOP_PWD`
  in `~/.Renviron`, never in a script.
- It downloads the archives of the kraje within 200 m of a site (all 14 at
  present: 0.6 GB VMB, about 4 GB per KVES version) into `Data/Cache/aopk/`.
  They are not downloaded again unless `refresh = TRUE`.
- It keeps the features within 200 m of a site (`habitat: margin_m`) and cuts
  them to that area. It puts the VMB segments cut by kraj lines back
  together.
- It writes `habitat_segments.gpkg` (one row per segment × mapped habitat,
  the input contract of `config.yml` `habitat: required_columns`), one
  `kves_<version>.gpkg` per KVES version, and `RETRIEVED.yml`.
- The published VMB has no Natura 2000 habitat code, so `HABITAT` is empty
  until a crosswalk is added. `FSB_EVAL` is derived: X for a mapped segment
  less than half natural. This is the network share's rule without two
  artefacts of its table joins (`CHANGELOG.md`).
- KVES before 2025 publishes only the category name. Its code is the 2025
  code of the category of the same name; `RETRIEVED.yml` lists the categories
  left without one.

**How the current layers describe a site.**

- A record takes its KVES information from the KVES version closest to its
  year (`habitat: kves_version_of_year`): 2019-2021 from 2021, 2022 from
  2022, 2023 from 2023, 2024 from 2025.
- A polygon site is described over its own area. A point or line site is
  described over the KVES polygon it lies in, from that version, cut to
  200 m around it (`site_areas()`; `Data/Processed/common/site_areas.gpkg`).
- Over that area: the habitat shares from VMB, each weighted by `STEJ_PR`,
  including the shares of natural habitats, of X habitats (kept apart from
  the unmapped area) and of the *Sanguisorba* meadows (T1.1, T1.4, T1.5,
  T1.7, T1.10); and the dominant KVES class.

**Which habitat layer a specification uses** (`habitat_source` in
`config.yml`):

- *legacy* uses the frozen intersection of the original analysis, so its
  outputs stay unchanged.
- *revised* uses the current VMB and KVES layers. Step 05 checks the current
  intersection against the frozen one on the polygon sites
  (`05_current_vs_frozen`).
- Without the VMB copy, step 05 tries the AOPK network share. Failing that,
  *revised* falls back to the frozen intersection with a warning in its
  report. Without a KVES copy, the point and line sites of its years stay as
  they are, with no area.

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

**Other tools.** `R/tools/download_habitat_layers.R` downloads the VMB and
KVES layers (see *Data*); `R/tools/build_landscape_layers.R` builds the
landscape rasters from them; `R/tools/refresh_reference_layers.R` fetches
the reference layers; `R/tools/render_report_pdf.R` renders
`Outputs/REPORT.md` as `Outputs/REPORT.pdf`.

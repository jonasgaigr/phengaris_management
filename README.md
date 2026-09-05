# 💙🦋 phengaris_management 
Study of management effects on <i>Phengaris nausithous</i> and <i>Phengaris teleius</i> in Czechia. Exploring the relationship between management and habitat utilisation of the two umbrella species using data from site monitoring between 2019 and 2024.
## Laymen summary
Researching the impacts of habitat management on the dusky large blue (*Phengaris nausithous*) and the scarce large blue (*Phengaris teleius*) is crucial for the effective conservation of these endangered butterfly species. Both depend on specific ecological interactions – primarily with the great burnet (*Sanguisorba officinalis*) and certain ant species – for their survival. Changes in mowing regimes, grazing intensity, or abandonment of traditional practices can disrupt these interactions, leading to local population declines. Understanding how different management actions influence their occurrence and persistence helps refine conservation strategies and supports the long-term viability of their populations within semi-natural grasslands.

## Running the analysis

```r
source("run_all.R")          # everything, in dependency order
```

```sh
Rscript run_all.R            # everything
Rscript run_all.R 07 08 15   # selected steps only
```

`R/00_setup.R` always runs first: it installs and loads the packages, sources the
helper functions and defines the project constants. All steps share one session,
because steps 01 and 14 exchange spatial objects that are expensive to rebuild.
Every step from 02 onwards reads its inputs from `Data/Processed`, so re-running
a later step on its own is safe once the earlier outputs exist.

### The cascade

| Step | Script | Produces |
|---|---|---|
| 01 | `01_load_source_data.R` | `lokal_new.gpkg`, spatial objects in the session |
| 02 | `02_impute_absences.R` | `data_with_imputed.csv` |
| 03 | `03_protected_areas.R` | `protected_area_id.csv` |
| 04 | `04_clean_occurrence.R` | `data_clean.csv` |
| 05 | `05_habitat_intersect.R` | `lokal_vmb.gpkg`, `data_lokal_vmb.csv` |
| 06 | `06_habitat_join.R` | `data_analysis.csv` — the table the models use |
| 07 | `07_describe_tables.R` | descriptive result tables |
| 08 | `08_describe_figures.R` | descriptive figures |
| 09 | `09_models_nausithous.R` | occupancy models, *P. nausithous* |
| 10 | `10_models_teleius.R` | occupancy models, *P. teleius* |
| 11 | `11_models_both_species.R` | both-species models and species comparisons |
| 12 | `12_model_figures.R` | figures accompanying the models |
| 13 | `13_threats_ordination.R` | PCA and RDA of threats and pressures |
| 14 | `14_maps.R` | distribution maps |
| 15 | `15_compile_report.R` | `Outputs/REPORT.md` |

### Outputs

Every step writes three things, and they cannot drift apart because the same
helper call produces all of them:

```
Outputs/
  Tables/     one CSV per result table (Windows-1250, for Czech Excel)
  Figures/    one PNG per figure
  Reports/    one Markdown report per step, with tables and figures inline
  REPORT.md   all step reports collected, with a table of contents
```

Start at `Outputs/REPORT.md`.

### Supporting code

```
R/functions/    io_helpers, report_helpers, model_helpers,
                summary_tables, habitat_layers (load_vmb)
R/tools/        download_habitat_layers.R — opt-in, not part of the cascade
```

### Requirements

* Internet access, for the AOPK open-data WFS layers and `RCzechia`.
* Access to the AOPK network share `//bali.nature.cz` for step 05. Without it,
  steps 05 and 06 skip the habitat attributes, the models that need
  `AREA_SITE`, `FSB`, `BIOTOP` or `HET_OUT` report themselves as skipped, and
  the rest of the cascade still runs to completion.
* `R/tools/download_habitat_layers.R` reads credentials from the environment
  variables `AOPK_USER` and `AOPK_PASSWORD`. Put them in `~/.Renviron`; never
  in a script.

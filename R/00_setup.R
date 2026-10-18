#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#            Step 00 - Setup and constants
#
#
#                   Jonáš Gaigr
#                       2025
#
#----------------------------------------------------------#
#
# Loads the packages, the helper functions and the project-wide constants that
# every later step relies on. It touches no data and writes no results.
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# Packages -----
#----------------------------------------------------------#

# The packages are installed by renv, at the versions recorded in renv.lock:
#
#   renv::restore()
#
# Setup never installs anything itself. It checks that every package the
# cascade calls is there and stops with that instruction when one is not.
#
# The analysis calls everything namespace-qualified (`dplyr::filter()`), so
# this list is what the cascade needs *installed*. The packages that only the
# opt-in tools in R/tools use (RCzechia, rn2kcz, httr, zip) are not checked
# here. Keep the list in step with the code.
pkgs <- c(
  "magrittr", "dplyr", "tidyr", "readr", "tibble", "stringr", "stringi",
  "forcats", "ggplot2", "ggrepel", "ggpubr", "sf", "terra", "units", "vegan",
  "lme4", "Matrix", "reformulas", "DHARMa", "yaml", "checkmate",
  # The revised model set (steps 17-20).
  "glmmTMB", "brglm2", "detectseparation", "marginaleffects", "mgcv",
  # Phase 4: the mixed-design occupancy model (step 23), the matching
  # (step 24) and the SPDE spatial field (step 18).
  "spOccupancy", "MatchIt", "optmatch", "sandwich", "sdmTMB"
)

missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop(
    "Missing package(s): ", paste(missing_pkgs, collapse = ", "), ".\n",
    "Run renv::restore() in the project root to install the versions ",
    "recorded in renv.lock.",
    call. = FALSE
  )
}

# `%>%` is the one function the analysis uses bare, so magrittr is the one
# package that has to be on the search path.
suppressPackageStartupMessages(library(magrittr))

# lme4 and Matrix have to be built against each other. A mismatch shows up as
# a cryptic error deep inside the model fitting, so it is cheaper to catch it
# here on a two-line example data set.
tryCatch(
  invisible(suppressMessages(lme4::glmer(
    cbind(incidence, size - incidence) ~ 1 + (1 | herd),
    data = lme4::cbpp, family = binomial
  ))),
  error = function(e) {
    stop(
      "lme4 cannot fit a model: ", conditionMessage(e), "\n",
      "lme4 and Matrix are probably out of step. Run renv::restore() to ",
      "reinstall the versions recorded in renv.lock.",
      call. = FALSE
    )
  }
)

#----------------------------------------------------------#
# Configuration -----
#----------------------------------------------------------#

# Every tunable choice lives in config.yml; the constants below are read
# from it.
source("R/functions/config.R", encoding = "UTF-8")
CONFIG <- load_config("config.yml")

#----------------------------------------------------------#
# Helper functions -----
#----------------------------------------------------------#

source("R/functions/pipeline.R", encoding = "UTF-8")
source("R/functions/io_helpers.R", encoding = "UTF-8")
source("R/functions/plot_style.R", encoding = "UTF-8")
source("R/functions/read_sources.R", encoding = "UTF-8")
source("R/functions/validate.R", encoding = "UTF-8")
source("R/functions/imputation.R", encoding = "UTF-8")
source("R/functions/protection.R", encoding = "UTF-8")
source("R/functions/sites.R", encoding = "UTF-8")
source("R/functions/connectivity.R", encoding = "UTF-8")
source("R/functions/strukt_pozn.R", encoding = "UTF-8")
source("R/functions/derive_variables.R", encoding = "UTF-8")
source("R/functions/design_variables.R", encoding = "UTF-8")
source("R/functions/flight_curve.R", encoding = "UTF-8")
source("R/functions/history.R", encoding = "UTF-8")
source("R/functions/detection.R", encoding = "UTF-8")
source("R/functions/landscape.R", encoding = "UTF-8")
source("R/functions/habitat_models.R", encoding = "UTF-8")
source("R/functions/natura2000.R", encoding = "UTF-8")
source("R/functions/threats.R", encoding = "UTF-8")
source("R/functions/spatial.R", encoding = "UTF-8")
source("R/functions/synthesis.R", encoding = "UTF-8")
source("R/functions/report_helpers.R", encoding = "UTF-8")
source("R/functions/model_helpers.R", encoding = "UTF-8")
source("R/functions/model_specs_legacy.R", encoding = "UTF-8")
source("R/functions/model_revised.R", encoding = "UTF-8")
source("R/functions/model_specs_revised.R", encoding = "UTF-8")
source("R/functions/ordination_legacy.R", encoding = "UTF-8")
source("R/functions/summary_tables.R", encoding = "UTF-8")
source("R/functions/figures.R", encoding = "UTF-8")
source("R/functions/habitat_layers.R", encoding = "UTF-8")
source("R/functions/habitat_build.R", encoding = "UTF-8")

#----------------------------------------------------------#
# Project constants -----
#----------------------------------------------------------#
#--------------------------------------------------#
## Coordinate reference system -----
#--------------------------------------------------#

# S-JTSK / Krovak East North, the national CRS all layers are aligned to.
CRS_SJTSK <- CONFIG$crs$epsg

#--------------------------------------------------#
## Species -----
#--------------------------------------------------#

SPECIES_NAU <- CONFIG$species$nausithous$name
SPECIES_TEL <- CONFIG$species$teleius$name

#--------------------------------------------------#
## Data sources counted as targeted monitoring -----
#--------------------------------------------------#

# Only records from these monitoring campaigns enter the analysis. Everything
# else is opportunistic recording, for which absences cannot be inferred.
target_mon_zdroj <- CONFIG$sources$targeted

#--------------------------------------------------#
## Shared plotting style -----
#--------------------------------------------------#

# The palette, the themes, the occupancy scales and the figure sizes are in
# R/functions/plot_style.R.

#----------------------------------------------------------#
# End setup -----
#----------------------------------------------------------#

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
  "magrittr", "dplyr", "tidyr", "readr", "tibble", "stringr", "forcats",
  "ggplot2", "ggrepel", "ggpubr", "sf", "terra", "units", "vegan", "lme4",
  "Matrix", "yaml", "checkmate"
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
source("R/functions/read_sources.R", encoding = "UTF-8")
source("R/functions/report_helpers.R", encoding = "UTF-8")
source("R/functions/model_helpers.R", encoding = "UTF-8")
source("R/functions/summary_tables.R", encoding = "UTF-8")
source("R/functions/habitat_layers.R", encoding = "UTF-8")

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

# Site occupancy is shown with the same two greys throughout, so that the
# figures read as one set in the manuscript.
OCCUPANCY_VALUES <- c("grey", "#595959")
OCCUPANCY_LABELS <- c("negative", "positive")

scale_fill_occupancy <- function(name = "site occupancy") {
  ggplot2::scale_fill_manual(
    labels = OCCUPANCY_LABELS,
    name   = name,
    values = OCCUPANCY_VALUES
  )
}

scale_colour_occupancy <- function(name = "site occupancy") {
  ggplot2::scale_color_manual(
    labels = OCCUPANCY_LABELS,
    name   = name,
    values = OCCUPANCY_VALUES
  )
}

# Bar charts start at zero and get a little headroom above the tallest bar.
scale_y_count <- function() {
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))
}

#----------------------------------------------------------#
# End setup -----
#----------------------------------------------------------#

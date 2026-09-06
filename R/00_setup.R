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

# The analysis calls everything namespace-qualified (`dplyr::filter()`), so this
# list is what the project needs *installed*; install_if_missing() attaches each
# one as a health check on the install. The bare `%>%` used throughout is the one
# thing that genuinely has to be attached, which magrittr provides.
#
# Keep the list in step with the code. renv reads the `pkg::` calls to decide what
# is used, so a package left here after its last call site is gone is flagged by
# renv::status() and drags its whole dependency tree into renv.lock with it.
pkgs <- c(
  "magrittr", "dplyr", "tidyr", "readr", "tibble", "stringr", "forcats",
  "ggplot2", "ggrepel", "ggpubr", "sf", "terra", "units", "RCzechia",
  "vegan", "lme4", "Matrix", "httr", "remotes"
)

pkg_type <- if (.Platform$OS.type == "windows") "binary" else "source"

#' Install a package if it is missing, and repair a broken installation.
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE, type = pkg_type)
  }
  ok <- tryCatch(
    {
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      TRUE
    },
    error = function(e) FALSE
  )
  if (!ok) {
    message("Broken install detected for ", pkg, " -> reinstalling (", pkg_type, ").")
    try(remove.packages(pkg), silent = TRUE)
    install.packages(pkg, dependencies = TRUE, type = pkg_type)
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}

invisible(lapply(pkgs, install_if_missing))

# `%>%` is the one function the analysis uses bare, so magrittr is the one package
# that has to be on the search path. The attach is spelled out here rather than
# left to the loop above so that renv sees it: renv reads `library()` calls but
# not the character vector, and without this magrittr would be recorded only as
# an incidental dependency of dplyr.
suppressPackageStartupMessages(library(magrittr))

# rn2kcz is the one dependency that only lives on GitHub, so renv records it from
# the GitHub remote rather than from CRAN.
if (!requireNamespace("rn2kcz", quietly = TRUE)) remotes::install_github("jonasgaigr/rn2kcz")
suppressPackageStartupMessages(library(rn2kcz))

# lme4 and Matrix have to be built against each other. A mismatch shows up as a
# cryptic error deep inside the model fitting, so it is cheaper to catch it here
# on a two-line example data set.
invisible(tryCatch(
  {
    suppressMessages(lme4::glmer(
      cbind(incidence, size - incidence) ~ 1 + (1 | herd),
      data = lme4::cbpp, family = binomial
    ))
    TRUE
  },
  error = function(e) {
    message("lme4 check failed: ", conditionMessage(e),
            "\nReinstalling Matrix + lme4 (", pkg_type, ")...")
    try(remove.packages(c("lme4", "Matrix")), silent = TRUE)
    install.packages(c("Matrix", "lme4"), dependencies = TRUE, type = pkg_type)
    suppressPackageStartupMessages(library(Matrix))
    suppressPackageStartupMessages(library(lme4))
    TRUE
  }
))

message("Packages installed and loaded (binary on Windows: ", pkg_type == "binary", ")")

#----------------------------------------------------------#
# Helper functions -----
#----------------------------------------------------------#

source("R/functions/io_helpers.R", encoding = "UTF-8")
source("R/functions/report_helpers.R", encoding = "UTF-8")
source("R/functions/model_helpers.R", encoding = "UTF-8")
source("R/functions/summary_tables.R", encoding = "UTF-8")
source("R/functions/habitat_layers.R", encoding = "UTF-8")

init_output_dirs()

#----------------------------------------------------------#
# Project constants -----
#----------------------------------------------------------#
#--------------------------------------------------#
## Coordinate reference system -----
#--------------------------------------------------#

# S-JTSK / Krovak East North, the national CRS all layers are aligned to.
CRS_SJTSK <- 5514

#--------------------------------------------------#
## Species -----
#--------------------------------------------------#

SPECIES_NAU <- "Phengaris nausithous"
SPECIES_TEL <- "Phengaris teleius"

#--------------------------------------------------#
## Data sources counted as targeted monitoring -----
#--------------------------------------------------#

# Only records from these monitoring campaigns enter the analysis. Everything
# else is opportunistic recording, for which absences cannot be inferred.
target_mon_zdroj <- c(
  "Kolektiv autorů (2017) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2018) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2019) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2020) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2021) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2020) Monitoring motýlů. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2021) Monitoring motýlů. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2022) Monitoring motýlů.",
  "Kolektiv autorů (2023) Monitoring motýlů.",
  "Kolektiv autorů (2024) Monitoring motýlů."
)

#--------------------------------------------------#
## Years excluded from the analysis -----
#--------------------------------------------------#

# Coverage in these years is too thin to contribute to the occupancy models.
EXCLUDED_YEARS <- c("2014", "2017")

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

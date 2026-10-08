#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#              Run the whole analysis
#
#
#                   Jonáš Gaigr
#                       2025
#
#----------------------------------------------------------#
#
# Runs every step of the cascade in dependency order.
#
#   From RStudio      source("run_all.R")
#   From the shell    Rscript run_all.R
#   Selected steps    Rscript run_all.R 07 08 15
#
# The steps, and the files each reads and writes, are declared in
# R/functions/pipeline.R. The order is:
#
#   1. the steps that do not depend on the analytical choices (01, 03, 05),
#      once;
#   2. the steps that do (02, 04, 06-14), once for each specification listed
#      in config.yml under `specifications: run:`;
#   3. the compiled report (15).
#
# Every step runs in its own environment and reads its inputs from disk, so
# nothing passes between steps through the session. A step whose inputs are
# missing stops with a message that names the file and the step writing it.
#
# Each step writes its tables to Outputs/<scope>/Tables, its figures to
# Outputs/<scope>/Figures and a Markdown report to Outputs/<scope>/Reports,
# where <scope> is "common" or the specification. Step 15 collects the
# reports into Outputs/REPORT.md.
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# Setup -----
#----------------------------------------------------------#

# The encoding is given explicitly because the monitoring source names and the
# habitat search patterns are Czech, and the files are UTF-8 regardless of the
# locale R happens to start in.
source("R/00_setup.R", encoding = "UTF-8")

#----------------------------------------------------------#
# Select the steps to run -----
#----------------------------------------------------------#

requested <- commandArgs(trailingOnly = TRUE)

# Interactive use: set run_steps before sourcing to run a subset,
# e.g. run_steps <- c("07", "08", "15")
if (!length(requested) && exists("run_steps")) {
  requested <- as.character(run_steps)
}

all_ids <- vapply(PIPELINE, function(s) s$id, character(1))
unknown <- setdiff(requested, all_ids)
if (length(unknown)) {
  stop("Unknown step id(s): ", paste(unknown, collapse = ", "), call. = FALSE)
}
selected <- if (length(requested)) PIPELINE[all_ids %in% requested] else PIPELINE

#----------------------------------------------------------#
# Run -----
#----------------------------------------------------------#

#' Run one step script in a fresh environment and time it.
run_step <- function(step, spec = "") {
  Sys.setenv(PHENGARIS_SPEC = spec)
  on.exit(Sys.unsetenv("PHENGARIS_SPEC"), add = TRUE)
  label <- if (nzchar(spec)) paste0(" [", spec, "]") else ""
  banner <- paste0("== Step ", step$id, label, " - ", step$title, " ")
  message("\n", banner, strrep("=", max(0, 60 - nchar(banner))))

  started <- Sys.time()
  source(step$file, local = new.env(parent = globalenv()), encoding = "UTF-8")
  data.frame(
    step = step$id, spec = if (nzchar(spec)) spec else "-", title = step$title,
    seconds = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1),
    stringsAsFactors = FALSE
  )
}

started <- Sys.time()
timings <- list()
scope_of <- function(s) s$scope

for (step in Filter(function(s) scope_of(s) == "common", selected)) {
  timings[[length(timings) + 1]] <- run_step(step)
}
for (spec in pipeline_specs()) {
  for (step in Filter(function(s) scope_of(s) == "spec", selected)) {
    timings[[length(timings) + 1]] <- run_step(step, spec)
  }
}
for (step in Filter(function(s) scope_of(s) == "final", selected)) {
  timings[[length(timings) + 1]] <- run_step(step)
}

#----------------------------------------------------------#
# Summary -----
#----------------------------------------------------------#

message("\n", strrep("=", 62))
print(do.call(rbind, timings), row.names = FALSE)
message(
  "Total: ",
  round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1),
  " minutes"
)
message("Report: ", file.path(CONFIG$paths$outputs, "REPORT.md"))

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

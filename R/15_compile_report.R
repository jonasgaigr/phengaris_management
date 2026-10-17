#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#          Step 15 - Compile the full report
#
#
#----------------------------------------------------------#
#
# Collects the per-step Markdown reports into one document with a table of
# contents: first the steps shared by every specification, then the steps of
# the specifications with the revised model set (the analysis the results
# rest on), then those of the others, each group in the order config.yml
# lists them. A list of the figures of the common steps and of the revised
# specifications follows the contents. Every figure and every result table in
# the project is reachable from Outputs/REPORT.md.
#
# Reads:  Outputs/common/Reports/*.md, Outputs/<spec>/Reports/*.md
# Writes: Outputs/REPORT.md
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("15")

#--------------------------------------------------#
## Helpers -----
#--------------------------------------------------#

#' Anchor a GitHub-flavoured Markdown heading links to.
heading_anchor <- function(title) {
  slug <- tolower(title)
  slug <- gsub("[^a-z0-9 -]", "", slug)
  gsub(" ", "-", trimws(slug))
}

#' Read one step report and fit it into the compiled document.
#'
#' The step's title becomes a level-2 heading tagged with its scope, every
#' other heading moves down one level, and the links to tables and figures
#' are rewritten for a document that sits in Outputs/ rather than in
#' Outputs/<scope>/Reports/.
#'
#' @return a list with the heading title, the lines of the section and its
#'   figures (caption and path)
compile_section <- function(file, scope) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  h1 <- which(grepl("^# ", lines))[1]
  title <- if (is.na(h1)) basename(file) else sub("^# ", "", lines[h1])
  title <- paste0(title, " [", scope, "]")
  lines <- sub("^(#+) ", "#\\1 ", lines)
  if (!is.na(h1)) lines[h1] <- paste("##", title)
  lines <- gsub("](../Tables/", paste0("](", scope, "/Tables/"), lines, fixed = TRUE)
  lines <- gsub("](../Figures/", paste0("](", scope, "/Figures/"), lines, fixed = TRUE)
  figure_pattern <- "^!\\[(.*)\\]\\((.*)\\)$"
  figure_lines <- grep(figure_pattern, lines, value = TRUE)
  figures <- data.frame(
    caption = sub(figure_pattern, "\\1", figure_lines),
    path = sub(figure_pattern, "\\2", figure_lines),
    stringsAsFactors = FALSE
  )
  list(title = title, lines = c(lines, "", "---", ""), figures = figures)
}

#--------------------------------------------------#
## Collect the step reports -----
#--------------------------------------------------#

# The specifications with the revised model set come before the others.
is_revised <- vapply(pipeline_specs(), function(s) identical(spec_settings(s)$model_set, "revised"), logical(1))
specs <- c(pipeline_specs()[is_revised], pipeline_specs()[!is_revised])
scopes <- c("common", specs)
scope_titles <- c(
  common = "Steps shared by every specification",
  stats::setNames(paste0("Specification: ", specs), specs)
)

sections <- list()
for (scope in scopes) {
  files <- sort(list.files(
    file.path(CONFIG$paths$outputs, scope, "Reports"),
    pattern = "\\.md$", full.names = TRUE
  ))
  if (!length(files)) next
  sections[[scope]] <- lapply(files, compile_section, scope = scope)
}

if (!length(sections)) {
  stop("No step reports found under ", CONFIG$paths$outputs, call. = FALSE)
}

#--------------------------------------------------#
## Table of contents and body -----
#--------------------------------------------------#

toc <- unlist(lapply(names(sections), function(scope) {
  titles <- vapply(sections[[scope]], `[[`, character(1), "title")
  c(
    sprintf("- [%s](#%s)", scope_titles[[scope]], heading_anchor(scope_titles[[scope]])),
    sprintf("  - [%s](#%s)", titles, heading_anchor(titles))
  )
}), use.names = FALSE)

# The figures of the common steps and of the revised specifications, each
# linked to its PNG and to the section it sits in.
figure_scopes <- intersect(names(sections), c("common", specs[is_revised[specs]]))
figure_list <- unlist(lapply(figure_scopes, function(scope) {
  unlist(lapply(sections[[scope]], function(s) {
    if (!nrow(s$figures)) {
      return(NULL)
    }
    sprintf("- [%s](%s) ([%s](#%s))", s$figures$caption, s$figures$path, s$title, heading_anchor(s$title))
  }), use.names = FALSE)
}), use.names = FALSE)

body <- unlist(lapply(names(sections), function(scope) {
  c(
    paste("#", scope_titles[[scope]]), "",
    if (scope != "common") {
      c(spec_settings(scope)$description, "")
    },
    unlist(lapply(sections[[scope]], `[[`, "lines"), use.names = FALSE)
  )
}), use.names = FALSE)

#--------------------------------------------------#
## Assemble -----
#--------------------------------------------------#

header <- c(
  "# Phengaris spp. management in Czechia - analysis report",
  "",
  paste0("_Compiled ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "_"),
  "",
  paste(
    "Effects of grassland management on the occupancy of *Phengaris",
    "nausithous* and *Phengaris teleius* at monitored sites in Czechia,",
    "2019-2024. Every table below is also written to",
    "`Outputs/<scope>/Tables` as CSV and every figure to",
    "`Outputs/<scope>/Figures` as PNG."
  ),
  "",
  paste(
    "The specifications with the revised model set come first: they are the",
    "analysis the results rest on. The others keep the original analysis for",
    "comparison (README.md, REVIEW.md)."
  ),
  "",
  "## Contents",
  "",
  toc,
  "",
  if (length(figure_list)) c("## Figures of the revised analysis", "", figure_list, ""),
  "---",
  ""
)

path_report <- file.path(CONFIG$paths$outputs, "REPORT.md")
writeLines(c(header, body), path_report, useBytes = FALSE)

message("  ", path_report, " (", sum(lengths(sections)), " step reports)")
step_end("15")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

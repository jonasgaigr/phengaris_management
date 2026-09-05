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
# contents. Every figure and every result table in the project is reachable
# from Outputs/REPORT.md.
#
# Reads:  Outputs/Reports/*.md
# Writes: Outputs/REPORT.md
#
#----------------------------------------------------------#

message("Step 15: compiling the full report")

step_reports <- sort(
  list.files(PATHS$reports, pattern = "\\.md$", full.names = TRUE)
)

if (!length(step_reports)) {
  message("  no step reports found - nothing to compile")
} else {

  #--------------------------------------------------#
  ## Table of contents -----
  #--------------------------------------------------#

  #' Anchor a GitHub-flavoured Markdown heading links to.
  heading_anchor <- function(title) {
    slug <- tolower(title)
    slug <- gsub("[^a-z0-9 -]", "", slug)
    gsub(" ", "-", trimws(slug))
  }

  titles <- vapply(
    step_reports,
    function(f) {
      lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
      h1 <- lines[grepl("^# ", lines)][1]
      if (is.na(h1)) basename(f) else sub("^# ", "", h1)
    },
    character(1)
  )

  toc <- sprintf("%d. [%s](#%s)", seq_along(titles), titles, heading_anchor(titles))

  #--------------------------------------------------#
  ## Body -----
  #--------------------------------------------------#

  body <- unlist(lapply(step_reports, function(f) {
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    # The compiled report sits one level above the per-step reports, so the
    # links to tables and figures lose their leading "../".
    lines <- gsub("](../Tables/", "](Tables/", lines, fixed = TRUE)
    lines <- gsub("](../Figures/", "](Figures/", lines, fixed = TRUE)
    c(lines, "", "---", "")
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
      "2019-2024. Every table below is also written to `Outputs/Tables` as CSV",
      "and every figure to `Outputs/Figures` as PNG."
    ),
    "",
    "## Contents",
    "",
    toc,
    "",
    "---",
    ""
  )

  path_report <- file.path(PATHS$outputs, "REPORT.md")
  writeLines(c(header, body), path_report, useBytes = FALSE)

  message("Step 15 done: ", path_report, " (", length(step_reports), " steps)")

}

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

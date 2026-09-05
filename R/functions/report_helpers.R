#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: structured Markdown reporting
#
#----------------------------------------------------------#
#
# Every analysis step opens a report, adds its tables and figures to it, and
# closes it. Adding a table or a figure through these helpers writes the file
# to Outputs/Tables or Outputs/Figures *and* records it in the step's Markdown
# report, so the three can never drift apart.
#
# Usage inside a step script:
#
#   report_start("07", "Descriptive summaries", "What this step does.")
#   report_table(my_table, "Positivity by year", "07_positivity_by_year")
#   report_figure(my_plot, "Site occupancy", "08_site_occupancy")
#   report_finish()
#
#----------------------------------------------------------#

# Internal state of the report currently being written.
.report_env <- new.env(parent = emptyenv())

#--------------------------------------------------#
## Markdown rendering -----
#--------------------------------------------------#

#' Format a column for Markdown display.
.md_format_col <- function(x, digits = 4) {
  if (is.numeric(x)) {
    finite <- x[is.finite(x)]
    # Counts and degrees of freedom read badly as "300." or "1.00".
    whole <- length(finite) > 0 &&
      all(finite == round(finite)) &&
      max(abs(finite)) < 1e15
    out <- if (whole) {
      ifelse(is.na(x), "", format(x, trim = TRUE, scientific = FALSE))
    } else {
      ifelse(is.na(x), "", formatC(x, digits = digits, format = "g"))
    }
    return(trimws(out))
  }
  out <- as.character(x)
  out[is.na(out)] <- ""
  # Pipes and newlines would break the table layout.
  out <- gsub("|", "\\|", out, fixed = TRUE)
  gsub("[\r\n]+", " ", out)
}

#' Render a data frame as a Markdown pipe table.
#'
#' @param x         data frame
#' @param max_rows  rows to show; the full table always goes to CSV
#' @param digits    significant digits for numeric columns
md_table <- function(x, max_rows = 25, digits = 4) {
  x <- as.data.frame(x)
  if (nrow(x) == 0) return("_(no rows)_")

  n_total <- nrow(x)
  truncated <- n_total > max_rows
  if (truncated) x <- x[seq_len(max_rows), , drop = FALSE]

  cells <- lapply(x, .md_format_col, digits = digits)
  header <- paste0("| ", paste(names(x), collapse = " | "), " |")
  rule   <- paste0("|", paste(rep("---", ncol(x)), collapse = "|"), "|")
  body <- vapply(
    seq_len(nrow(x)),
    function(i) {
      paste0("| ", paste(vapply(cells, `[`, character(1), i), collapse = " | "), " |")
    },
    character(1)
  )

  out <- c(header, rule, body)
  if (truncated) {
    out <- c(
      out,
      "",
      sprintf("_Showing the first %d of %d rows._", max_rows, n_total)
    )
  }
  paste(out, collapse = "\n")
}

#--------------------------------------------------#
## Report lifecycle -----
#--------------------------------------------------#

#' Open a step report.
#'
#' @param id     two-digit step id, e.g. "07"
#' @param title  human-readable step title
#' @param intro  one or more paragraphs describing what the step does
report_start <- function(id, title, intro = NULL) {
  .report_env$id     <- id
  .report_env$title  <- title
  .report_env$lines  <- c(
    sprintf("# Step %s - %s", id, title),
    "",
    sprintf("_Generated %s_", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    ""
  )
  if (!is.null(intro)) report_note(intro)
  invisible(NULL)
}

.report_require_open <- function() {
  if (is.null(.report_env$lines)) {
    stop("No report is open. Call report_start() first.", call. = FALSE)
  }
}

#' Add one or more Markdown paragraphs.
report_note <- function(...) {
  .report_require_open()
  txt <- unlist(list(...), use.names = FALSE)
  .report_env$lines <- c(.report_env$lines, paste(txt, collapse = "\n"), "")
  invisible(NULL)
}

#' Add a section heading.
report_section <- function(title, level = 2) {
  .report_require_open()
  .report_env$lines <- c(
    .report_env$lines,
    paste0(strrep("#", level), " ", title),
    ""
  )
  invisible(NULL)
}

#' Add a bullet list.
report_bullets <- function(items) {
  .report_require_open()
  .report_env$lines <- c(.report_env$lines, paste0("- ", items), "")
  invisible(NULL)
}

#' Add a callout for something the reader must be aware of.
report_warning <- function(...) {
  txt <- paste(unlist(list(...), use.names = FALSE), collapse = " ")
  report_note(paste0("> **Note.** ", txt))
}

#' Write a table to CSV and embed it in the report.
#'
#' @param x        data frame
#' @param caption  table caption
#' @param stem     file name without extension
#' @param max_rows rows to show in the Markdown preview
report_table <- function(x, caption, stem, max_rows = 25, digits = 4) {
  .report_require_open()
  path <- write_table_csv(x, stem)
  .report_env$lines <- c(
    .report_env$lines,
    sprintf("**Table - %s**", caption),
    "",
    md_table(x, max_rows = max_rows, digits = digits),
    "",
    sprintf("Full table: [`%s`](../Tables/%s.csv)", basename(path), stem),
    ""
  )
  invisible(path)
}

#' Write a figure to PNG and embed it in the report.
#'
#' @param plot     ggplot object
#' @param caption  figure caption
#' @param stem     file name without extension
report_figure <- function(plot, caption, stem, width = 8, height = 6, dpi = 300) {
  .report_require_open()
  write_figure_png(plot, stem, width = width, height = height, dpi = dpi)
  .report_env$lines <- c(
    .report_env$lines,
    sprintf("**Figure - %s**", caption),
    "",
    sprintf("![%s](../Figures/%s.png)", caption, stem),
    ""
  )
  invisible(NULL)
}

#' Draw a base-graphics figure to PNG and embed it in the report.
#'
#' @param expr     plotting code, evaluated for its side effect
#' @param caption  figure caption
#' @param stem     file name without extension
report_base_figure <- function(expr, caption, stem, width = 8, height = 6, dpi = 300) {
  .report_require_open()
  path <- file.path(PATHS$figures, paste0(stem, ".png"))
  grDevices::png(path, width = width * dpi, height = height * dpi, res = dpi)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
  .report_env$lines <- c(
    .report_env$lines,
    sprintf("**Figure - %s**", caption),
    "",
    sprintf("![%s](../Figures/%s.png)", caption, stem),
    ""
  )
  invisible(path)
}

#' Close the report and write it to Outputs/Reports.
report_finish <- function() {
  .report_require_open()
  slug <- tolower(gsub("[^A-Za-z0-9]+", "_", .report_env$title))
  slug <- gsub("^_|_$", "", slug)
  path <- file.path(
    PATHS$reports,
    sprintf("%s_%s.md", .report_env$id, slug)
  )
  writeLines(.report_env$lines, path, useBytes = FALSE)
  message("  report written: ", path)
  .report_env$lines <- NULL
  invisible(path)
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

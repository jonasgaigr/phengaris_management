#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#     Tool - Snapshot the outputs and compare two snapshots
#
#----------------------------------------------------------#
#
# NOT part of the cascade. Used while revising the code, to prove that a
# refactoring leaves every number unchanged and to list every number a bug fix
# does change.
#
#   source("R/tools/compare_snapshots.R")
#   snapshot_outputs("03_before_config")       # before the change
#   ... change the code, re-run the cascade ...
#   snapshot_outputs("04_after_config")
#   compare_snapshots("03_before_config", "04_after_config", "a03_config")
#
# A snapshot is a flat copy of every CSV under Data/Processed and of every CSV
# under Outputs/**/Tables, in Data/Snapshots/<label>/ (not tracked by git).
# The comparison is written to changes/<stem>.md and changes/<stem>.csv, which
# are tracked, so that CHANGELOG.md can point to the full list of changed
# numbers.
#
#----------------------------------------------------------#

# md_table() renders the Markdown tables of the comparison report.
if (!exists("md_table")) source("R/functions/report_helpers.R", encoding = "UTF-8")

#' Copy the current processed data and result tables into a snapshot.
#'
#' @param label     folder name under Data/Snapshots
#' @param processed folder holding the processed data, searched recursively
#' @param outputs   folder holding the result tables, searched recursively for
#'                  folders called "Tables"
#' @return the snapshot directory, invisibly
snapshot_outputs <- function(label,
                             processed = "Data/Processed",
                             outputs = "Outputs",
                             root = "Data/Snapshots") {
  dest <- file.path(root, label)
  if (dir.exists(dest)) unlink(dest, recursive = TRUE)
  dir.create(file.path(dest, "processed"), recursive = TRUE)
  dir.create(file.path(dest, "tables"), recursive = TRUE)

  processed_files <- list.files(processed, "\\.csv$", recursive = TRUE, full.names = TRUE)
  table_files <- list.files(outputs, "\\.csv$", recursive = TRUE, full.names = TRUE)
  table_files <- table_files[basename(dirname(table_files)) == "Tables"]

  file.copy(processed_files, file.path(dest, "processed", basename(processed_files)))
  file.copy(table_files, file.path(dest, "tables", basename(table_files)))

  message(
    "Snapshot ", dest, ": ", length(processed_files), " processed files, ",
    length(table_files), " tables"
  )
  invisible(dest)
}

#' Read one snapshot CSV with every column as character.
#'
#' Result tables are written in Windows-1250, the processed data in UTF-8.
.read_snapshot_csv <- function(path, kind) {
  encoding <- if (kind == "tables") "Windows-1250" else "UTF-8"
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    locale = readr::locale(encoding = encoding),
    na = c("", "NA"),
    progress = FALSE
  )
}

#' Columns that identify a row: every column that is not entirely numeric.
.key_columns <- function(x, file) {
  known <- list(
    "data_with_imputed.csv" = c("ID_NALEZ", "DRUH", "IMPUTED"),
    "data_clean.csv"        = c("ID_NALEZ", "DRUH", "IMPUTED"),
    "data_analysis.csv"     = c("ID_NALEZ", "DRUH", "IMPUTED"),
    "records.csv"           = c("ID_NALEZ")
  )
  if (!is.null(known[[file]])) {
    return(intersect(known[[file]], names(x)))
  }
  is_text <- vapply(x, function(col) {
    v <- col[!is.na(col)]
    length(v) > 0 && anyNA(suppressWarnings(as.numeric(v)))
  }, logical(1))
  names(x)[is_text]
}

#' Build a unique row key: the key columns plus the occurrence number.
.row_keys <- function(x, key_cols) {
  base <- if (length(key_cols)) {
    do.call(paste, c(lapply(x[key_cols], function(v) ifelse(is.na(v), "<NA>", v)), sep = " | "))
  } else {
    rep("row", nrow(x))
  }
  paste0(base, " #", stats::ave(seq_along(base), base, FUN = seq_along))
}

#' Compare two cell vectors: equal, numerically equal, or changed.
.cell_status <- function(a, b, tolerance = 1e-9) {
  same_na <- is.na(a) & is.na(b)
  same_txt <- !is.na(a) & !is.na(b) & a == b
  na <- suppressWarnings(as.numeric(a))
  nb <- suppressWarnings(as.numeric(b))
  same_num <- !is.na(na) & !is.na(nb) &
    abs(na - nb) <= tolerance * pmax(1, abs(na), abs(nb))
  ifelse(same_na | same_txt, "equal", ifelse(same_num, "numerically_equal", "changed"))
}

#' Compare one file present in both snapshots.
.compare_file <- function(path_a, path_b, file, kind) {
  a <- .read_snapshot_csv(path_a, kind)
  b <- .read_snapshot_csv(path_b, kind)

  cols_added <- setdiff(names(b), names(a))
  cols_removed <- setdiff(names(a), names(b))
  common <- intersect(names(a), names(b))

  key_cols <- intersect(.key_columns(a, file), common)
  ka <- .row_keys(a, key_cols)
  kb <- .row_keys(b, key_cols)
  rows_removed <- sum(!ka %in% kb)
  rows_added <- sum(!kb %in% ka)

  shared <- intersect(ka, kb)
  ia <- match(shared, ka)
  ib <- match(shared, kb)

  cells <- lapply(setdiff(common, key_cols), function(col) {
    st <- .cell_status(a[[col]][ia], b[[col]][ib])
    keep <- st != "equal"
    if (!any(keep)) {
      return(NULL)
    }
    data.frame(
      file = file, row = shared[keep], column = col,
      before = a[[col]][ia][keep], after = b[[col]][ib][keep],
      status = st[keep], stringsAsFactors = FALSE
    )
  })
  cells <- do.call(rbind, Filter(Negate(is.null), cells))
  if (is.null(cells)) {
    cells <- data.frame(
      file = character(), row = character(), column = character(),
      before = character(), after = character(), status = character()
    )
  }

  n_changed <- sum(cells$status == "changed")
  unchanged <- !length(cols_added) && !length(cols_removed) &&
    !rows_added && !rows_removed && n_changed == 0
  status <- if (unchanged) {
    if (nrow(cells)) "numerically identical" else "identical"
  } else {
    "changed"
  }

  list(
    summary = data.frame(
      file = file, kind = kind, status = status,
      rows_before = nrow(a), rows_after = nrow(b),
      rows_removed = rows_removed, rows_added = rows_added,
      cols_removed = paste(cols_removed, collapse = ", "),
      cols_added = paste(cols_added, collapse = ", "),
      cells_changed = n_changed,
      stringsAsFactors = FALSE
    ),
    cells = cells[cells$status == "changed", , drop = FALSE]
  )
}

#' Compare two snapshots and write the differences to changes/.
#'
#' @param before,after  snapshot labels under Data/Snapshots
#' @param stem          file name stem for changes/<stem>.md and .csv
#' @param title         heading of the Markdown report
#' @param max_cells     changed cells listed per file in the Markdown report;
#'                      the CSV always lists all of them
#' @return invisibly, a list with the per-file summary, the changed cells and a
#'         flag telling whether everything is identical
compare_snapshots <- function(before, after, stem, title = stem,
                              max_cells = 60, root = "Data/Snapshots",
                              out_dir = "changes") {
  results <- list()
  for (kind in c("processed", "tables")) {
    fa <- list.files(file.path(root, before, kind), "\\.csv$")
    fb <- list.files(file.path(root, after, kind), "\\.csv$")
    for (f in union(fa, fb)) {
      if (!f %in% fb || !f %in% fa) {
        results[[length(results) + 1]] <- list(
          summary = data.frame(
            file = f, kind = kind,
            status = if (f %in% fa) "removed" else "added",
            rows_before = NA_integer_, rows_after = NA_integer_,
            rows_removed = NA_integer_, rows_added = NA_integer_,
            cols_removed = "", cols_added = "", cells_changed = NA_integer_,
            stringsAsFactors = FALSE
          ),
          cells = NULL
        )
        next
      }
      results[[length(results) + 1]] <- .compare_file(
        file.path(root, before, kind, f),
        file.path(root, after, kind, f),
        f, kind
      )
    }
  }

  summary <- do.call(rbind, lapply(results, `[[`, "summary"))
  cells <- do.call(rbind, lapply(results, `[[`, "cells"))
  if (is.null(cells)) cells <- data.frame()
  all_identical <- all(summary$status %in% c("identical", "numerically identical"))

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  utils::write.csv(cells, file.path(out_dir, paste0(stem, ".csv")), row.names = FALSE)

  changed <- summary[summary$status != "identical", , drop = FALSE]
  md <- c(
    paste0("# ", title), "",
    sprintf("Snapshot `%s` compared with `%s`.", before, after), "",
    sprintf(
      paste(
        "%d files compared: %d identical, %d numerically identical (differences below 1e-9",
        "relative), %d changed, %d added, %d removed."
      ),
      nrow(summary), sum(summary$status == "identical"),
      sum(summary$status == "numerically identical"),
      sum(summary$status == "changed"), sum(summary$status == "added"),
      sum(summary$status == "removed")
    ),
    ""
  )
  if (nrow(changed)) {
    md <- c(md, "## Files that differ", "", md_table(changed, max_rows = 500), "")
  }
  if (nrow(cells)) {
    md <- c(
      md, "## Changed numbers", "",
      sprintf("The full list of %d changed cells is in `%s.csv`.", nrow(cells), stem), ""
    )
    for (f in unique(cells$file)) {
      x <- cells[cells$file == f, c("row", "column", "before", "after")]
      md <- c(md, paste0("### ", f), "", md_table(x, max_rows = max_cells), "")
    }
  }
  writeLines(md, file.path(out_dir, paste0(stem, ".md")), useBytes = FALSE)

  message(
    if (all_identical) "IDENTICAL: " else "DIFFERENT: ",
    sum(summary$status == "changed"), " changed, ",
    sum(summary$status %in% c("added", "removed")), " added/removed, ",
    nrow(cells), " changed cells -> ", file.path(out_dir, paste0(stem, ".md"))
  )
  invisible(list(summary = summary, cells = cells, identical = all_identical))
}

#----------------------------------------------------------#
# End tool -----
#----------------------------------------------------------#

#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: validating the data steps' outputs
#
#----------------------------------------------------------#
#
# Every data step (01-06) ends by checking what it is about to write:
# expected columns and types, unique keys, no unexpected NA, allowed values,
# coordinates inside the border of Czechia, and row counts consistent with
# the step before.
# All checks use checkmate. Every failed check is collected and reported
# together, and the step stops before it writes a file that later steps
# would trust.
#
#----------------------------------------------------------#

#' Check a table and stop with every problem found.
#'
#' @param x        data frame to check
#' @param name     what is being checked, for the error message
#' @param columns  named character vector: column = expected type, one of
#'                 "character", "numeric", "logical"
#' @param key      columns whose combination must be unique
#' @param not_na   columns that must not contain NA
#' @param values   named list: column = allowed values (NA always allowed
#'                 unless the column is also in `not_na`)
#' @param xy       names of the coordinate columns to check against the
#'                 bounding box and the border of Czechia in S-JTSK, or NULL
#' @param n_rows   exact number of rows expected, or NULL
#' @param max_rows largest number of rows allowed, or NULL
#' @return `x`, invisibly
validate_table <- function(x, name, columns = NULL, key = NULL, not_na = NULL,
                           values = NULL, xy = NULL, n_rows = NULL, max_rows = NULL) {
  problems <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x, add = problems, .var.name = name)

  if (length(columns)) {
    checkmate::assert_names(
      names(x),
      must.include = names(columns), add = problems,
      .var.name = paste0("columns of ", name)
    )
    for (col in intersect(names(columns), names(x))) {
      check <- switch(columns[[col]],
        character = checkmate::check_character(x[[col]]),
        numeric = checkmate::check_numeric(x[[col]]),
        logical = checkmate::check_logical(x[[col]]),
        stop("Unknown column type in validate_table(): ", columns[[col]], call. = FALSE)
      )
      if (!isTRUE(check)) problems$push(paste0(name, "$", col, ": ", check))
    }
  }

  if (length(key)) {
    duplicated_keys <- sum(duplicated(as.data.frame(x)[, key, drop = FALSE]))
    if (duplicated_keys) {
      problems$push(sprintf(
        "%s: %d rows duplicate the key (%s)", name, duplicated_keys,
        paste(key, collapse = ", ")
      ))
    }
  }

  for (col in intersect(not_na, names(x))) {
    n_na <- sum(is.na(x[[col]]))
    if (n_na) problems$push(sprintf("%s$%s: %d unexpected NA", name, col, n_na))
  }

  for (col in intersect(names(values), names(x))) {
    bad <- setdiff(unique(stats::na.omit(x[[col]])), values[[col]])
    if (length(bad)) {
      problems$push(sprintf(
        "%s$%s: unexpected value(s) %s", name, col,
        paste(utils::head(bad, 5), collapse = ", ")
      ))
    }
  }

  if (length(xy)) {
    bbox <- CONFIG$crs$bbox
    outside <- sum(
      x[[xy[1]]] < bbox$xmin | x[[xy[1]]] > bbox$xmax |
        x[[xy[2]]] < bbox$ymin | x[[xy[2]]] > bbox$ymax,
      na.rm = TRUE
    )
    if (outside) {
      problems$push(sprintf(
        "%s: %d rows have coordinates outside the S-JTSK bounding box of Czechia",
        name, outside
      ))
    }
    # The bounding box also holds parts of Germany, Poland, Austria and
    # Slovakia, so the points inside it are tested against the border too.
    known <- !is.na(x[[xy[1]]]) & !is.na(x[[xy[2]]])
    outside_border <- sum(!inside_czechia(x[[xy[1]]][known], x[[xy[2]]][known]))
    if (outside_border) {
      problems$push(sprintf(
        "%s: %d rows have coordinates outside the border of Czechia",
        name, outside_border
      ))
    }
  }

  if (!is.null(n_rows) && nrow(x) != n_rows) {
    problems$push(sprintf("%s: %d rows, expected %d", name, nrow(x), n_rows))
  }
  if (!is.null(max_rows) && nrow(x) > max_rows) {
    problems$push(sprintf("%s: %d rows, more than the %d allowed", name, nrow(x), max_rows))
  }

  checkmate::reportAssertions(problems)
  invisible(x)
}

.validate_env <- new.env(parent = emptyenv())

#' Whether points lie inside the border of Czechia.
#'
#' The border is read once per session from the reference layer.
#'
#' @param x,y coordinates in S-JTSK, without NA
#' @return logical vector, TRUE for a point inside the border or on it
inside_czechia <- function(x, y) {
  if (is.null(.validate_env$border)) {
    .validate_env$border <- read_reference_layer("czechia_border") %>%
      sf::st_transform(CRS_SJTSK) %>%
      sf::st_union()
  }
  points <- sf::st_as_sf(data.frame(x = x, y = y), coords = c("x", "y"), crs = CRS_SJTSK)
  lengths(sf::st_intersects(points, .validate_env$border)) > 0
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

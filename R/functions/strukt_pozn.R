#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: parsing the structured notes (STRUKT_POZN)
#
#----------------------------------------------------------#
#
# STRUKT_POZN holds the structured part of the field form as one string of
# "key: value" pairs separated by ", ", for example
#
#   Seč vhodně načasovaná: ano, Seč celoplošná: ne, vliv: sukcese,
#   eutrofizace či okyselování, Perspektiva lokality: dobrá,
#   Přítomnost rostlin: hojně
#
# Values can themselves contain ", " (the threat list does, and so do the
# free-text notes), so the string cannot simply be split on commas. Instead
# the known keys are located, and each value runs from the end of its key to
# the start of the next one. The keys and the column each one is parsed into
# are listed in config.yml under `strukt_pozn: keys`.
#
# The form changed over the years, so two keys can feed the same column:
#
#   host_plant  `sta_pritomnostrostlin` (2019-2020), `Přítomnost rostlin`
#               (2020 onwards)
#   threats     `Negativní vlivy na lokalitě` (2019-2023, an older list of
#               threat items), `vliv` (2021 onwards, a newer list)
#
#----------------------------------------------------------#

#--------------------------------------------------#
## Key-value parser -----
#--------------------------------------------------#

#' Escape the characters that have a meaning in a regular expression.
.regex_escape <- function(x) {
  gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", x)
}

#' Split one structured note into its key-value pairs.
#'
#' @param x         one STRUKT_POZN string
#' @param key_regex regular expression matching any known key, built by
#'                  parse_strukt_pozn()
#' @return a named character vector, key = value, plus an "unparsed" element
#'         holding any text before the first known key
.parse_one_note <- function(x, key_regex) {
  if (is.na(x) || !nzchar(x)) {
    return(character(0))
  }
  m <- gregexpr(key_regex, x, perl = TRUE)[[1]]
  if (m[1] == -1) {
    return(c(unparsed = x))
  }

  starts <- as.integer(m)
  lengths <- attr(m, "match.length")
  ends <- c(starts[-1] - 1L, nchar(x))
  keys <- sub(": $", "", sub("^, ", "", regmatches(x, list(m))[[1]]))
  values <- trimws(substring(x, starts + lengths, ends))

  out <- stats::setNames(values, keys)
  if (starts[1] > 1L) out <- c(unparsed = substr(x, 1L, starts[1] - 1L), out)
  out
}

#' Parse the structured notes into one column per key.
#'
#' @param x     character vector of STRUKT_POZN strings
#' @param keys  named list mapping each key of the form to a column name, as
#'              in config.yml `strukt_pozn: keys`; several keys may map to
#'              the same column
#' @return a tibble with one row per element of `x` and one character column
#'         per target column, plus `unparsed` (text outside any known key).
#'         When two keys of the same row map to one column, their values are
#'         joined with ", ".
parse_strukt_pozn <- function(x, keys = CONFIG$strukt_pozn$keys) {
  key_names <- names(keys)
  # Longest first, so that a key can never match only part of a longer one.
  key_names <- key_names[order(nchar(key_names), decreasing = TRUE)]
  key_regex <- paste0(
    "(?:^|, )(?:", paste(.regex_escape(key_names), collapse = "|"), "): "
  )

  columns <- unique(c(unlist(keys, use.names = FALSE), "unparsed"))
  unique_x <- unique(x)
  parsed <- lapply(unique_x, .parse_one_note, key_regex = key_regex)

  rows <- lapply(parsed, function(p) {
    out <- stats::setNames(rep(NA_character_, length(columns)), columns)
    if (!length(p)) {
      return(out)
    }
    target <- ifelse(names(p) == "unparsed", "unparsed", unlist(keys[names(p)]))
    for (col in unique(target)) {
      out[[col]] <- paste(p[target == col], collapse = ", ")
    }
    out
  })
  table <- tibble::as_tibble(do.call(rbind, rows))
  table[match(x, unique_x), , drop = FALSE]
}

#--------------------------------------------------#
## Threats and pressures -----
#--------------------------------------------------#

#' Flag each threat class in a parsed threat list.
#'
#' A record has a threat class when its threat list contains any of the
#' class's phrases. A record without any threat list gets `missing` for every
#' class.
#'
#' @param threats  character vector, the `threats` column of
#'                 parse_strukt_pozn()
#' @param classes  named list, class = character vector of phrases, as in
#'                 config.yml `strukt_pozn: threat_classes`
#' @param missing  value for records without a threat list
#' @return a tibble with one 0/1 column per class
threat_class_flags <- function(threats, classes, missing = 0) {
  flags <- lapply(classes, function(phrases) {
    hit <- Reduce(`|`, lapply(phrases, function(p) grepl(p, threats, fixed = TRUE)))
    ifelse(is.na(threats), missing, as.numeric(hit))
  })
  tibble::as_tibble(flags)
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

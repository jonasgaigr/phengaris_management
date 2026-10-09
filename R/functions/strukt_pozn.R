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
#'
#' The pattern needs perl = TRUE: in the default (TRE) engine a backslash
#' inside brackets is literal, and the class then matched almost nothing, so
#' nothing was escaped. The keys of the form contain no special characters,
#' so this mattered only for the threat items, which contain parentheses.
.regex_escape <- function(x) {
  gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", x, perl = TRUE)
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

#' Does a threat list contain an item, as a whole item?
#'
#' Items are separated by "," with or without a space, and an item can carry
#' a free-text comment after ":". An item therefore matches when it starts
#' the list or follows a comma, and is followed by a comma, a colon or the
#' end of the list. "eutrofizace" does not match inside "(např. sešlap,
#' eutrofizace apod.)", nor "sukcese" inside "sukcese všeobecně".
#'
#' @param threats  character vector of threat lists
#' @param item     one item, verbatim
#' @return logical, FALSE where the list is NA
has_threat_item <- function(threats, item) {
  pattern <- paste0("(?:^|,)\\s*", .regex_escape(item), "\\s*(?=,|:|$)")
  !is.na(threats) & grepl(pattern, threats, perl = TRUE)
}

#' Flag each threat class by whole-item matching (the harmonised vocabulary).
#'
#' @param threats  character vector, the `threats` column of
#'                 parse_strukt_pozn()
#' @param items    named list, class = character vector of items, as in
#'                 config.yml `strukt_pozn: threat_items`
#' @param missing  value for records without a threat list
#' @return a tibble with one 0/1 column per class
threat_item_flags <- function(threats, items, missing = NA) {
  flags <- lapply(items, function(class_items) {
    hit <- Reduce(`|`, lapply(class_items, has_threat_item, threats = threats))
    ifelse(is.na(threats), missing, as.numeric(hit))
  })
  tibble::as_tibble(flags)
}

#' How much of the threat lists the harmonised vocabulary recognises.
#'
#' @param threats    character vector of threat lists
#' @param items      config.yml `strukt_pozn: threat_items`
#' @param unmapped   config.yml `strukt_pozn: threat_items_unmapped`
#' @param no_threat  config.yml `strukt_pozn: no_threat_phrase`
#' @return a list: `items`, the records containing each known item with its
#'         class; `unrecognised`, the text left over once every known item is
#'         removed, with the records it occurs in
threat_vocabulary_coverage <- function(threats, items = CONFIG$strukt_pozn$threat_items,
                                       unmapped = unlist(CONFIG$strukt_pozn$threat_items_unmapped),
                                       no_threat = CONFIG$strukt_pozn$no_threat_phrase) {
  known <- dplyr::bind_rows(
    tibble::tibble(
      class = rep(names(items), lengths(items)),
      item = unlist(items, use.names = FALSE)
    ),
    tibble::tibble(class = "(unmapped)", item = unmapped),
    tibble::tibble(class = "(no threat)", item = no_threat)
  )
  known$records <- vapply(known$item, function(i) sum(has_threat_item(threats, i)), integer(1), USE.NAMES = FALSE)

  # Remove every known item, longest first, with any comment after it.
  rest <- threats[!is.na(threats)]
  for (i in known$item[order(nchar(known$item), decreasing = TRUE)]) {
    rest <- gsub(paste0("(?:^|,)\\s*", .regex_escape(i), "\\s*(?::[^,]*)?(?=,|$)"), "", rest, perl = TRUE)
  }
  rest <- trimws(gsub("^[,[:space:]]+|[,[:space:]]+$", "", rest))
  unrecognised <- tibble::tibble(text = rest[nzchar(rest)]) %>%
    dplyr::count(text, name = "records", sort = TRUE)

  list(items = known, unrecognised = unrecognised)
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

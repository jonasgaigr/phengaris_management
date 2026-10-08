#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: project configuration
#
#----------------------------------------------------------#
#
# config.yml holds every tunable choice of the analysis. load_config() reads
# it and fails with a clear message when a required entry is missing, so that
# a typo in the file stops the cascade at setup instead of half-way through.
#
#----------------------------------------------------------#

#' Read and check the project configuration.
#'
#' @param path  the YAML file, config.yml in the project root by default
#' @return the configuration as a nested list
load_config <- function(path = "config.yml") {
  if (!file.exists(path)) {
    stop("Configuration file not found: ", path, call. = FALSE)
  }
  cfg <- yaml::read_yaml(path, fileEncoding = "UTF-8")

  required <- c(
    "years.first", "years.last",
    "species.nausithous.name", "species.teleius.name",
    "species.nausithous.n2k_name", "species.teleius.n2k_name",
    "crs.epsg", "sources.targeted",
    "paths.input", "paths.processed", "paths.outputs",
    "paths.occurrence.nausithous", "paths.occurrence.teleius",
    "paths.site_layers.nausithous", "paths.site_layers.teleius",
    "paths.ranges.nausithous", "paths.ranges.teleius"
  )
  missing <- required[!vapply(required, function(key) {
    !is.null(config_get(cfg, key))
  }, logical(1))]
  if (length(missing)) {
    stop(
      "config.yml is missing: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  checkmate::assert_int(cfg$years$first)
  checkmate::assert_int(cfg$years$last, lower = cfg$years$first)
  checkmate::assert_int(cfg$crs$epsg)
  checkmate::assert_character(cfg$sources$targeted, min.len = 1, any.missing = FALSE)

  cfg
}

#' Look up a nested configuration entry by a dotted key.
#'
#' @param cfg  configuration list
#' @param key  e.g. "paths.occurrence.teleius"
#' @return the entry, or NULL when any part of the key is missing
config_get <- function(cfg, key) {
  out <- cfg
  for (part in strsplit(key, ".", fixed = TRUE)[[1]]) {
    if (!is.list(out) || is.null(out[[part]])) return(NULL)
    out <- out[[part]]
  }
  out
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#

#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#           Step 21 - The landscape around the sites
#
#
#----------------------------------------------------------#
#
# P4.7: every surveyed site described by the landscape around it, at several
# scales. In circles of config.yml `landscape: radii_m` around the site's
# representative point (step 16):
#
#   GRASSLAND, ARABLE, FOREST, WATER, URBAN   shares of the land-cover groups
#                                             of KVES 2025
#   NATURAL, SANGUISORBA                      shares of natural habitats and
#                                             of Sanguisorba meadows (VMB)
#
# as <VARIABLE>_<radius>. Step 04 joins them onto the records of the
# specifications with design variables; step 22 picks the radius of each by
# AIC. Without the landscape rasters (R/tools/build_landscape_layers.R) the
# step writes no table and step 22 skips the landscape.
#
# Reads:  Data/Processed/common/records.csv   (step 01)
#         Data/Processed/common/site_design.csv   (step 16)
#         Data/Input/landscape/*.tif   (optional)
# Writes: Data/Processed/common/landscape.csv
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("21")

settings <- CONFIG$landscape
paths <- file.path(settings$dir, c(landcover = "landcover_25m.tif", vmb = "vmb_25m.tif", coarse = "landscape_100m.tif"))
names(paths) <- c("landcover", "vmb", "coarse")

report_start(
  "21",
  "Landscape around the sites",
  paste(
    "The shares of grassland, arable land, forest, water and settlements",
    "(KVES 2025) and of natural habitats and Sanguisorba meadows (VMB) in",
    "circles of several radii around every surveyed site."
  )
)

out_path <- processed_path("landscape.csv", "common")
if (!all(file.exists(paths))) {
  report_warning(
    "The landscape rasters are missing (", paste(paths[!file.exists(paths)], collapse = ", "), ").",
    "Run build_landscape_layers() (R/tools/build_landscape_layers.R); until then",
    "the revised models have no landscape variables."
  )
  if (file.exists(out_path)) file.remove(out_path)
  report_finish()
  step_end("21")
} else {
  terra::terraOptions(progress = 0)
  records <- read_records()
  site_design <- readr::read_csv(processed_path("site_design.csv", "common"), show_col_types = FALSE)
  surveyed <- site_design %>%
    dplyr::filter(ID_LOKAL %in% records$ID_LOKAL[records$ZDROJ %in% target_mon_zdroj]) %>%
    dplyr::select(ID_LOKAL, SITE_X, SITE_Y)

  landscape <- landscape_shares(
    surveyed,
    landcover = terra::rast(paths[["landcover"]]),
    vmb = terra::rast(paths[["vmb"]]),
    coarse = terra::rast(paths[["coarse"]])
  )

  built <- yaml::read_yaml(file.path(settings$dir, "BUILT.yml"))
  report_note(sprintf(
    paste(
      "%d sites of the targeted records. Land cover from KVES %s (built %s);",
      "the groups are the KVES codes starting with: %s. Radii up to %d m are",
      "summarised on the %d m rasters, larger ones on the %d m rasters. A share",
      "counts only the part of the circle inside Czechia."
    ),
    nrow(landscape), built$kves_version, built$built,
    paste(sprintf("%s (%s)", names(settings$groups), vapply(settings$groups, function(g) paste(unlist(g), collapse = ", "), character(1))), collapse = "; "),
    settings$fine_max_radius_m, settings$resolution_m, settings$coarse_resolution_m
  ))

  long <- landscape %>%
    tidyr::pivot_longer(-ID_LOKAL, names_to = c("variable", "radius_m"), names_pattern = "^(.*)_([0-9]+)$", values_to = "share") %>%
    dplyr::mutate(radius_m = as.integer(radius_m))
  report_table(
    long %>%
      dplyr::group_by(variable, radius_m) %>%
      dplyr::summarise(
        median = stats::median(share, na.rm = TRUE),
        p10 = stats::quantile(share, 0.1, na.rm = TRUE),
        p90 = stats::quantile(share, 0.9, na.rm = TRUE),
        sites_without = sum(share == 0, na.rm = TRUE),
        .groups = "drop"
      ),
    "Shares of the landscape around the surveyed sites by radius (median, 10th and 90th percentile)",
    "21_landscape_summary",
    max_rows = 60
  )

  # How much the radius matters: the correlation of each variable between
  # radii. High correlations mean the choice of radius changes little.
  radii <- unlist(settings$radii_m)
  correlations <- dplyr::bind_rows(lapply(landscape_variables(), function(v) {
    x <- as.matrix(landscape[landscape_column(v, radii)])
    r <- stats::cor(x, use = "pairwise.complete.obs", method = "spearman")
    tibble::tibble(variable = v, radius_m = radii, !!!stats::setNames(as.data.frame(r), paste0("r_", radii)))
  }))
  report_table(
    correlations,
    "Spearman correlation of each landscape variable between radii",
    "21_landscape_correlation_between_radii",
    max_rows = nrow(correlations)
  )

  report_figure(
    ggplot2::ggplot(long, ggplot2::aes(x = factor(radius_m), y = share)) +
      ggplot2::geom_boxplot(outlier.size = 0.3, colour = "grey30") +
      ggplot2::facet_wrap(~variable, scales = "free_y") +
      ggplot2::labs(x = "\nradius (m)", y = "share of the circle\n") +
      ggplot2::theme_classic(base_size = 12),
    "The landscape around the surveyed sites by radius",
    "21_landscape_by_radius",
    width = 10, height = 7
  )

  validate_table(
    landscape, "landscape",
    key = "ID_LOKAL",
    not_na = "ID_LOKAL",
    n_rows = nrow(surveyed)
  )
  checkmate::assert_numeric(as.matrix(landscape[-1]), lower = 0, upper = 1, .var.name = "landscape shares")

  readr::write_csv(landscape, out_path)
  report_finish()
  message("  ", nrow(landscape), " sites described at ", length(radii), " radii")
  step_end("21")
}

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#

#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#     Tool - Refresh the snapshots of the remote reference layers
#
#----------------------------------------------------------#
#
# NOT part of the cascade. The cascade reads the reference layers only from
# the local copies in Data/Input/reference/, so that a re-run gives the same
# result whatever the remote services happen to serve that day. Run this by
# hand, deliberately, when the copies should be brought up to date:
#
#   source("R/00_setup.R")
#   source("R/tools/refresh_reference_layers.R")
#   refresh_reference_layers()
#
# Then re-run the cascade and diff the outputs (R/tools/compare_snapshots.R)
# to see what the newer layers changed.
#
# What is fetched, and from where, is set in config.yml under `reference`.
# Each vector layer is stored as delivered, in its own CRS, as a zipped
# GeoPackage (EVL alone is 28 MB unzipped, 12 MB zipped); every
# transformation stays in the cascade. The one exception is the shaded
# relief, which is stored already projected, aggregated and clipped to the
# border, exactly as step 14 used to prepare it on every run, because the raw
# raster is about 80 MB. RETRIEVED.yml records the date, the source and the
# size of each copy.
#
#----------------------------------------------------------#

#' Write an sf layer as a zipped GeoPackage, <name>.gpkg.zip.
write_zipped_gpkg <- function(x, dest, name) {
  tmp <- file.path(tempdir(), paste0(name, ".gpkg"))
  sf::st_write(x, tmp, delete_dsn = TRUE, quiet = TRUE)
  # Absolute, because zip::zip() works from inside `root`.
  zip_path <- file.path(normalizePath(dest, winslash = "/"), paste0(name, ".gpkg.zip"))
  if (file.exists(zip_path)) file.remove(zip_path)
  zip::zip(zip_path, basename(tmp), root = dirname(tmp), compression_level = 9)
  file.remove(tmp)
  invisible(zip_path)
}

#' Fetch every remote reference layer and store it in Data/Input/reference.
#'
#' @param dest  destination folder
#' @return the destination folder, invisibly
refresh_reference_layers <- function(dest = CONFIG$paths$reference) {
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  ref <- CONFIG$reference
  log <- list()

  #--------------------------------------------------#
  ## AOPK open-data WFS layers -----
  #--------------------------------------------------#

  for (name in names(ref$wfs_layers)) {
    url <- paste0(
      ref$wfs_endpoint,
      "service=WFS&version=2.0.0&request=GetFeature&typeName=",
      ref$wfs_layers[[name]]
    )
    message("Fetching ", name, " ...")
    x <- sf::st_read(url, quiet = TRUE)
    write_zipped_gpkg(x, dest, name)
    log[[name]] <- list(source = url, features = nrow(x))
  }

  #--------------------------------------------------#
  ## Border of Czechia (RCzechia) -----
  #--------------------------------------------------#

  message("Fetching the border of Czechia ...")
  border <- RCzechia::republika(resolution = "high")
  write_zipped_gpkg(border, dest, "czechia_border")
  log$czechia_border <- list(
    source = paste0("RCzechia::republika(resolution = 'high'), RCzechia ", utils::packageVersion("RCzechia")),
    features = nrow(border)
  )

  #--------------------------------------------------#
  ## Natura 2000 site subjects (rn2kcz) -----
  #--------------------------------------------------#

  message("Fetching the Natura 2000 site-subject table ...")
  subjects <- rn2kcz::load_n2k_sites(return = TRUE, envir = new.env())
  readr::write_csv(subjects, file.path(dest, "n2k_site_subjects.csv"))
  n2k_url <- regmatches(
    paste(deparse(rn2kcz::load_n2k_sites), collapse = " "),
    regexpr("https?://[^\"]+", paste(deparse(rn2kcz::load_n2k_sites), collapse = " "))
  )
  log$n2k_site_subjects <- list(
    source = paste0("rn2kcz::load_n2k_sites(), rn2kcz ", utils::packageVersion("rn2kcz"), ", ", n2k_url),
    rows = nrow(subjects)
  )

  #--------------------------------------------------#
  ## Shaded relief, prepared for the map -----
  #--------------------------------------------------#

  message("Fetching and preparing the shaded relief ...")
  relief <- RCzechia::vyskopis(format = "rayshaded", cropped = FALSE)
  # The relief arrives in WGS84 while every other layer is in S-JTSK.
  relief <- terra::project(relief, paste0("EPSG:", CONFIG$crs$epsg))
  # At full resolution this is about 21 million cells, far more than a figure
  # of a few hundred dpi can show.
  relief <- terra::aggregate(relief, fact = ref$relief_aggregate, fun = "mean", na.rm = TRUE)
  # Reprojecting the WGS84 rectangle leaves a rotated block of relief spilling
  # past Czechia, so it is clipped to the border.
  border_vect <- terra::vect(sf::st_transform(border, CONFIG$crs$epsg))
  relief <- terra::mask(terra::crop(relief, border_vect), border_vect)
  terra::writeRaster(
    relief, file.path(dest, "relief_shaded.tif"), overwrite = TRUE,
    datatype = "FLT4S", gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3")
  )
  log$relief_shaded <- list(
    source = paste0(
      "RCzechia::vyskopis(format = 'rayshaded', cropped = FALSE), RCzechia ",
      utils::packageVersion("RCzechia"),
      "; projected to EPSG:", CONFIG$crs$epsg, ", aggregated by ",
      ref$relief_aggregate, ", clipped to the border"
    ),
    cells = terra::ncell(relief)
  )

  #--------------------------------------------------#
  ## Record what was fetched -----
  #--------------------------------------------------#

  retrieved <- format(Sys.Date())
  for (name in names(log)) {
    files <- list.files(dest, paste0("^", name, "\\."), full.names = TRUE)
    log[[name]]$retrieved <- retrieved
    log[[name]]$file <- basename(files[1])
    log[[name]]$size_mb <- round(file.size(files[1]) / 1e6, 2)
  }
  yaml::write_yaml(log, file.path(dest, "RETRIEVED.yml"))
  message("Reference layers written to ", dest)
  invisible(dest)
}

#----------------------------------------------------------#
# End tool -----
#----------------------------------------------------------#

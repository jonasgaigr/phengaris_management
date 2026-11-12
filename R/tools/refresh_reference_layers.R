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
#   refresh_reference_layers()                       # every layer
#   refresh_reference_layers(layers = "elevation")   # only the ones named
#
# Then re-run the cascade and diff the outputs (R/tools/compare_snapshots.R)
# to see what the newer layers changed. Refreshing only some layers keeps the
# copies, and the RETRIEVED.yml entries, of the others as they are.
#
# What is fetched, and from where, is set in config.yml under `reference`.
# Each vector layer is stored as delivered, in its own CRS, as a zipped
# GeoPackage (EVL alone is 28 MB unzipped, 12 MB zipped); every
# transformation stays in the cascade. The exceptions are the two rasters:
# the shaded relief, which is stored already projected, aggregated and
# clipped to the border, exactly as step 14 used to prepare it on every run,
# because the raw raster is about 80 MB; and the elevation model, projected
# to S-JTSK and clipped to the border, from which step 16 reads the elevation
# of every site. RETRIEVED.yml records the date, the source and the size of
# each copy.
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

#' The reference layers this tool can fetch.
REFERENCE_LAYERS <- c(
  "evl", "mzchu", "sitmap", "czechia_border", "n2k_site_subjects", "habitat_catalogue", "relief_shaded",
  "elevation"
)

#' The libraries rnaturecz is looked for in.
#'
#' rnaturecz is not in renv.lock while its repository is private (a lockfile
#' naming it could not be restored without access to it). Installed in the
#' user library, where `remotes::install_github("BiodivMonCZ/rnaturecz")`
#' puts it, it is found there; RNATURECZ_LIB names another library.
rnaturecz_libraries <- function() {
  version <- paste(R.version$major, sub("\\..*", "", R.version$minor), sep = ".")
  user <- if (.Platform$OS.type == "windows") {
    file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", version)
  } else {
    file.path("~", "R", paste0(R.version$platform, "-library"), version)
  }
  libs <- c(.libPaths(), Sys.getenv("RNATURECZ_LIB"), path.expand(user))
  unique(libs[nzchar(libs) & dir.exists(libs)])
}

#' Fetch the remote reference layers and store them in Data/Input/reference.
#'
#' @param dest    destination folder
#' @param layers  the layers to fetch (REFERENCE_LAYERS); the copies of the
#'                others are left as they are
#' @return the destination folder, invisibly
refresh_reference_layers <- function(dest = CONFIG$paths$reference, layers = REFERENCE_LAYERS) {
  layers <- match.arg(layers, REFERENCE_LAYERS, several.ok = TRUE)
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  ref <- CONFIG$reference
  log <- list()

  #--------------------------------------------------#
  ## AOPK open-data WFS layers -----
  #--------------------------------------------------#

  for (name in intersect(names(ref$wfs_layers), layers)) {
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

  if ("czechia_border" %in% layers) {
    message("Fetching the border of Czechia ...")
    border <- RCzechia::republika(resolution = "high")
    write_zipped_gpkg(border, dest, "czechia_border")
    log$czechia_border <- list(
      source = paste0("RCzechia::republika(resolution = 'high'), RCzechia ", utils::packageVersion("RCzechia")),
      features = nrow(border)
    )
  } else {
    # The rasters are clipped to the copy already there.
    border <- sf::st_read(file.path("/vsizip", dest, "czechia_border.gpkg.zip", "czechia_border.gpkg"), quiet = TRUE)
  }
  border_vect <- terra::vect(sf::st_transform(border, CONFIG$crs$epsg))

  #--------------------------------------------------#
  ## Natura 2000 site subjects (rn2kcz) -----
  #--------------------------------------------------#

  if ("n2k_site_subjects" %in% layers) {
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
  }

  #--------------------------------------------------#
  ## Habitat catalogue with the Natura 2000 habitat types (rnaturecz) -----
  #--------------------------------------------------#

  # The habitats of the habitat mapping (Katalog biotopů ČR) with the Natura
  # 2000 habitat type of each (TPS), from portal.nature.cz. The published VMB
  # has no Natura 2000 code; the cascade derives it from this table
  # (natura_habitat_of()).
  if ("habitat_catalogue" %in% layers) {
    message("Fetching the habitat catalogue ...")
    libs <- rnaturecz_libraries()
    if (!requireNamespace("rnaturecz", lib.loc = libs, quietly = TRUE)) {
      stop(
        "Package rnaturecz is missing. Install it with ",
        "remotes::install_github(\"BiodivMonCZ/rnaturecz\") in the user library.",
        call. = FALSE
      )
    }
    habitats <- rnaturecz::list_habitats(quiet = TRUE)
    catalogue <- habitats %>%
      dplyr::transmute(
        BIOTOP = kod, BIOTOP_NAZEV = nazev, FORMACNI_SKUPINA = formacni_skupina, PRIORITA = priorita,
        HABITAT = tps_kod, HABITAT_NAZEV = tps_nazev, HABITAT_PRIORITA = tps_priorita
      ) %>%
      dplyr::arrange(BIOTOP)
    checkmate::assert_character(catalogue$BIOTOP, any.missing = FALSE, unique = TRUE)
    readr::write_csv(catalogue, file.path(dest, "habitat_catalogue.csv"), na = "")
    log$habitat_catalogue <- list(
      source = paste0(
        "rnaturecz::list_habitats(), rnaturecz ", utils::packageVersion("rnaturecz", lib.loc = libs), ", ",
        attr(habitats, "rnaturecz_source")$page
      ),
      rows = nrow(catalogue),
      with_natura_code = sum(!is.na(catalogue$HABITAT))
    )
  }

  #--------------------------------------------------#
  ## Shaded relief, prepared for the map -----
  #--------------------------------------------------#

  if ("relief_shaded" %in% layers) {
    message("Fetching and preparing the shaded relief ...")
    relief <- RCzechia::vyskopis(format = "rayshaded", cropped = FALSE)
    # The relief arrives in WGS84 while every other layer is in S-JTSK.
    relief <- terra::project(relief, paste0("EPSG:", CONFIG$crs$epsg))
    # At full resolution this is about 21 million cells, far more than a figure
    # of a few hundred dpi can show.
    relief <- terra::aggregate(relief, fact = ref$relief_aggregate, fun = "mean", na.rm = TRUE)
    # Reprojecting the WGS84 rectangle leaves a rotated block of relief spilling
    # past Czechia, so it is clipped to the border.
    relief <- terra::mask(terra::crop(relief, border_vect), border_vect)
    terra::writeRaster(
      relief, file.path(dest, "relief_shaded.tif"),
      overwrite = TRUE,
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
  }

  #--------------------------------------------------#
  ## Elevation model -----
  #--------------------------------------------------#

  if ("elevation" %in% layers) {
    message("Fetching and preparing the elevation model ...")
    elevation <- RCzechia::vyskopis(format = "actual", cropped = FALSE)
    # Bilinear: elevation is continuous. Kept at full resolution, since step
    # 16 reads single sites from it.
    elevation <- terra::project(elevation, paste0("EPSG:", CONFIG$crs$epsg), method = "bilinear")
    elevation <- terra::mask(terra::crop(elevation, border_vect), border_vect)
    names(elevation) <- "elevation"
    # Whole metres are enough for the elevation of a site, and a third of
    # the size of floats.
    terra::writeRaster(
      round(elevation), file.path(dest, "elevation.tif"),
      overwrite = TRUE,
      datatype = "INT2S", gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2")
    )
    log$elevation <- list(
      source = paste0(
        "RCzechia::vyskopis(format = 'actual', cropped = FALSE), RCzechia ",
        utils::packageVersion("RCzechia"),
        "; projected to EPSG:", CONFIG$crs$epsg, " (bilinear), clipped to the border; metres above sea level"
      ),
      resolution_m = round(terra::res(elevation)[1], 1),
      cells = terra::ncell(elevation)
    )
  }

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
  # The entries of the layers not fetched this time stay as they were.
  log_path <- file.path(dest, "RETRIEVED.yml")
  previous <- if (file.exists(log_path)) yaml::read_yaml(log_path) else list()
  previous[names(log)] <- log
  yaml::write_yaml(previous[intersect(REFERENCE_LAYERS, names(previous))], log_path)
  message("Reference layers written to ", dest, ": ", paste(names(log), collapse = ", "))
  invisible(dest)
}

#----------------------------------------------------------#
# End tool -----
#----------------------------------------------------------#

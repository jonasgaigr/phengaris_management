#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#     Tool - Build the landscape rasters from KVES and VMB
#
#
#----------------------------------------------------------#
#
# NOT part of the cascade. Run it by hand, after download_habitat_layers()
# has put the kraj archives into the cache:
#
#   source("R/00_setup.R")
#   source("R/tools/download_habitat_layers.R")
#   source("R/tools/build_landscape_layers.R")
#   build_landscape_layers()        # about an hour; needs the cache only
#
# Then re-run the cascade from step 21 and diff the outputs
# (R/tools/compare_snapshots.R).
#
# The landscape around a site reaches up to 4 km (P4.7: Kajzer-Bonk et al.
# 2016 found the effects of grassland cover at 3-4 km), far beyond the 200 m
# the local copies of VMB and KVES keep. This tool therefore reads the whole
# kraj archives of the cache and rasterises them for all of Czechia, on the
# S-JTSK grid of config.yml `crs: bbox`:
#
#   landcover_25m.tif    the land-cover group of each 25 m cell from KVES
#                        (config.yml `landscape: groups`; 0 for the other
#                        classes, NA outside Czechia)
#   vmb_25m.tif          per cent of each 25 m cell's VMB segment in natural
#                        habitats and in Sanguisorba meadows (config.yml
#                        `habitat: sanguisorba_meadow_codes`); 0 where no
#                        segment is mapped
#   landscape_100m.tif   the same at 100 m: per cent of each cell in each
#                        land-cover group, and the VMB per cents averaged
#
# A cell takes the polygon covering its centre. Step 21 summarises the
# rasters in circles around every site. BUILT.yml next to them records the
# archives they were built from, with their md5.
#
#----------------------------------------------------------#

#' The raster grid all landscape layers share.
landscape_template <- function(resolution = CONFIG$landscape$resolution_m, bbox = CONFIG$crs$bbox) {
  terra::rast(
    xmin = bbox$xmin, xmax = bbox$xmax, ymin = bbox$ymin, ymax = bbox$ymax,
    resolution = resolution, crs = paste0("EPSG:", CONFIG$crs$epsg)
  )
}

#' The land-cover group of KVES codes, by the code prefixes of config.yml.
#'
#' @param kod     KVES codes, e.g. "C111"
#' @param groups  config.yml `landscape: groups`: group = code prefixes
#' @return the group's position in `groups` (1, 2, ...), 0 for a code in no
#'         group, NA for a missing code
landcover_group <- function(kod, groups = CONFIG$landscape$groups) {
  out <- ifelse(is.na(kod), NA_integer_, 0L)
  for (i in seq_along(groups)) {
    hit <- !is.na(kod) & out == 0L & Reduce(`|`, lapply(unlist(groups[[i]]), function(p) startsWith(kod, p)))
    out[hit] <- i
  }
  as.integer(out)
}

#' Unzip one cached archive into the cache's scratch folder.
#'
#' One kraj of KVES takes 1-2 GB unzipped, so the caller removes the folder
#' as soon as the kraj is done.
#'
#' @return the folder
.unzip_archive <- function(zip, cache_dir) {
  tmp <- file.path(cache_dir, "unzipped", paste0("landscape_", tools::file_path_sans_ext(basename(zip))))
  unlink(tmp, recursive = TRUE)
  utils::unzip(zip, exdir = tmp)
  tmp
}

#' The one file in a folder matching a pattern.
.find_file <- function(dir, pattern) {
  hit <- list.files(dir, pattern, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(hit) != 1) stop(dir, ": expected one file matching ", pattern, ", found ", length(hit), call. = FALSE)
  hit
}

#' Read a kraj shapefile as a SpatVector in the working CRS.
#'
#' As use_working_crs(): the shapefiles' ESRI WKT is equivalent to EPSG:5514,
#' and is replaced rather than transformed, so that no datum shift creeps in.
.read_kraj_vector <- function(shp, encoding) {
  v <- terra::vect(shp, opts = paste0("ENCODING=", encoding))
  if (sf::st_crs(terra::crs(v)) == sf::st_crs(CONFIG$crs$epsg)) {
    terra::crs(v) <- paste0("EPSG:", CONFIG$crs$epsg)
  } else {
    v <- terra::project(v, paste0("EPSG:", CONFIG$crs$epsg))
  }
  v
}

#' Rasterise the polygons of a SpatVector onto the template, cut to their
#' extent.
.rasterise_on <- function(v, template, field, ...) {
  window <- terra::crop(template, terra::ext(v), snap = "out")
  terra::rasterize(v, window, field = field, ...)
}

#' Build the landscape rasters (see the header).
#'
#' @param dest       output folder, config.yml `landscape: dir`
#' @param version    the KVES version, config.yml `landscape: kves_version`
#' @param cache_dir  the archive cache of download_habitat_layers()
#' @return the paths written, invisibly
build_landscape_layers <- function(dest = CONFIG$landscape$dir,
                                   version = CONFIG$landscape$kves_version,
                                   cache_dir = file.path(CONFIG$paths$cache, "aopk")) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  archives <- aopk_archive_table() %>%
    dplyr::filter(layer %in% c("vmb", paste0("kves_", version)))
  missing <- archives$zip[!file.exists(archives$zip)]
  if (length(missing)) {
    stop(
      "Archives missing from the cache (run download_habitat_layers() first):\n",
      paste0("  ", missing, collapse = "\n"),
      call. = FALSE
    )
  }
  template <- landscape_template()
  groups <- CONFIG$landscape$groups
  terra::terraOptions(progress = 0)
  paths <- c(
    landcover = file.path(dest, "landcover_25m.tif"),
    vmb = file.path(dest, "vmb_25m.tif"),
    coarse = file.path(dest, "landscape_100m.tif")
  )

  #--------------------------------------------------#
  ## KVES land cover -----
  #--------------------------------------------------#

  kves_rows <- archives[archives$layer == paste0("kves_", version), ]
  pieces <- list()
  codes_seen <- list()
  for (i in seq_len(nrow(kves_rows))) {
    message("KVES ", version, " ", kves_rows$nuts3[i], ": rasterising")
    tmp <- .unzip_archive(kves_rows$zip[i], cache_dir)
    v <- .read_kraj_vector(.find_file(tmp, "\\.shp$"), kves_rows$encoding[i])
    v$GROUP <- landcover_group(v$KOD, groups)
    codes_seen[[i]] <- as.data.frame(table(KOD = v$KOD, GROUP = v$GROUP))
    pieces[[i]] <- terra::wrap(.rasterise_on(v[, "GROUP"], template, "GROUP"))
    rm(v)
    unlink(tmp, recursive = TRUE)
    gc()
  }
  landcover <- terra::merge(terra::sprc(lapply(pieces, terra::unwrap)))
  landcover <- terra::extend(landcover, template)
  landcover <- terra::crop(landcover, template)
  names(landcover) <- "landcover"
  terra::writeRaster(landcover, paths[["landcover"]],
    overwrite = TRUE,
    datatype = "INT1U", gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2")
  )
  landcover <- terra::rast(paths[["landcover"]])
  rm(pieces)

  #--------------------------------------------------#
  ## VMB natural and Sanguisorba shares -----
  #--------------------------------------------------#

  vmb_rows <- archives[archives$layer == "vmb", ]
  sanguisorba <- unlist(CONFIG$habitat$sanguisorba_meadow_codes)
  natural_pieces <- sanguisorba_pieces <- list()
  for (i in seq_len(nrow(vmb_rows))) {
    message("VMB ", vmb_rows$nuts3[i], ": rasterising")
    tmp <- .unzip_archive(vmb_rows$zip[i], cache_dir)
    biotopes <- tibble::as_tibble(suppressWarnings(sf::st_read(
      .find_file(tmp, "_Biotop\\.dbf$"),
      options = paste0("ENCODING=", vmb_rows$encoding[i]), quiet = TRUE
    ))) %>%
      dplyr::distinct(SEGMENT_ID, B_ID, BIOTOP, STEJ_PR) %>%
      dplyr::group_by(SEGMENT_ID) %>%
      dplyr::summarise(
        NATURAL_PCT = sum(STEJ_PR[is_natural_habitat(BIOTOP)], na.rm = TRUE),
        SANGUISORBA_PCT = sum(STEJ_PR[BIOTOP %in% sanguisorba], na.rm = TRUE),
        .groups = "drop"
      )
    v <- .read_kraj_vector(.find_file(tmp, "^[^_]+\\.shp$"), vmb_rows$encoding[i])[, "SEGMENT_ID"]
    shares <- biotopes[match(v$SEGMENT_ID, biotopes$SEGMENT_ID), ]
    v$NATURAL_PCT <- dplyr::coalesce(shares$NATURAL_PCT, 0)
    v$SANGUISORBA_PCT <- dplyr::coalesce(shares$SANGUISORBA_PCT, 0)
    natural_pieces[[i]] <- terra::wrap(.rasterise_on(v, template, "NATURAL_PCT"))
    sanguisorba_pieces[[i]] <- terra::wrap(.rasterise_on(v, template, "SANGUISORBA_PCT"))
    rm(v, biotopes, shares)
    unlink(tmp, recursive = TRUE)
    gc()
  }
  # Inside Czechia a cell outside every segment is unmapped: 0 per cent.
  merge_vmb <- function(pieces, name) {
    x <- terra::merge(terra::sprc(lapply(pieces, terra::unwrap)))
    x <- terra::crop(terra::extend(x, template), template)
    x <- terra::ifel(is.na(x) & !is.na(landcover), 0, x)
    names(x) <- name
    x
  }
  vmb <- c(
    merge_vmb(natural_pieces, "natural_pct"),
    merge_vmb(sanguisorba_pieces, "sanguisorba_pct")
  )
  terra::writeRaster(round(vmb), paths[["vmb"]],
    overwrite = TRUE,
    datatype = "INT1U", gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2")
  )
  vmb <- terra::rast(paths[["vmb"]])
  rm(natural_pieces, sanguisorba_pieces)

  #--------------------------------------------------#
  ## The 100 m layers -----
  #--------------------------------------------------#

  message("Aggregating to ", CONFIG$landscape$coarse_resolution_m, " m")
  factor <- CONFIG$landscape$coarse_resolution_m / CONFIG$landscape$resolution_m
  binary <- terra::segregate(landcover, classes = seq_along(groups), other = 0)
  names(binary) <- tolower(names(groups))
  coarse <- terra::aggregate(c(binary, vmb / 100), fact = factor, fun = "mean", na.rm = TRUE)
  names(coarse) <- c(paste0(tolower(names(groups)), "_pct"), "natural_pct", "sanguisorba_pct")
  terra::writeRaster(round(coarse * 100), paths[["coarse"]],
    overwrite = TRUE,
    datatype = "INT1U", gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2")
  )

  #--------------------------------------------------#
  ## Record what was built -----
  #--------------------------------------------------#

  codes <- dplyr::bind_rows(codes_seen) %>%
    dplyr::mutate(Freq = as.integer(Freq)) %>%
    dplyr::group_by(KOD, GROUP) %>%
    dplyr::summarise(polygons = sum(Freq), .groups = "drop") %>%
    dplyr::filter(polygons > 0)
  archive_entries <- function(rows) {
    stats::setNames(lapply(rows$zip, function(zip) {
      meta <- archive_metadata(zip)
      list(url = meta$url, md5 = meta$md5, downloaded = meta$downloaded)
    }), rows$nuts3)
  }
  yaml::write_yaml(
    list(
      built = format(Sys.Date()),
      licence = CONFIG$habitat$download$licence,
      crs = paste0("EPSG:", CONFIG$crs$epsg),
      resolution_m = CONFIG$landscape$resolution_m,
      coarse_resolution_m = CONFIG$landscape$coarse_resolution_m,
      kves_version = version,
      groups = lapply(groups, unlist),
      kves_codes_by_group = stats::setNames(
        lapply(c(0, seq_along(groups)), function(g) sort(as.character(codes$KOD[codes$GROUP == g]))),
        c("other", names(groups))
      ),
      sanguisorba_meadow_codes = sanguisorba,
      files = lapply(paths, function(p) list(file = basename(p), size_mb = round(file.size(p) / 1e6, 2))),
      archives = list(kves = archive_entries(kves_rows), vmb = archive_entries(vmb_rows))
    ),
    file.path(dest, "BUILT.yml")
  )
  message("Landscape rasters written to ", dest)
  invisible(paths)
}

#----------------------------------------------------------#
# End tool -----
#----------------------------------------------------------#

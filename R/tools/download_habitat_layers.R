#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#     Tool - Download the habitat (VMB) and ecosystem (KVES) layers
#
#
#----------------------------------------------------------#
#
# NOT part of the cascade. Run it by hand, deliberately, when the local copies
# of the two layers should be made or brought up to date:
#
#   source("R/00_setup.R")
#   source("R/tools/download_habitat_layers.R")
#   download_habitat_layers()
#
# It needs the sites of step 01 (Data/Processed/common/sites.gpkg). Then
# re-run the cascade from step 05 and diff the outputs
# (R/tools/compare_snapshots.R).
#
# The download follows the alcon-spatial project. AOPK ČR publishes both
# layers on data.nature.cz as one zip archive per kraj, clipped at the kraj
# boundary (CC BY 4.0, "(c) AOPK ČR"):
#
#   VMB   the habitat mapping: segments, and a table of the habitats mapped
#         in each segment with their share (ds/21)
#   KVES  the consolidated ecosystem layer, wall to wall (ds/145)
#
# 1. Only the archives of the kraje within `extract_margin_m` of a site are
#    downloaded (config.yml `habitat: download`). They are kept in
#    Data/Cache/aopk (several GB, not tracked by git), each with a .json
#    giving its URL, server date and md5. An archive already there is not
#    downloaded again unless `refresh = TRUE`.
# 2. Of each kraj only the features within `extract_margin_m` of a site are
#    kept. They are cached per kraj in Data/Cache/aopk/subsets and extracted
#    again only when an archive or the sites change.
# 3. The kraje are put together, cut to the area within the margin of the
#    sites, and written as the two layers the cascade reads:
#      config.yml habitat: layer        VMB, the habitat input contract
#      config.yml habitat: kves_layer   KVES
#    and RETRIEVED.yml next to them records what was fetched and when.
#
# The downloads need an ISOP login. The NDOP account is an ISOP account, so
# its credentials are reused. They are read from the environment, never
# written into this file and never printed. Put them in ~/.Renviron:
#
#   NDOP_USER=your_username
#   NDOP_PWD=your_password
#
#----------------------------------------------------------#

#--------------------------------------------------#
## data.nature.cz -----
#--------------------------------------------------#

#' Open a logged-in session on data.nature.cz.
#'
#' The credentials are removed from the handle after the login; the session
#' cookie stays.
#'
#' @return a curl handle carrying the session cookie
aopk_session <- function(dl = CONFIG$habitat$download) {
  user <- Sys.getenv("NDOP_USER")
  password <- Sys.getenv("NDOP_PWD")
  if (!nzchar(user) || !nzchar(password)) {
    stop("Set NDOP_USER and NDOP_PWD in ~/.Renviron before running this.", call. = FALSE)
  }
  handle <- curl::new_handle(followlocation = TRUE, cookiefile = "")
  curl::handle_setform(
    handle,
    isop_user = user, isop_password = password,
    appid = as.character(dl$login_appid), appurlid = "1", isop_login = ""
  )
  curl::curl_fetch_memory(dl$login_url, handle = handle)
  curl::handle_reset(handle)
  curl::handle_setopt(handle, followlocation = TRUE)

  # The download links are shown only to a logged-in user.
  if (!length(aopk_download_list(dl$vmb$dataset, handle))) {
    stop("The data.nature.cz login failed (credentials not shown).", call. = FALSE)
  }
  handle
}

#' The archives a dataset's download list offers to a logged-in user.
#'
#' @return file names, e.g. "Zlinsky.zip"
aopk_download_list <- function(dataset_url, handle) {
  res <- curl::curl_fetch_memory(paste0(dataset_url, "/download-list"), handle = handle)
  html <- rawToChar(res$content)
  links <- regmatches(html, gregexpr("/download/kraj/[^\"'<> ]+\\.zip", html))[[1]]
  unique(basename(links))
}

#' Download one archive into the cache.
#'
#' An expired session gets the login page instead of the zip, so the archive
#' is opened before it is moved into place. A .json next to it records the
#' URL, the server date, the size, the md5 and the time of the download.
aopk_download <- function(url, dest, handle) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  part <- paste0(dest, ".part")
  on.exit(unlink(part), add = TRUE)
  message("  downloading ", url)
  res <- curl::curl_fetch_disk(url, part, handle = handle)
  headers <- curl::parse_headers_list(res$headers)
  is_zip <- res$status_code == 200 &&
    !inherits(try(utils::unzip(part, list = TRUE), silent = TRUE), "try-error")
  if (!is_zip) {
    stop(
      "No zip archive received from ", url, " (HTTP ", res$status_code,
      "); the session may have expired.",
      call. = FALSE
    )
  }
  file.rename(part, dest)
  jsonlite::write_json(
    list(
      url = url,
      last_modified_server = headers[["last-modified"]],
      size_bytes = file.size(dest),
      md5 = unname(tools::md5sum(dest)),
      downloaded = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    ),
    sub("\\.zip$", ".json", dest),
    auto_unbox = TRUE, pretty = TRUE
  )
  invisible(dest)
}

#' The metadata written next to a cached archive.
archive_metadata <- function(zip) {
  jsonlite::read_json(sub("\\.zip$", ".json", zip))
}

#--------------------------------------------------#
## Which kraje -----
#--------------------------------------------------#

#' The kraje of config.yml with the URL and cache path of their archives.
aopk_kraje <- function(dl = CONFIG$habitat$download, cache_dir = file.path(CONFIG$paths$cache, "aopk")) {
  tibble::tibble(
    nuts3 = names(dl$kraje),
    vmb_stem = vapply(dl$kraje, `[[`, character(1), "vmb"),
    kves_stem = vapply(dl$kraje, `[[`, character(1), "kves")
  ) %>%
    dplyr::mutate(
      vmb_url = sprintf(dl$vmb$archive, dl$vmb$dataset, vmb_stem),
      kves_url = sprintf(dl$kves$archive, dl$kves$dataset, kves_stem),
      vmb_zip = file.path(cache_dir, "vmb", basename(vmb_url)),
      kves_zip = file.path(cache_dir, "kves", basename(kves_url))
    )
}

#' The archives of a set of kraje, downloading those not yet in the cache.
#'
#' Every archive is first looked up in the dataset's download list, so that a
#' wrong file name in config.yml stops before anything is downloaded. A failed
#' download is retried once after a new login.
#'
#' @param codes    NUTS3 codes of the kraje
#' @param refresh  TRUE downloads the archives already in the cache again
#' @return the rows of aopk_kraje() for those kraje
aopk_archives <- function(codes, dl = CONFIG$habitat$download, refresh = FALSE) {
  archives <- aopk_kraje(dl) %>% dplyr::filter(nuts3 %in% codes)
  unknown <- setdiff(codes, archives$nuts3)
  if (length(unknown)) {
    stop("No archives configured for the kraj (kraje) ", paste(unknown, collapse = ", "),
      " (config.yml habitat: download: kraje).",
      call. = FALSE
    )
  }

  todo <- tibble::tibble(
    url = c(archives$vmb_url, archives$kves_url),
    dest = c(archives$vmb_zip, archives$kves_zip)
  ) %>%
    dplyr::filter(refresh | !file.exists(dest))
  if (!nrow(todo)) {
    return(archives)
  }

  handle <- aopk_session(dl)
  offered <- c(
    aopk_download_list(dl$vmb$dataset, handle),
    aopk_download_list(dl$kves$dataset, handle)
  )
  not_offered <- setdiff(basename(todo$url), offered)
  if (length(not_offered)) {
    stop(
      "data.nature.cz does not offer ", paste(not_offered, collapse = ", "),
      ". It offers: ", paste(sort(offered), collapse = ", "),
      ". Correct config.yml habitat: download: kraje.",
      call. = FALSE
    )
  }

  for (i in seq_len(nrow(todo))) {
    tryCatch(
      aopk_download(todo$url[i], todo$dest[i], handle),
      error = function(e) {
        message("  retrying after a new login: ", conditionMessage(e))
        handle <<- aopk_session(dl)
        aopk_download(todo$url[i], todo$dest[i], handle)
      }
    )
  }
  archives
}

#--------------------------------------------------#
## The features near the sites, per kraj -----
#--------------------------------------------------#

#' Read a cached GeoPackage layer with its geometry column named "geometry",
#' as the shapefiles have it.
read_cached_layer <- function(gpkg, layer) {
  sf::st_read(gpkg, layer, quiet = TRUE) %>%
    dplyr::rename(geometry = geom) %>%
    use_working_crs()
}

#' Read a table without geometry (a .dbf, or a GeoPackage attribute table).
#'
#' sf warns that there is no geometry, which is expected here.
read_attribute_table <- function(dsn, ...) {
  tibble::as_tibble(suppressWarnings(sf::st_read(dsn, ..., quiet = TRUE)))
}

#' The VMB segments, VMB habitat rows and KVES polygons of one kraj that lie
#' within the margin of a site.
#'
#' The result is cached per kraj and rebuilt only when an archive or the
#' filter changes. To build it, the archives are unzipped into the cache,
#' read with a spatial filter and deleted again: alcon-spatial found reading
#' through /vsizip/ 35 to more than 50 times slower, and one kraj of KVES
#' takes 1-2 GB unzipped. Invalid geometries are repaired before caching.
#'
#' @param arch         one row of aopk_archives()
#' @param filter_geom  the sites of the kraj, buffered by the margin (sfc)
#' @return list(segments = sf, biotopes = data frame, kves = sf, repaired =
#'         named integer)
kraj_habitat_subset <- function(arch, filter_geom, dl = CONFIG$habitat$download,
                                cache_dir = file.path(CONFIG$paths$cache, "aopk")) {
  gpkg <- file.path(cache_dir, "subsets", paste0(arch$nuts3, ".gpkg"))
  meta_path <- sub("\\.gpkg$", ".json", gpkg)
  # cache_format: raise it whenever the content of the cached subsets changes.
  key <- list(
    cache_format = 1L,
    vmb_md5 = archive_metadata(arch$vmb_zip)$md5,
    kves_md5 = archive_metadata(arch$kves_zip)$md5,
    filter_hash = rlang::hash(sf::st_as_binary(filter_geom))
  )
  cached <- file.exists(gpkg) && file.exists(meta_path) &&
    identical(jsonlite::read_json(meta_path)$key, key)

  if (!cached) {
    message("  ", arch$nuts3, ": extracting the features near the sites")
    tmp <- file.path(cache_dir, "unzipped", arch$nuts3)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    utils::unzip(arch$vmb_zip, exdir = file.path(tmp, "vmb"))
    utils::unzip(arch$kves_zip, exdir = file.path(tmp, "kves"))
    find_file <- function(dir, pattern) {
      hit <- list.files(file.path(tmp, dir), pattern, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(hit) != 1) {
        stop(arch$nuts3, ": expected one file matching ", pattern, " in the ", dir, " archive, found ",
          length(hit),
          call. = FALSE
        )
      }
      hit
    }
    wkt <- sf::st_as_text(filter_geom)
    vmb_encoding <- paste0("ENCODING=", dl$vmb$encoding)

    segments <- sf::st_read(
      find_file("vmb", "^[^_]+\\.shp$"),
      wkt_filter = wkt, options = vmb_encoding, quiet = TRUE
    ) %>%
      dplyr::select(SEGMENT_ID, FSB, BIOTOP_SEZ, ROK_AKT, DATUM) %>%
      use_working_crs() %>%
      make_valid_polygons()
    biotopes <- read_attribute_table(find_file("vmb", "_Biotop\\.dbf$"), options = vmb_encoding) %>%
      dplyr::select(SEGMENT_ID, B_ID, BIOTOP, STEJ_PR, REPRE, ZACH, KVALITA, DG, RB) %>%
      dplyr::filter(SEGMENT_ID %in% segments$layer$SEGMENT_ID)
    kves <- sf::st_read(
      find_file("kves", "\\.shp$"),
      wkt_filter = wkt, options = paste0("ENCODING=", dl$kves$encoding),
      fid_column_name = "kves_fid", quiet = TRUE
    ) %>%
      dplyr::select(kves_fid, KOD, KATEGORIE) %>%
      use_working_crs() %>%
      make_valid_polygons()

    dir.create(dirname(gpkg), recursive = TRUE, showWarnings = FALSE)
    unlink(gpkg)
    sf::st_write(segments$layer, gpkg, layer = "vmb_segments", quiet = TRUE)
    sf::st_write(kves$layer, gpkg, layer = "kves", quiet = TRUE)
    sf::st_write(biotopes, gpkg, layer = "vmb_biotopes", quiet = TRUE)
    jsonlite::write_json(
      list(
        key = key, nuts3 = arch$nuts3,
        archives = c(arch$vmb_url, arch$kves_url),
        n_vmb_segments = nrow(segments$layer), n_vmb_biotopes = nrow(biotopes),
        n_kves = nrow(kves$layer),
        repaired = list(vmb_segments = segments$repaired, kves = kves$repaired),
        created = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
      ),
      meta_path,
      auto_unbox = TRUE, pretty = TRUE
    )
  }

  meta <- jsonlite::read_json(meta_path)
  segments <- read_cached_layer(gpkg, "vmb_segments")
  kves <- read_cached_layer(gpkg, "kves")
  segments$NUTS3 <- arch$nuts3
  kves$NUTS3 <- arch$nuts3
  list(
    segments = segments,
    biotopes = read_attribute_table(gpkg, "vmb_biotopes"),
    kves = kves,
    repaired = unlist(meta$repaired)
  )
}

#--------------------------------------------------#
## The tool -----
#--------------------------------------------------#

#' Download the VMB and KVES archives of the kraje near the sites and write
#' the two local layers the cascade reads.
#'
#' @param refresh  TRUE downloads the archives already in the cache again
#' @return the paths of the two layers, invisibly
download_habitat_layers <- function(refresh = FALSE,
                                    dl = CONFIG$habitat$download,
                                    vmb_path = CONFIG$habitat$layer,
                                    kves_path = CONFIG$habitat$kves_layer) {
  margin <- dl$extract_margin_m
  sites <- read_sites() %>% use_working_crs()
  # One polygon per cluster of sites, with the margin around it.
  near <- sf::st_cast(sf::st_union(sf::st_buffer(sf::st_geometry(sites), margin)), "POLYGON")

  # The kraj boundaries come from RCzechia, which downloads them.
  kraje <- RCzechia::kraje(resolution = "high") %>%
    sf::st_transform(sf::st_crs(sites))
  codes <- sort(kraje$KOD_CZNUTS3[lengths(sf::st_intersects(kraje, near)) > 0])
  message("Kraje within ", margin, " m of a site: ", paste(codes, collapse = ", "))
  archives <- aopk_archives(codes, dl, refresh = refresh)

  subsets <- lapply(seq_len(nrow(archives)), function(i) {
    arch <- archives[i, ]
    kraj <- sf::st_geometry(kraje[kraje$KOD_CZNUTS3 == arch$nuts3, ])
    # The archive holds only the kraj's own features, so the filter need not
    # be clipped to the kraj; it only has to leave out the sites far from it.
    filter_geom <- sf::st_union(near[lengths(sf::st_intersects(near, kraj)) > 0])
    kraj_habitat_subset(arch, filter_geom, dl)
  })

  message("Putting the kraje together")
  vmb <- build_habitat_segments(
    dplyr::bind_rows(lapply(subsets, `[[`, "segments")),
    dplyr::bind_rows(lapply(subsets, `[[`, "biotopes"))
  )
  if (vmb$share_mismatch > 0) {
    warning(
      vmb$share_mismatch, " VMB segment(s) have habitat shares (STEJ_PR) that do not add up to 100.",
      call. = FALSE
    )
  }
  kves <- build_kves_layer(dplyr::bind_rows(lapply(subsets, `[[`, "kves")))
  repaired <- Reduce(`+`, lapply(subsets, `[[`, "repaired"))

  message("Cutting the layers to ", margin, " m around the sites")
  vmb$layer <- clip_to_area(vmb$layer, near)
  kves <- clip_to_area(kves, near)

  for (path in c(vmb_path, kves_path)) dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  sf::st_write(vmb$layer, vmb_path, layer = "habitat_segments", delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(kves, kves_path, layer = "kves", delete_dsn = TRUE, quiet = TRUE)
  # The cascade reads the VMB layer through the contract check.
  read_habitat_layer(vmb_path)

  #--------------------------------------------------#
  ## Record what was fetched -----
  #--------------------------------------------------#

  archive_log <- function(zips) {
    stats::setNames(lapply(zips, function(zip) {
      meta <- archive_metadata(zip)
      list(url = meta$url, last_modified = meta$last_modified_server, md5 = meta$md5, downloaded = meta$downloaded)
    }), archives$nuts3)
  }
  common <- list(
    retrieved = format(Sys.Date()),
    licence = dl$licence,
    extract_margin_m = margin,
    sites = nrow(sites),
    kraje = archives$nuts3
  )
  retrieved <- list(
    habitat_segments = c(
      list(source = dl$vmb$dataset, file = basename(vmb_path)),
      common,
      list(
        features = nrow(vmb$layer),
        segments = dplyr::n_distinct(vmb$layer$SEGMENT_ID),
        segments_merged_across_kraj_lines = vmb$split,
        segments_with_shares_not_adding_to_100 = vmb$share_mismatch,
        geometries_repaired = unname(repaired["vmb_segments"]),
        size_mb = round(file.size(vmb_path) / 1e6, 2),
        note = paste(
          "One feature per segment x mapped habitat, cut to the area within extract_margin_m",
          "of a site; SHAPE_Area is the area of the whole segment. HABITAT (the Natura 2000",
          "code) is not published and is empty. FSB_EVAL is derived (habitat_build.R). The",
          "mappers' names and free-text notes are not kept."
        ),
        archives = archive_log(archives$vmb_zip)
      )
    ),
    kves = c(
      list(source = dl$kves$dataset, file = basename(kves_path)),
      common,
      list(
        features = nrow(kves),
        geometries_repaired = unname(repaired["kves"]),
        size_mb = round(file.size(kves_path) / 1e6, 2),
        note = "Polygons cut to the area within extract_margin_m of a site.",
        archives = archive_log(archives$kves_zip)
      )
    )
  )
  yaml::write_yaml(retrieved, file.path(dirname(vmb_path), "RETRIEVED.yml"))

  message(
    "Written ", vmb_path, " (", nrow(vmb$layer), " features, ", dplyr::n_distinct(vmb$layer$SEGMENT_ID),
    " segments) and ",
    kves_path, " (", nrow(kves), " polygons)"
  )
  invisible(c(vmb = vmb_path, kves = kves_path))
}

#' Empty the temporary data folder.
#'
#' Also lived in the old config.R, where it ran on every source() and deleted
#' Data/Temp without being asked.
clear_temp_dir <- function(temp_dir = CONFIG$paths$temp) {
  if (!dir.exists(temp_dir)) {
    return(invisible(FALSE))
  }

  files_to_delete <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)
  file.remove(files_to_delete[!dir.exists(files_to_delete)])

  dirs_to_delete <- list.dirs(temp_dir, recursive = TRUE, full.names = TRUE)
  # Deepest first, so a folder is empty by the time it is removed.
  dirs_to_delete <- dirs_to_delete[order(nchar(dirs_to_delete), decreasing = TRUE)]

  for (d in dirs_to_delete) {
    unlink(d, recursive = TRUE, force = TRUE)
  }

  invisible(TRUE)
}

#----------------------------------------------------------#
# End tool -----
#----------------------------------------------------------#

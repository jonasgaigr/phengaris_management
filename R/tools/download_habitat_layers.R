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
# of the layers should be made or brought up to date:
#
#   source("R/00_setup.R")
#   source("R/tools/download_habitat_layers.R")
#   download_habitat_layers()
#
# It needs the sites of step 01 (Data/Processed/common/sites.gpkg). Then
# re-run the cascade from step 05 and diff the outputs
# (R/tools/compare_snapshots.R).
#
# The download follows the alcon-spatial project. AOPK ČR publishes the
# layers on data.nature.cz as one zip archive per kraj, clipped at the kraj
# boundary (CC BY 4.0, "(c) AOPK ČR"):
#
#   VMB   the habitat mapping: segments, and a table of the habitats mapped
#         in each segment with their share (ds/21)
#   KVES  the consolidated ecosystem layer, wall to wall, in the versions of
#         config.yml `habitat: download: kves` (2021, 2022, 2023, 2025)
#
# 1. Only the archives of the kraje within `habitat: margin_m` of a site are
#    downloaded. They are kept in Data/Cache/aopk (vmb/, kves/<version>/;
#    about 20 GB, not tracked by git), each with a .json giving its URL,
#    server date and md5. An archive already there is not downloaded again
#    unless `refresh = TRUE`.
# 2. Of each archive only the features within the margin of a site are kept.
#    They are cached in Data/Cache/aopk/subsets and extracted again only when
#    the archive or the sites change.
# 3. The kraje are put together, cut to the area within the margin of the
#    sites, and written as the layers the cascade reads:
#      config.yml habitat: layer         VMB, the habitat input contract
#      config.yml habitat: kves_layers   one KVES layer per version
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

#' The kraj archives a dataset's download list offers to a logged-in user.
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
## Which archives -----
#--------------------------------------------------#

#' Every archive config.yml describes: one row per kraj x layer.
#'
#' @return tibble with nuts3, layer ("vmb", "kves_2021", ...), version (NA
#'         for VMB), dataset, url, zip (the cache path) and encoding
aopk_archive_table <- function(dl = CONFIG$habitat$download, cache_dir = file.path(CONFIG$paths$cache, "aopk")) {
  kraje <- tibble::tibble(
    nuts3 = names(dl$kraje),
    stem = vapply(dl$kraje, `[[`, character(1), "stem"),
    stem_kraj = vapply(dl$kraje, `[[`, character(1), "stem_kraj")
  )
  layers <- c(list(vmb = dl$vmb), stats::setNames(dl$kves, paste0("kves_", names(dl$kves))))
  dplyr::bind_rows(lapply(names(layers), function(layer) {
    d <- layers[[layer]]
    version <- if (layer == "vmb") NA_character_ else sub("^kves_", "", layer)
    folder <- if (layer == "vmb") "vmb" else file.path("kves", version)
    url <- sprintf(d$archive, d$dataset, kraje[[d$stem]])
    tibble::tibble(
      nuts3 = kraje$nuts3, layer = layer, version = version, dataset = d$dataset,
      url = url, zip = file.path(cache_dir, folder, basename(url)), encoding = d$encoding
    )
  }))
}

#' The archives of a set of kraje, downloading those not yet in the cache.
#'
#' Every archive is first looked up in its dataset's download list, so that a
#' wrong file name in config.yml stops before anything is downloaded. A failed
#' download is retried once after a new login.
#'
#' @param codes    NUTS3 codes of the kraje
#' @param refresh  TRUE downloads the archives already in the cache again
#' @return the rows of aopk_archive_table() for those kraje
aopk_archives <- function(codes, dl = CONFIG$habitat$download, refresh = FALSE) {
  archives <- aopk_archive_table(dl) %>% dplyr::filter(nuts3 %in% codes)
  unknown <- setdiff(codes, archives$nuts3)
  if (length(unknown)) {
    stop("No archives configured for the kraj (kraje) ", paste(unknown, collapse = ", "),
      " (config.yml habitat: download: kraje).",
      call. = FALSE
    )
  }

  todo <- archives %>% dplyr::filter(refresh | !file.exists(zip))
  if (!nrow(todo)) {
    return(archives)
  }

  handle <- aopk_session(dl)
  for (dataset in unique(todo$dataset)) {
    not_offered <- setdiff(basename(todo$url[todo$dataset == dataset]), aopk_download_list(dataset, handle))
    if (length(not_offered)) {
      stop(
        dataset, " does not offer ", paste(not_offered, collapse = ", "),
        ". Correct config.yml habitat: download.",
        call. = FALSE
      )
    }
  }

  for (i in seq_len(nrow(todo))) {
    tryCatch(
      aopk_download(todo$url[i], todo$zip[i], handle),
      error = function(e) {
        message("  retrying after a new login: ", conditionMessage(e))
        handle <<- aopk_session(dl)
        aopk_download(todo$url[i], todo$zip[i], handle)
      }
    )
  }
  archives
}

#--------------------------------------------------#
## The features near the sites, per archive -----
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

#' The features of one archive that lie within the margin of a site.
#'
#' For VMB, the segments and the habitat rows of their segments; for KVES,
#' the ecosystem polygons. The result is cached per archive and rebuilt only
#' when the archive or the filter changes. To build it, the archive is
#' unzipped into the cache, read with a spatial filter and deleted again:
#' alcon-spatial found reading through /vsizip/ 35 to more than 50 times
#' slower, and one kraj of KVES takes 1-2 GB unzipped. Invalid geometries are
#' repaired before caching.
#'
#' @param arch         one row of aopk_archives()
#' @param filter_geom  the sites near the kraj, buffered by the margin (sfc)
#' @return for VMB list(segments = sf, biotopes = data frame), for KVES
#'         list(kves = sf); each with `repaired`, the number of geometries
#'         repaired, and NUTS3 set on the features
archive_subset <- function(arch, filter_geom, cache_dir = file.path(CONFIG$paths$cache, "aopk")) {
  gpkg <- file.path(cache_dir, "subsets", paste0(arch$layer, "_", arch$nuts3, ".gpkg"))
  meta_path <- sub("\\.gpkg$", ".json", gpkg)
  # cache_format: raise it whenever the content of the cached subsets changes.
  key <- list(
    cache_format = 2L,
    md5 = archive_metadata(arch$zip)$md5,
    filter_hash = rlang::hash(sf::st_as_binary(filter_geom))
  )
  cached <- file.exists(gpkg) && file.exists(meta_path) &&
    identical(jsonlite::read_json(meta_path)$key, key)
  is_vmb <- arch$layer == "vmb"

  if (!cached) {
    message("  ", arch$layer, " ", arch$nuts3, ": extracting the features near the sites")
    tmp <- file.path(cache_dir, "unzipped", paste0(arch$layer, "_", arch$nuts3))
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    utils::unzip(arch$zip, exdir = tmp)
    find_file <- function(pattern) {
      hit <- list.files(tmp, pattern, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(hit) != 1) {
        stop(arch$zip, ": expected one file matching ", pattern, ", found ", length(hit), call. = FALSE)
      }
      hit
    }
    wkt <- sf::st_as_text(filter_geom)
    encoding <- paste0("ENCODING=", arch$encoding)

    dir.create(dirname(gpkg), recursive = TRUE, showWarnings = FALSE)
    unlink(gpkg)
    if (is_vmb) {
      segments <- sf::st_read(find_file("^[^_]+\\.shp$"), wkt_filter = wkt, options = encoding, quiet = TRUE) %>%
        dplyr::select(SEGMENT_ID, FSB, BIOTOP_SEZ, ROK_AKT, DATUM) %>%
        use_working_crs() %>%
        make_valid_polygons()
      biotopes <- read_attribute_table(find_file("_Biotop\\.dbf$"), options = encoding) %>%
        dplyr::select(SEGMENT_ID, B_ID, BIOTOP, STEJ_PR, REPRE, ZACH, KVALITA, DG, RB) %>%
        dplyr::filter(SEGMENT_ID %in% segments$layer$SEGMENT_ID)
      sf::st_write(segments$layer, gpkg, layer = "vmb_segments", quiet = TRUE)
      sf::st_write(biotopes, gpkg, layer = "vmb_biotopes", quiet = TRUE)
      repaired <- segments$repaired
      counts <- list(n_vmb_segments = nrow(segments$layer), n_vmb_biotopes = nrow(biotopes))
    } else {
      kves <- sf::st_read(
        find_file("\\.shp$"),
        wkt_filter = wkt, options = encoding, fid_column_name = "kves_fid", quiet = TRUE
      )
      # The versions before 2025 have no KOD.
      kves <- kves[intersect(c("kves_fid", "KOD", "KATEGORIE"), names(kves))] %>%
        use_working_crs() %>%
        make_valid_polygons()
      sf::st_write(kves$layer, gpkg, layer = "kves", quiet = TRUE)
      repaired <- kves$repaired
      counts <- list(n_kves = nrow(kves$layer))
    }
    jsonlite::write_json(
      c(
        list(key = key, nuts3 = arch$nuts3, layer = arch$layer, archive = arch$url),
        counts,
        list(repaired = repaired, created = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
      ),
      meta_path,
      auto_unbox = TRUE, pretty = TRUE
    )
  }

  repaired <- jsonlite::read_json(meta_path)$repaired
  if (is_vmb) {
    segments <- read_cached_layer(gpkg, "vmb_segments")
    segments$NUTS3 <- arch$nuts3
    list(segments = segments, biotopes = read_attribute_table(gpkg, "vmb_biotopes"), repaired = repaired)
  } else {
    kves <- read_cached_layer(gpkg, "kves")
    kves$NUTS3 <- arch$nuts3
    list(kves = kves, repaired = repaired)
  }
}

#--------------------------------------------------#
## The tool -----
#--------------------------------------------------#

#' Download the VMB and KVES archives of the kraje near the sites and write
#' the local layers the cascade reads.
#'
#' @param refresh  TRUE downloads the archives already in the cache again
#' @return the paths of the layers, invisibly
download_habitat_layers <- function(refresh = FALSE,
                                    dl = CONFIG$habitat$download,
                                    margin = CONFIG$habitat$margin_m,
                                    vmb_path = CONFIG$habitat$layer,
                                    kves_paths = CONFIG$habitat$kves_layers) {
  missing_paths <- setdiff(names(dl$kves), names(kves_paths))
  if (length(missing_paths)) {
    stop("No path in config.yml habitat: kves_layers for KVES ", paste(missing_paths, collapse = ", "), call. = FALSE)
  }
  sites <- read_sites() %>% use_working_crs()
  # One polygon per cluster of sites, with the margin around it.
  near <- sf::st_cast(sf::st_union(sf::st_buffer(sf::st_geometry(sites), margin)), "POLYGON")

  # The kraj boundaries come from RCzechia, which downloads them.
  kraje <- RCzechia::kraje(resolution = "high") %>%
    sf::st_transform(sf::st_crs(sites))
  codes <- sort(kraje$KOD_CZNUTS3[lengths(sf::st_intersects(kraje, near)) > 0])
  message("Kraje within ", margin, " m of a site: ", paste(codes, collapse = ", "))
  archives <- aopk_archives(codes, dl, refresh = refresh)

  # The archive holds only the kraj's own features, so the filter need not be
  # clipped to the kraj; it only has to leave out the sites far from it.
  filters <- lapply(stats::setNames(codes, codes), function(code) {
    kraj <- sf::st_geometry(kraje[kraje$KOD_CZNUTS3 == code, ])
    sf::st_union(near[lengths(sf::st_intersects(near, kraj)) > 0])
  })
  subsets_of <- function(layer) {
    rows <- archives[archives$layer == layer, ]
    lapply(seq_len(nrow(rows)), function(i) archive_subset(rows[i, ], filters[[rows$nuts3[i]]]))
  }
  archive_log <- function(layer) {
    rows <- archives[archives$layer == layer, ]
    stats::setNames(lapply(rows$zip, function(zip) {
      meta <- archive_metadata(zip)
      list(url = meta$url, last_modified = meta$last_modified_server, md5 = meta$md5, downloaded = meta$downloaded)
    }), rows$nuts3)
  }
  common <- list(
    retrieved = format(Sys.Date()),
    licence = dl$licence,
    margin_m = margin,
    sites = nrow(sites),
    kraje = codes
  )
  write_layer <- function(x, path, layer) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    sf::st_write(x, path, layer = layer, delete_dsn = TRUE, quiet = TRUE)
  }

  #--------------------------------------------------#
  ## VMB -----
  #--------------------------------------------------#

  message("VMB: putting the kraje together")
  subsets <- subsets_of("vmb")
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
  vmb$layer <- clip_to_area(vmb$layer, near)
  write_layer(vmb$layer, vmb_path, "habitat_segments")
  # The cascade reads the VMB layer through the contract check.
  read_habitat_layer(vmb_path)

  retrieved <- list(habitat_segments = c(
    list(source = dl$vmb$dataset, file = basename(vmb_path)),
    common,
    list(
      features = nrow(vmb$layer),
      segments = dplyr::n_distinct(vmb$layer$SEGMENT_ID),
      segments_merged_across_kraj_lines = vmb$split,
      segments_with_shares_not_adding_to_100 = vmb$share_mismatch,
      geometries_repaired = sum(unlist(lapply(subsets, `[[`, "repaired"))),
      size_mb = round(file.size(vmb_path) / 1e6, 2),
      note = paste(
        "One feature per segment x mapped habitat, cut to the area within margin_m of a",
        "site; SHAPE_Area is the area of the whole segment. HABITAT (the Natura 2000 code)",
        "is not published and is empty. FSB_EVAL is derived (habitat_build.R). The mappers'",
        "names and free-text notes are not kept."
      ),
      archives = archive_log("vmb")
    )
  ))
  rm(subsets, vmb)

  #--------------------------------------------------#
  ## KVES, version by version -----
  #--------------------------------------------------#

  # The versions with codes (2025) first, so that the others can take their
  # codes from them by the category name.
  versions <- names(dl$kves)
  versions <- versions[order(versions != max(versions))]
  kves_codes <- NULL
  for (version in versions) {
    message("KVES ", version, ": putting the kraje together")
    subsets <- subsets_of(paste0("kves_", version))
    kves <- build_kves_layer(dplyr::bind_rows(lapply(subsets, `[[`, "kves")), kves_codes)
    if (is.null(kves_codes) && !all(is.na(kves$layer$KOD))) {
      kves_codes <- sf::st_drop_geometry(kves$layer) %>% dplyr::distinct(KATEGORIE, KOD)
    }
    kves$layer <- clip_to_area(kves$layer, near)
    path <- kves_paths[[version]]
    write_layer(kves$layer, path, "kves")
    read_kves_layer(path)

    retrieved[[paste0("kves_", version)]] <- c(
      list(source = dl$kves[[version]]$dataset, file = basename(path)),
      common,
      list(
        features = nrow(kves$layer),
        categories = dplyr::n_distinct(kves$layer$KATEGORIE),
        categories_without_code = as.list(kves$uncoded),
        geometries_repaired = sum(unlist(lapply(subsets, `[[`, "repaired"))),
        size_mb = round(file.size(path) / 1e6, 2),
        note = "Polygons cut to the area within margin_m of a site.",
        archives = archive_log(paste0("kves_", version))
      )
    )
    message("  written ", path, " (", nrow(kves$layer), " polygons)")
    rm(subsets, kves)
  }

  yaml::write_yaml(retrieved, file.path(dirname(vmb_path), "RETRIEVED.yml"))
  message("Written ", vmb_path, " and ", length(versions), " KVES layers")
  invisible(c(vmb = vmb_path, unlist(kves_paths)))
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

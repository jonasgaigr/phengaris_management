#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#     Tool - Download the habitat mapping layers by region
#
#
#----------------------------------------------------------#
#
# NOT part of the cascade. Run it by hand only when the habitat mapping layers
# have to be fetched over HTTP instead of read from the //bali.nature.cz share,
# for example from outside the AOPK network.
#
#   Sys.setenv(AOPK_USER = "...", AOPK_PASSWORD = "...")
#   source("R/tools/download_habitat_layers.R")
#   download_habitat_layers()
#
# Credentials are read from the environment, never written into this file. Put
# them in ~/.Renviron so they are not typed into the console each time:
#
#   AOPK_USER=your_username
#   AOPK_PASSWORD=your_password
#
# This lived at the bottom of the old config.R, where it ran on every source()
# with placeholder credentials and so failed every time.
#
#----------------------------------------------------------#

#' Download and unpack the habitat mapping layers, one archive per region.
#'
#' @param dest_dir  where to unpack the archives
#' @param regions   archive names to fetch
#' @return the destination directory, invisibly
download_habitat_layers <- function(
  dest_dir = file.path(PATHS$temp, "Habitats"),
  regions = c(
    "Jihocesky.zip",
    "Jihomoravsky.zip",
    "Karlovarsky.zip",
    "Kralovehradecky.zip",
    "Liberecky.zip",
    "Moravskoslezsky.zip",
    "Olomoucky.zip",
    "Pardubicky.zip",
    "Plzensky.zip",
    "Praha.zip",
    "Stredocesky.zip",
    "Ustecky.zip",
    "Vysocina.zip",
    "Zlinsky.zip"
  )
) {

  username <- Sys.getenv("AOPK_USER")
  password <- Sys.getenv("AOPK_PASSWORD")

  if (!nzchar(username) || !nzchar(password)) {
    stop(
      "Set AOPK_USER and AOPK_PASSWORD in the environment before running this.",
      call. = FALSE
    )
  }

  login_url <- "https://cas.nature.cz/cas/login"
  url_base  <- "https://data.nature.cz/ds/21/download/kraj/"

  res_login <- httr::POST(
    login_url,
    body = list(username = username, password = password),
    encode = "form"
  )
  login_cookies <- httr::cookies(res_login)

  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  for (region in regions) {
    file_url   <- paste0(url_base, region)
    dest_file  <- file.path(dest_dir, region)
    unzip_dir  <- file.path(dest_dir, tools::file_path_sans_ext(region))
    if (!dir.exists(unzip_dir)) dir.create(unzip_dir, recursive = TRUE)

    message("Downloading: ", region, " ...")

    res <- httr::GET(
      file_url,
      httr::set_cookies(
        .cookies = stats::setNames(
          login_cookies$value,
          login_cookies$name
        )
      ),
      httr::write_disk(
        dest_file,
        overwrite = TRUE
      )
    )

    if (httr::status_code(res) == 200) {
      message("  downloaded, unzipping")
      utils::unzip(dest_file, exdir = unzip_dir)
      message("  done")
    } else {
      warning(
        "Failed to download ", region,
        " - HTTP status: ", httr::status_code(res),
        call. = FALSE
      )
    }
  }

  invisible(dest_dir)
}

#' Empty the temporary data folder.
#'
#' Also lived in the old config.R, where it ran on every source() and deleted
#' Data/Temp without being asked.
clear_temp_dir <- function(temp_dir = PATHS$temp) {
  if (!dir.exists(temp_dir)) return(invisible(FALSE))

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

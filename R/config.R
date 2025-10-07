#----------------------------------------------------------#
#
#
#       Sanguisorba Phengaris spp. management in Czechia
#
#                     Config file
#
#
#                      Jonáš Gaigr
#                         2025
#
#----------------------------------------------------------#

# --- 1. Install and activate renv --------------------------------------------
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# Initialize or restore existing environment
if (!file.exists("renv.lock")) {
  message("🧩 Initializing new renv environment...")
  renv::init(bare = TRUE)
} else {
  message("🔄 Restoring existing renv environment...")
  renv::restore(prompt = FALSE)
}

# --- 2. Install CRAN packages (only if missing) -------------------------------
cran_pkgs <- c(
  "tidyverse",
  "sf",
  "sp",
  "proj4",
  "openxlsx",
  "lmerTest",
  "vegan",
  "GLMMadaptive",
  "RCzechia",
  "rvest",
  "httr",
  "xml2"
)

to_install <- cran_pkgs[!cran_pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) renv::install(to_install)

# --- 3. Enforce compatible Matrix / lme4 versions -----------------------------
if (!"Matrix" %in% installed.packages()[, "Package"] ||
    packageVersion("Matrix") != "1.6-4") {
  renv::install("Matrix@1.6-4")
}

if (!"lme4" %in% installed.packages()[, "Package"] ||
    packageVersion("lme4") != "1.1-35.3") {
  renv::install("lme4@1.1-35.3")
}

# --- 4. Install GitHub packages ----------------------------------------------
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

github_pkgs <- list(
  rn2kcz = "jonasgaigr/rn2kcz",
  rndop  = "kalab-oto/rndop"
)

for (pkg in names(github_pkgs)) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    renv::install(github_pkgs[[pkg]])
  }
}

# --- 5. Snapshot environment (record exact versions) --------------------------
renv::snapshot(prompt = FALSE)

# --- 6. Load all packages -----------------------------------------------------
pkg_list <- c(cran_pkgs, "lme4", names(github_pkgs))
invisible(lapply(pkg_list, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))

message("✅ All packages loaded successfully and environment is reproducible.")

#----------------------------------------------------------#
# Load data -----
#----------------------------------------------------------#
#--------------------------------------------------#
## Load remote data -----
#--------------------------------------------------#
# Borders of Czechia
czechia_border <- 
  RCzechia::republika(
    resolution = "high"
    ) %>%
  sf::st_transform(
    ., 
    st_crs("+init=epsg:5514")
  ) 

# Data on protected areas and mapping fields
endpoint <- "http://gis.nature.cz/arcgis/services/Aplikace/Opendata/MapServer/WFSServer?"
caps_url <- base::paste0(endpoint, "request=GetCapabilities&service=WFS")

layer_name_evl <- "Opendata:Evropsky_vyznamne_lokality"
layer_name_mzchu <- "Opendata:Maloplosna_zvlaste_chranena_uzemi__MZCHU_"
layer_name_sitmap1rad <- "Opendata:Mapovaci_sit_-_deleni_1.radu"

getfeature_url_evl <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_evl
)
getfeature_url_mzchu <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_mzchu
)
getfeature_url_sitmap1rad <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_sitmap1rad
)


evl <- sf::st_read(getfeature_url_evl) %>%
  sf::st_transform(
    ., 
    st_crs("+init=epsg:5514")
    ) 
mzchu <- sf::st_read(getfeature_url_mzchu) %>%
  sf::st_transform(
    ., 
    st_crs("+init=epsg:5514")
    )
sitmap <- sf::st_read(getfeature_url_sitmap1rad) %>%
  sf::st_transform(
    ., 
    st_crs("+init=epsg:5514")
    ) %>%
  sf::st_crop(
    .,
    czechia_border  # crop by the border of Czechia
    )

# Sites where the species are target features
rn2kcz::load_n2k_sites()

#--------------------------------------------------#
## Load range layers -----
#--------------------------------------------------#
range_nausithous <- 
  sf::st_read(
    "Data/Input/Reporting_range/nausithous.shp"
  ) %>%
  sf::st_transform(5514) %>%
  sf::st_make_valid() %>%
  dplyr::mutate(
    row_n = dplyr::row_number()
    )

range_teleius <- 
  sf::st_read(
    "Data/Input/Reporting_range/teleius.shp"
  ) %>%
  sf::st_transform(5514) %>%
  sf::st_make_valid() %>%
  dplyr::mutate(
    row_n = dplyr::row_number()
  )

#--------------------------------------------------#
## Load species data 2012 - 2018 -----
#--------------------------------------------------#

data_Pnau_old <- 
  readr::read_csv2(
    "Data/Input/Phengaris_nausithous_2012_2018.csv",
    locale = locale(encoding = "Windows-1250")
  )
data_Ptel_old <- 
  readr::read_csv2(
    "Data/Input/Phengaris_teleius_2012_2018.csv",
    locale = locale(encoding = "Windows-1250")
  )

data_old <-
  dplyr::bind_rows(
    data_Pnau_old,
    data_Ptel_old
  )

lokal_Pnau_b_old <- 
  sf::st_read(
    "Data/Input/Phengaris_nausithous_2012_2018/w03_nd_lokalizace_b.shp"
    )  %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("POINT") %>%
  sf::st_make_valid()

lokal_Pnau_p_old <- sf::st_read(
  "Data/Input/Phengaris_nausithous_2012_2018/w03_nd_lokalizace_p.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("POLYGON") %>%
  sf::st_make_valid()

lokal_Pnau_l_old <- sf::st_read(
  "Data/Input/Phengaris_nausithous_2012_2018/w03_nd_lokalizace_l.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("LINESTRING") %>%
  sf::st_make_valid()

lokal_Ptel_b_old <- sf::st_read(
  "Data/Input/Phengaris_teleius_2012_2018/w03_nd_lokalizace_b.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("POINT") %>%
  sf::st_make_valid()

lokal_Ptel_p_old <- sf::st_read(
  "Data/Input/Phengaris_teleius_2012_2018/w03_nd_lokalizace_p.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("POLYGON") %>%
  sf::st_make_valid()

lokal_Ptel_l_old <- sf::st_read(
  "Data/Input/Phengaris_teleius_2012_2018/w03_nd_lokalizace_l.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("LINESTRING") %>%
  sf::st_make_valid()

lokal_old <- dplyr::bind_rows(
  lokal_Pnau_b_old, 
  lokal_Pnau_p_old, 
  lokal_Pnau_l_old,
  lokal_Ptel_b_old, 
  lokal_Ptel_p_old, 
  lokal_Ptel_l_old
) %>%
  dplyr::rename(
    ID_LOKAL = idx_nd_lok
  ) %>%
  sf::st_as_sf() %>%
  sf::st_make_valid()

phengaris_lokal_old <- 
  data_old %>%
  dplyr::left_join(
    ., 
    lokal_old
  ) %>%
  sf::st_as_sf() %>%
  sf::st_make_valid()

#--------------------------------------------------#
## Load species data 2019 - 2024 -----
#--------------------------------------------------#
data_Pnau_new <- 
  readr::read_csv2(
    "Data/Input/Phengaris_nausithous_2019_2024.csv",
    locale = locale(encoding = "Windows-1250")
    )
data_Ptel_new <- 
  readr::read_csv2(
    "Data/Input/Phengaris_teleius_2019_2024.csv",
    locale = locale(encoding = "Windows-1250")
    )

data_new <-
  dplyr::bind_rows(
    data_Pnau_new,
    data_Ptel_new
  )

lokal_Pnau_b_new <- sf::st_read(
  "Data/Input/Phengaris_nausithous_2019_2024/w03_nd_lokalizace_b.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("POINT") %>%
  sf::st_make_valid()

lokal_Pnau_p_new <- sf::st_read(
  "Data/Input/Phengaris_nausithous_2019_2024/w03_nd_lokalizace_p.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("POLYGON") %>%
  sf::st_make_valid()

lokal_Pnau_l_new <- sf::st_read(
  "Data/Input/Phengaris_nausithous_2019_2024/w03_nd_lokalizace_l.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("LINESTRING") %>%
  sf::st_make_valid()

lokal_Ptel_b_new <- sf::st_read(
  "Data/Input/Phengaris_teleius_2019_2024/w03_nd_lokalizace_b.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("POINT") %>%
  sf::st_make_valid()

lokal_Ptel_p_new <- sf::st_read(
  "Data/Input/Phengaris_teleius_2019_2024/w03_nd_lokalizace_p.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("POLYGON") %>%
  sf::st_make_valid()

lokal_Ptel_l_new <- sf::st_read(
  "Data/Input/Phengaris_teleius_2019_2024/w03_nd_lokalizace_l.shp"
  ) %>%
  sf::st_set_crs(5514) %>%  # assign the correct CRS
  sf::st_transform(5514) %>%
  sf::st_cast("LINESTRING") %>%
  sf::st_make_valid()

lokal_new <- dplyr::bind_rows(
  lokal_Pnau_b_new, 
  lokal_Pnau_p_new, 
  lokal_Pnau_l_new,
  lokal_Ptel_b_new, 
  lokal_Ptel_p_new, 
  lokal_Ptel_l_new
  ) %>%
  dplyr::rename(
    ID_LOKAL = idx_nd_lok
    ) %>%
  sf::st_as_sf() %>%
  sf::st_make_valid()

phengaris_lokal_new <- 
  data_new %>%
  dplyr::left_join(
    ., 
    lokal_new
    ) %>%
  sf::st_as_sf() %>%
  sf::st_make_valid()

#--------------------------------------------------#
## Load habitat layer -----
#--------------------------------------------------#
library(tools)
rndop::isop_login()

library(httr)
library(tools)

# Your login info (replace with your actual username and password)
username <- "your_username"
password <- "your_password"

# Login URL (adjust if needed)
login_url <- "https://cas.nature.cz/cas/login"

# Log in and get cookies
res_login <- POST(login_url, body = list(
  username = username,
  password = password
), encode = "form")

# Extract cookies from login response
login_cookies <- cookies(res_login)

# Base URL for ZIP files
url_base <- "https://data.nature.cz/ds/21/download/kraj/"

regions <- c(
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

# Destination directory
dest_dir <- "Data/Temp/Habitats"
if (!dir.exists(dest_dir)) dir.create(dest_dir)

for (region in regions) {
  file_url <- paste0(url_base, region)
  dest_file <- file.path(dest_dir, region)
  unzip_dir <- file.path(dest_dir, tools::file_path_sans_ext(region))
  if (!dir.exists(unzip_dir)) dir.create(unzip_dir)
  
  cat("Downloading:", region, "...\n")
  
  # Download with cookies
  res <- GET(
    file_url, 
    set_cookies(
      .cookies = setNames(
        login_cookies$value, 
        login_cookies$name
        )
      ),
    write_disk(
      dest_file, 
      overwrite = TRUE
      )
    )
  
  if (status_code(res) == 200) {
    cat("Downloaded", region, "\nUnzipping...\n")
    unzip(dest_file, exdir = unzip_dir)
    cat("Done\n")
  } else {
    cat("Failed to download", region, "- HTTP status:", status_code(res), "\n")
  }
}

#----------------------------------------------------------#
# List ZDROJ for analyis -----
#----------------------------------------------------------#
target_mon_zdroj <- c(
  "Kolektiv autorů (2017) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2018) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2019) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2020) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2021) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2020) Monitoring motýlů. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2021) Monitoring motýlů. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2022) Monitoring motýlů.",
  "Kolektiv autorů (2023) Monitoring motýlů.",
  "Kolektiv autorů (2024) Monitoring motýlů."
  )

#----------------------------------------------------------#
# Delete Temp files -----
#----------------------------------------------------------#
temp_dir <- "Data/Temp"

# List all files and folders inside, full paths
files_to_delete <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)

# Delete all files and folders
file.remove(files_to_delete[!dir.exists(files_to_delete)])  # Remove files only

# Remove directories (empty after deleting files inside)
dirs_to_delete <- list.dirs(temp_dir, recursive = TRUE, full.names = TRUE)
# Sort in decreasing order to delete subfolders before parents
dirs_to_delete <- dirs_to_delete[order(nchar(dirs_to_delete), decreasing = TRUE)]

# Remove directories
for (d in dirs_to_delete) {
  unlink(d, recursive = TRUE, force = TRUE)
}

#----------------------------------------------------------#
# End config -----
#----------------------------------------------------------#
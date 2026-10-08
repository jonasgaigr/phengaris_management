#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#     Evidence for REVIEW.md (Phase 1, commit b8d3297)
#
#----------------------------------------------------------#
#
# Recomputes every number quoted in REVIEW.md from the raw exports in
# Data/Input and the processed tables of the clean-up branch. Not part of the
# cascade, and it writes nothing into the project. Run from the project root:
#
#   Rscript review/review_evidence.R > review/review_evidence.txt
#
# The sections are the ad-hoc scripts the review was written from, kept in the
# order they were run; later sections reuse two cached objects (tm.rds,
# kv.rds) that earlier sections write into a temporary directory.
#
#----------------------------------------------------------#

Sys.setenv(SP = tempdir())


#----------------------------------------------------------#
# Section r01b_coords -----
#----------------------------------------------------------#

suppressPackageStartupMessages(library(magrittr))
f <- c("Data/Input/Phengaris_nausithous_2019_2024.csv", "Data/Input/Phengaris_teleius_2019_2024.csv")
loc <- readr::locale(encoding = "Windows-1250")
old <- dplyr::bind_rows(lapply(f, function(x) suppressMessages(suppressWarnings(readr::read_csv2(x, locale = loc, show_col_types = FALSE)))))
raw <- dplyr::bind_rows(lapply(f, function(x) readr::read_delim(x, delim = ";", locale = loc, col_types = readr::cols(.default = "c"))))
cat("rows:", nrow(raw), "\n")
ndec <- function(s) ifelse(grepl("[.]", s), nchar(sub(".*[.]", "", s)), 0)
for (v in c("X", "Y")) {
  cat(v, "decimals in raw:\n"); print(table(ndec(raw[[v]]), useNA = "ifany"))
  good <- as.numeric(raw[[v]])
  ratio <- old[[v]] / good
  cat(v, "ratio read_csv2/correct:\n"); print(table(round(ratio, 6), useNA = "ifany"))
  cat(v, "range correct:", range(good, na.rm = TRUE), " range read_csv2:", range(old[[v]], na.rm = TRUE), "\n")
}
num_cols <- names(old)[sapply(old, is.numeric)]
for (v in num_cols) {
  s <- raw[[v]]; s <- s[!is.na(s)]
  has_dot <- sum(grepl("[.]", s)); has_comma <- sum(grepl(",", s))
  mism <- sum(abs(old[[v]] - suppressWarnings(as.numeric(raw[[v]]))) > 1e-9, na.rm = TRUE)
  na_new <- sum(is.na(old[[v]]) & !is.na(raw[[v]]))
  cat(sprintf("%-14s n=%5d dot=%5d comma=%4d mismatch=%d lost_to_NA=%d\n", v, length(s), has_dot, has_comma, mism, na_new))
}
cat("\nPOCET raw values with non-digits:\n"); print(head(unique(raw$POCET[grepl("[^0-9]", raw$POCET)]), 30))
cat("\nODHAD values:\n"); print(table(raw$ODHAD, useNA = "ifany"))
cat("\nPOCITANO:\n"); print(table(raw$POCITANO, useNA = "ifany"))
cat("\nREL_POC:\n"); print(head(sort(table(raw$REL_POC), decreasing = TRUE), 20))
cat("DATUM_OD == DATUM_DO share:", mean(raw$DATUM_OD == raw$DATUM_DO, na.rm = TRUE), "\n")
cat("CXPRESNOST:\n"); print(head(sort(table(raw$CXPRESNOST), decreasing = TRUE), 15))
cat("CAS parse problems: raw non-NA", sum(!is.na(raw$CAS)), " old non-NA", sum(!is.na(old$CAS)), "\n")
print(head(unique(raw$CAS[is.na(old$CAS) & !is.na(raw$CAS)])))
for (v in c("POKRYVN", "PATROVITOST", "FENOFAZE", "KODHN", "GAR_POZN")) cat(v, "raw non-NA:", sum(!is.na(raw[[v]])), " old non-NA:", sum(!is.na(old[[v]])), "\n")

#----------------------------------------------------------#
# Section r02_processed -----
#----------------------------------------------------------#

suppressPackageStartupMessages(library(magrittr))
SPECIES_NAU <- "Phengaris nausithous"; SPECIES_TEL <- "Phengaris teleius"
d <- readr::read_csv("Data/Processed/data_analysis.csv", show_col_types = FALSE, guess_max = 1e5)
wi <- readr::read_csv("Data/Processed/data_with_imputed.csv", show_col_types = FALSE, guess_max = 1e5)
dc <- readr::read_csv("Data/Processed/data_clean.csv", show_col_types = FALSE, guess_max = 1e5)
cat("data_with_imputed:", nrow(wi), " data_clean:", nrow(dc), " data_analysis:", nrow(d), "\n")
ndec <- function(x) { s <- format(x, scientific = FALSE, trim = TRUE); 0 }
# decimals of X in analysis: infer ratio by comparing to raw file
loc <- readr::locale(encoding = "Windows-1250")
raw <- dplyr::bind_rows(lapply(c("Data/Input/Phengaris_nausithous_2019_2024.csv", "Data/Input/Phengaris_teleius_2019_2024.csv"),
  function(x) readr::read_delim(x, delim = ";", locale = loc, col_types = readr::cols(.default = "c"))))
raw_xy <- raw %>% dplyr::transmute(ID_NALEZ = as.numeric(ID_NALEZ), DRUH, Xr = X) %>% dplyr::distinct()
cat("ID_NALEZ unique in raw:", !anyDuplicated(raw_xy$ID_NALEZ), "\n")
dd <- d %>% dplyr::left_join(raw_xy %>% dplyr::select(ID_NALEZ, Xr), by = "ID_NALEZ")
nd <- ifelse(grepl("[.]", dd$Xr), nchar(sub(".*[.]", "", dd$Xr)), 0)
cat("decimals of X among data_analysis rows (", sum(!is.na(dd$Xr)), "matched):\n"); print(table(nd, useNA = "ifany"))
cat("decimals of X among distinct ID_NALEZ in data_analysis:\n")
print(table(nd[!duplicated(dd$ID_NALEZ)]))

## 2. duplicates
cat("\n== Duplicates ==\n")
cat("ID_NALEZ distinct:", dplyr::n_distinct(d$ID_NALEZ), " duplicates:", sum(duplicated(d$ID_NALEZ)), "\n")
dup_by <- d %>% dplyr::group_by(ID_NALEZ) %>% dplyr::summarise(n = dplyr::n(), n_rown = dplyr::n_distinct(row_n), n_imp = dplyr::n_distinct(IMPUTED), n_druh = dplyr::n_distinct(DRUH), .groups = "drop")
print(table(n = dup_by$n)); cat("dups explained by row_n:", sum(dup_by$n > 1 & dup_by$n_rown == dup_by$n), "of", sum(dup_by$n > 1), "\n")
cat("dups with mixed IMPUTED:", sum(dup_by$n_imp > 1), " mixed DRUH:", sum(dup_by$n_druh > 1), "\n")
# a teleius record imputed as nausithous absence shares ID_NALEZ with the original
key <- paste(d$ID_NALEZ, d$DRUH)
cat("dup (ID_NALEZ, DRUH):", sum(duplicated(key)), "\n")
# fully identical rows on all columns except row_n and range flags
cols_ex <- setdiff(names(d), c("row_n", "range_Pnau", "range_Ptel", "range_both", grep("^(OBJECTID|Shape|SHAPE|AREA|LENGTH|kod|nazev|NAZEV|KOD|FID)", names(d), value = TRUE)))
cat("Column names data_analysis:\n"); print(names(d))
cat("\ndup by DRUH & POSITIVE: \n"); print(d %>% dplyr::filter(duplicated(key) | duplicated(key, fromLast = TRUE)) %>% dplyr::count(DRUH, IMPUTED, POSITIVE))
cat("Unique records per species after dedup on (ID_NALEZ, DRUH):\n"); print(d %>% dplyr::distinct(ID_NALEZ, DRUH, .keep_all = TRUE) %>% dplyr::count(DRUH, POSITIVE, IMPUTED))
cat("Before dedup:\n"); print(d %>% dplyr::count(DRUH, POSITIVE, IMPUTED))

## 3. site identifiers
cat("\n== Site identifiers ==\n")
cat("ID_LOKAL levels:", dplyr::n_distinct(d$ID_LOKAL), "\n")
yrs_per_lok <- d %>% dplyr::group_by(ID_LOKAL) %>% dplyr::summarise(ny = dplyr::n_distinct(YEAR))
print(table(yrs_per_lok$ny))
naz <- d %>% dplyr::group_by(NAZ_LOKAL) %>% dplyr::summarise(ny = dplyr::n_distinct(YEAR))
cat("NAZ_LOKAL levels:", nrow(naz), " in >1 year:", sum(naz$ny > 1), sprintf("(%.1f%%)", 100 * mean(naz$ny > 1)), "\n")
xy <- paste(d$X, d$Y)
cat("X:Y levels:", dplyr::n_distinct(xy), " singletons:", sum(table(xy) == 1), "\n")
for (sp in c(SPECIES_NAU, SPECIES_TEL)) {
  s <- d %>% dplyr::filter(DRUH == sp); t <- table(paste(s$X, s$Y))
  cat(sp, ": n =", nrow(s), " X:Y levels =", length(t), " singletons =", sum(t == 1), "\n")
}
cat("SITMAP levels:", dplyr::n_distinct(d$SITMAP), "\n")
sit <- d %>% dplyr::group_by(SITMAP) %>% dplyr::summarise(ny = dplyr::n_distinct(YEAR), n = dplyr::n())
cat("SITMAP in >1 year:", sum(sit$ny > 1), " median records per SITMAP:", median(sit$n), "\n"); print(table(sit$ny))

## 11. visits
cat("\n== Visits ==\n")
sy <- d %>% dplyr::distinct(ID_NALEZ, DRUH, .keep_all = TRUE) %>% dplyr::group_by(NAZ_LOKAL, YEAR, DRUH) %>% dplyr::summarise(n = dplyr::n(), nd = dplyr::n_distinct(DATUM_OD), .groups = "drop")
cat("site(NAZ_LOKAL)-year-species combos:", nrow(sy), " with 1 record:", sum(sy$n == 1), " with >1 distinct date:", sum(sy$nd > 1), "\n")
sy2 <- d %>% dplyr::group_by(NAZ_LOKAL, YEAR, DRUH) %>% dplyr::summarise(n = dplyr::n(), .groups = "drop")
cat("(with duplicates) combos:", nrow(sy2), " 1 record:", sum(sy2$n == 1), "\n")
cat("observers:", dplyr::n_distinct(d$AUTOR), "\n")
cat("ZDROJ x YEAR:\n"); print(table(d$ZDROJ, d$YEAR))
cat("YEAR class:", class(d$YEAR), "\n")

#----------------------------------------------------------#
# Section r03_imputation -----
#----------------------------------------------------------#

suppressPackageStartupMessages(library(magrittr))
SPECIES_NAU <- "Phengaris nausithous"; SPECIES_TEL <- "Phengaris teleius"
d <- readr::read_csv("Data/Processed/data_analysis.csv", show_col_types = FALSE, guess_max = 1e5)
wi <- readr::read_csv("Data/Processed/data_with_imputed.csv", show_col_types = FALSE, guess_max = 1e5)
loc <- readr::locale(encoding = "Windows-1250")
raw <- dplyr::bind_rows(lapply(c("Data/Input/Phengaris_nausithous_2019_2024.csv", "Data/Input/Phengaris_teleius_2019_2024.csv"),
  function(x) readr::read_delim(x, delim = ";", locale = loc, col_types = readr::cols(.default = "c"))))
cat("All ZDROJ in raw 2019-2024 (count):\n"); print(sort(table(raw$ZDROJ), decreasing = TRUE)[1:15])
src <- c(
  "Kolektiv autorů (2017) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2018) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2019) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2020) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2021) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2020) Monitoring motýlů. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2021) Monitoring motýlů. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2022) Monitoring motýlů.",
  "Kolektiv autorů (2023) Monitoring motýlů.",
  "Kolektiv autorů (2024) Monitoring motýlů.")
cat("\nRaw rows per target source:\n"); print(sapply(src, function(s) sum(raw$ZDROJ == s, na.rm = TRUE)))
tm <- raw %>% dplyr::filter(ZDROJ %in% src)
cat("raw targeted rows:", nrow(tm), "\n"); print(table(tm$DRUH, tm$NEGATIV))
cat("targeted rows by year x species:\n"); print(table(substr(tm$DATUM_OD, 1, 4), tm$DRUH))
cat("ZDROJ year vs record year (targeted):\n"); print(table(zdroj_year = sub(".*\\((\\d{4})\\).*", "\\1", tm$ZDROJ), rec_year = substr(tm$DATUM_OD, 1, 4)))

## range polygons
rn <- sf::st_read("Data/Input/Reporting_range/nausithous.shp", quiet = TRUE)
rt <- sf::st_read("Data/Input/Reporting_range/teleius.shp", quiet = TRUE)
for (r in list(rn, rt)) {
  cat("\nrange layer: n =", nrow(r), " CRS:", sf::st_crs(r)$input, " geom:", as.character(unique(sf::st_geometry_type(r))), "\n")
  a <- as.numeric(sf::st_area(sf::st_make_valid(r))) / 1e6
  cat("area km2 summary:\n"); print(summary(a))
  print(names(r)); print(head(sf::st_drop_geometry(r), 3))
}

## imputed rows
imp <- wi %>% dplyr::filter(IMPUTED)
cat("\nimputed rows in data_with_imputed:", nrow(imp), "\n"); print(table(imp$DRUH))
cat("imputed with non-NA POCET:", sum(!is.na(imp$POCET)), " (POCET>0:", sum(imp$POCET > 0, na.rm = TRUE), ")\n")
cat("imputed with non-NA REL_POC:", sum(!is.na(imp$REL_POC)), " ODHAD==1:", sum(imp$ODHAD == 1, na.rm = TRUE), "\n")
cat("imputed with non-NA STRUKT_POZN:", sum(!is.na(imp$STRUKT_POZN)), "\n")
cat("imputed CESKE_JMENO (should match DRUH):\n"); print(table(imp$DRUH, imp$CESKE_JMENO))
cat("source record NEGATIV distribution (inherited before overwrite unknown) - check source records' NEGATIV via ID_NALEZ:\n")
srcrec <- wi %>% dplyr::filter(!IMPUTED) %>% dplyr::select(ID_NALEZ, DRUH_src = DRUH, NEG_src = NEGATIV) %>% dplyr::distinct()
ij <- imp %>% dplyr::left_join(srcrec, by = "ID_NALEZ", relationship = "many-to-many")
print(table(imputed_as = ij$DRUH, src = ij$DRUH_src, src_negativ = ij$NEG_src, useNA = "ifany"))
cat("\nIn data_analysis:\n"); print(d %>% dplyr::count(DRUH, IMPUTED, POSITIVE))
ia <- d %>% dplyr::filter(IMPUTED)
cat("imputed in analysis:", nrow(ia), " with POCET non-NA:", sum(!is.na(ia$POCET)), "\n")
tel_neg <- d %>% dplyr::filter(DRUH == SPECIES_TEL, POSITIVE == 0)
cat("share of P. teleius negatives that are imputed:", mean(tel_neg$IMPUTED), " (", sum(tel_neg$IMPUTED), "/", nrow(tel_neg), ")\n")
nau_neg <- d %>% dplyr::filter(DRUH == SPECIES_NAU, POSITIVE == 0)
cat("share of P. nausithous negatives that are imputed:", mean(nau_neg$IMPUTED), " (", sum(nau_neg$IMPUTED), "/", nrow(nau_neg), ")\n")

## visit-level check: how many imputed teleius absences correspond to a visit where a teleius record exists for the same NAZ_LOKAL/DATUM_OD?
obs <- d %>% dplyr::filter(!IMPUTED)
vk <- function(x) paste(x$NAZ_LOKAL, x$DATUM_OD)
cat("imputed tel absences whose visit (NAZ_LOKAL,date) has an observed teleius record:",
    sum(vk(ia %>% dplyr::filter(DRUH == SPECIES_TEL)) %in% vk(obs %>% dplyr::filter(DRUH == SPECIES_TEL))), "\n")
cat("observed both-species visits (same NAZ_LOKAL & date, both species observed records):",
    length(intersect(vk(obs %>% dplyr::filter(DRUH == SPECIES_TEL)), vk(obs %>% dplyr::filter(DRUH == SPECIES_NAU)))), "\n")
# raw targeted: visits by ID_LOKAL/date
tmv <- tm %>% dplyr::mutate(v = paste(ID_LOKAL, DATUM_OD))
both_v <- intersect(tmv$v[tmv$DRUH == SPECIES_NAU], tmv$v[tmv$DRUH == SPECIES_TEL])
cat("raw targeted visits by (ID_LOKAL,date) with both species records:", length(both_v), "\n")
cat("ID_LOKAL shared between species in raw targeted:", length(intersect(tm$ID_LOKAL[tm$DRUH == SPECIES_NAU], tm$ID_LOKAL[tm$DRUH == SPECIES_TEL])), "\n")
# Does the same ID_LOKAL appear for both species? (one site geometry with records for both)
cat("raw rows per ID_LOKAL:\n"); print(table(table(tm$ID_LOKAL)))

#----------------------------------------------------------#
# Section r04_range -----
#----------------------------------------------------------#

suppressPackageStartupMessages(library(magrittr))
SPECIES_NAU <- "Phengaris nausithous"; SPECIES_TEL <- "Phengaris teleius"
d <- readr::read_csv("Data/Processed/data_analysis.csv", show_col_types = FALSE, guess_max = 1e5)
for (f in c("nausithous", "teleius")) {
  r <- sf::st_read(sprintf("Data/Input/Reporting_range/%s.shp", f), quiet = TRUE)
  wkb <- sf::st_as_binary(sf::st_geometry(r))
  cat(f, ": rows", nrow(r), " distinct geometries:", length(unique(wkb)), " distinct KOD:", dplyr::n_distinct(r$KOD),
      " CNTRY_CHK:", paste(names(table(r$CNTRY_CHK)), table(r$CNTRY_CHK), collapse = " "), " REGION_CHK:", paste(names(table(r$REGION_CHK)), table(r$REGION_CHK), collapse = " "), "\n")
  ov <- sf::st_overlaps(sf::st_make_valid(r))
  cat("  pairs of overlapping cells:", sum(lengths(ov)) / 2, "\n")
  eq <- sf::st_equals(r)
  cat("  cells with an identical twin:", sum(lengths(eq) > 1), "\n")
  print(head(table(r$KOD), 5)); print(summary(as.numeric(table(r$KOD))))
}
# The 77 true duplicates: same ID_NALEZ & DRUH
key <- paste(d$ID_NALEZ, d$DRUH)
dupk <- unique(key[duplicated(key)])
dd <- d %>% dplyr::filter(key %in% dupk)
cat("\nrows in true-duplicate groups:", nrow(dd), " groups:", length(dupk), "\n")
cat("differ only in row_n? (n distinct row_n per group):\n"); print(table(tapply(dd$row_n, paste(dd$ID_NALEZ, dd$DRUH), dplyr::n_distinct)))
cat("geometry type of those sites? -> check lokal_new\n")
lok <- sf::st_read("Data/Processed/lokal_new.gpkg", quiet = TRUE)
cat("lokal_new rows:", nrow(lok), " distinct ID_LOKAL:", dplyr::n_distinct(lok$ID_LOKAL), "\n")
print(table(sf::st_geometry_type(lok)))
dupl_lok <- lok$ID_LOKAL[duplicated(lok$ID_LOKAL)]
cat("ID_LOKAL duplicated in lokal_new:", length(dupl_lok), "\n")
print(table(sf::st_geometry_type(lok[lok$ID_LOKAL %in% dd$ID_LOKAL, ])))
cat("CRS lokal_new:", sf::st_crs(lok)$input, "\n")
bb <- sf::st_bbox(lok); print(bb)
a <- as.numeric(sf::st_area(lok)); cat("polygon area (m2) summary for polygons:\n"); print(summary(a[sf::st_geometry_type(lok) %in% c("POLYGON", "MULTIPOLYGON")]))
l <- as.numeric(sf::st_length(lok)); cat("line length (m) summary:\n"); print(summary(l[sf::st_geometry_type(lok) %in% c("LINESTRING", "MULTILINESTRING")]))
# compare X,Y (corrected) with centroid of geometry
loc <- readr::locale(encoding = "Windows-1250")
raw <- dplyr::bind_rows(lapply(c("Data/Input/Phengaris_nausithous_2019_2024.csv", "Data/Input/Phengaris_teleius_2019_2024.csv"),
  function(x) readr::read_delim(x, delim = ";", locale = loc, col_types = readr::cols(.default = "c"))))
raw <- raw %>% dplyr::mutate(ID_LOKAL = as.numeric(ID_LOKAL), X = as.numeric(X), Y = as.numeric(Y))
cen <- suppressWarnings(sf::st_point_on_surface(lok %>% dplyr::distinct(ID_LOKAL, .keep_all = TRUE)))
cxy <- cbind(ID_LOKAL = cen$ID_LOKAL, sf::st_coordinates(cen))
m <- raw %>% dplyr::distinct(ID_LOKAL, X, Y) %>% dplyr::inner_join(as.data.frame(cxy), by = "ID_LOKAL", suffix = c("", ".geom"))
dist <- sqrt((m$X - m$X.geom)^2 + (m$Y - m$Y.geom)^2)
cat("distance raw X/Y to site geometry point-on-surface (m):\n"); print(summary(dist)); print(quantile(dist, c(.9, .99)))
cat("raw records with no site geometry:", sum(!raw$ID_LOKAL %in% lok$ID_LOKAL), "\n")

#----------------------------------------------------------#
# Section r05_strukt -----
#----------------------------------------------------------#

suppressPackageStartupMessages(library(magrittr))
loc <- readr::locale(encoding = "Windows-1250")
raw <- dplyr::bind_rows(lapply(c("Data/Input/Phengaris_nausithous_2019_2024.csv", "Data/Input/Phengaris_teleius_2019_2024.csv"),
  function(x) readr::read_delim(x, delim = ";", locale = loc, col_types = readr::cols(.default = "c"))))
src <- c("Kolektiv autorů (2019) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2020) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2021) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2020) Monitoring motýlů. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2021) Monitoring motýlů. Monitoring druhů ČR. AOPK ČR.",
  "Kolektiv autorů (2022) Monitoring motýlů.")
tm <- raw %>% dplyr::filter(ZDROJ %in% src)
s <- tm$STRUKT_POZN
cat("targeted rows:", length(s), " non-NA STRUKT_POZN:", sum(!is.na(s)), "\n")
cat("\nSample strings:\n"); writeLines(head(unique(s[!is.na(s)]), 8))
writeLines("\n---- longest ----"); writeLines(s[which.max(nchar(s))])
# Try to identify keys: tokens "<Key>: " at start or after ", "
keys <- unlist(regmatches(s, gregexpr("(^|, )[A-ZÁČĎÉĚÍŇÓŘŠŤÚŮÝŽ][^:,]{2,60}:", s)))
keys <- trimws(sub("^, ", "", keys))
cat("\nCandidate keys (count):\n"); print(sort(table(keys), decreasing = TRUE))
cat("\nAny semicolons/pipes/newlines inside STRUKT_POZN?", sum(grepl(";", s)), sum(grepl("[|]", s)), sum(grepl("\n", s)), "\n")
cat("strings by year:\n"); print(table(substr(tm$DATUM_OD, 1, 4), is.na(s)))
saveRDS(tm, file.path(Sys.getenv("SP"), "tm.rds"))

#----------------------------------------------------------#
# Section r06_strukt_kv -----
#----------------------------------------------------------#

suppressPackageStartupMessages(library(magrittr))
tm <- readRDS(file.path(Sys.getenv("SP"), "tm.rds"))
s <- tm$STRUKT_POZN
keys <- unlist(regmatches(s, gregexpr("(^|, )[^,:()]{2,70}: ", s)))
keys <- trimws(sub("^, ", "", sub(": $", "", keys)))
cat("All 'xxx: ' tokens (count):\n"); print(sort(table(keys), decreasing = TRUE))

KEYS <- c("Seč vhodně načasovaná", "Seč celoplošná", "Pastva", "vliv", "Negativní vlivy na lokalitě",
          "jiný vliv", "Perspektiva lokality", "Přítomnost rostlin", "Poměr pohlaví",
          "Procento mikrolokalit s vyhovujícím stavem živných rostlin")
kre <- paste0("(^|, )(", paste(KEYS, collapse = "|"), "): ")
parse1 <- function(x) {
  if (is.na(x)) return(NULL)
  m <- gregexpr(kre, x, perl = TRUE)[[1]]
  if (m[1] == -1) return(data.frame(key = NA, value = x))
  starts <- as.integer(m); lens <- attr(m, "match.length")
  ends <- c(starts[-1] - 1, nchar(x))
  tok <- regmatches(x, list(m))[[1]]
  k <- sub(": $", "", sub("^, ", "", tok))
  v <- substring(x, starts + lens, ends)
  data.frame(key = k, value = v)
}
kv <- dplyr::bind_rows(lapply(seq_along(s), function(i) { r <- parse1(s[i]); if (!is.null(r)) r$row <- i; r }))
cat("\nunparsed prefix/no-key rows:", sum(is.na(kv$key)), "\n")
# text before the first key?
pre <- vapply(s, function(x) { if (is.na(x)) return(NA_character_); m <- regexpr(kre, x, perl = TRUE); if (m > 1) substr(x, 1, m - 1) else "" }, "")
cat("rows with text before first known key:", sum(nzchar(pre), na.rm = TRUE), "\n"); print(head(unique(pre[nzchar(pre) & !is.na(pre)]), 10))
cat("keys repeated within a row:", sum(duplicated(kv[, c("row", "key")])), "\n")
for (k in setdiff(KEYS, c("jiný vliv", "Poměr pohlaví"))) {
  cat("\n== ", k, " ==\n")
  v <- kv$value[kv$key == k]
  if (k %in% c("vliv", "Negativní vlivy na lokalitě")) {
    # split on commas not inside parentheses
    parts <- unlist(lapply(v, function(z) trimws(strsplit(z, ",(?![^()]*\\))", perl = TRUE)[[1]])))
    print(sort(table(parts), decreasing = TRUE))
  } else print(sort(table(v), decreasing = TRUE))
}
cat("\njiný vliv examples:\n"); print(head(kv$value[kv$key == "jiný vliv"], 8))
cat("Poměr pohlaví examples:\n"); print(head(unique(kv$value[kv$key == "Poměr pohlaví"]), 8))
# Co-occurrence of keys: does 'vliv' and 'Negativní vlivy' appear by year?
kv$year <- substr(tm$DATUM_OD[kv$row], 1, 4)
print(table(kv$key, kv$year))
saveRDS(kv, file.path(Sys.getenv("SP"), "kv.rds"))

#----------------------------------------------------------#
# Section r07_compare -----
#----------------------------------------------------------#

suppressPackageStartupMessages(library(magrittr))
SPECIES_NAU <- "Phengaris nausithous"; SPECIES_TEL <- "Phengaris teleius"
d <- readr::read_csv("Data/Processed/data_analysis.csv", show_col_types = FALSE, guess_max = 1e5)
wi <- readr::read_csv("Data/Processed/data_with_imputed.csv", show_col_types = FALSE, guess_max = 1e5)
KEYS <- c("Seč vhodně načasovaná", "Seč celoplošná", "Pastva", "vliv", "Negativní vlivy na lokalitě",
          "jiný vliv", "Perspektiva lokality", "Přítomnost rostlin", "sta_pritomnostrostlin", "Poměr pohlaví",
          "Procento mikrolokalit s vyhovujícím stavem živných rostlin")
kre <- paste0("(^|, )(", paste(KEYS, collapse = "|"), "): ")
getkey <- function(x, key) {
  vapply(x, function(z) {
    if (is.na(z)) return(NA_character_)
    m <- gregexpr(kre, z, perl = TRUE)[[1]]
    if (m[1] == -1) return(NA_character_)
    starts <- as.integer(m); lens <- attr(m, "match.length"); ends <- c(starts[-1] - 1, nchar(z))
    k <- sub(": $", "", sub("^, ", "", regmatches(z, list(m))[[1]]))
    i <- which(k == key); if (!length(i)) return(NA_character_)
    trimws(substring(z, starts[i] + lens[i], ends[i]))
  }, character(1), USE.NAMES = FALSE)
}
# 'jiný vliv' sits INSIDE the threat lists (it is itself a threat item with free text). Check
cat("rows with 'jiný vliv:' :", sum(grepl("jiný vliv:", wi$STRUKT_POZN)), "\n")
s <- d$STRUKT_POZN
plant_new <- dplyr::coalesce(getkey(s, "Přítomnost rostlin"), getkey(s, "sta_pritomnostrostlin"))
cat("\nplant key in analysis rows:\n"); print(table(plant_new, useNA = "ifany"))
cat("PLANT_QUANT (old) vs parsed plant key:\n"); print(table(old = d$PLANT_QUANT, new = plant_new, useNA = "ifany"))
both_keys <- !is.na(getkey(s, "Přítomnost rostlin")) & !is.na(getkey(s, "sta_pritomnostrostlin"))
cat("rows with both plant keys:", sum(both_keys), "\n")
tim <- getkey(s, "Seč vhodně načasovaná"); met <- getkey(s, "Seč celoplošná"); past <- getkey(s, "Pastva")
cat("\nTIMING old vs key:\n"); print(table(old = d$TIMING, new = tim, useNA = "ifany"))
cat("METHOD old vs key:\n"); print(table(old = d$METHOD, new = met, useNA = "ifany"))
cat("GRAZE old vs Pastva key:\n"); print(table(old = d$GRAZE, new = past, useNA = "ifany"))
cat("GRAZE_MET old vs key:\n"); print(table(old = d$GRAZE_MET, new = past, useNA = "ifany"))
cat("MOW old vs any Seč key present:\n"); print(table(old = d$MOW, new = !is.na(tim) | !is.na(met), useNA = "ifany"))
cat("MANAGEMENT_HET == !is.na(METHOD):", all(d$MANAGEMENT_HET == as.numeric(!is.na(d$METHOD))), "\n")
cat("GRAZE NA count:", sum(is.na(d$GRAZE)), "\n")
cat("OVERALL table vs METHOD/TIMING NA:\n"); print(table(OVERALL = d$OVERALL, mowing_assessed = !is.na(d$METHOD) & !is.na(d$TIMING)))
cat("None table:\n"); print(table(d$None, useNA = "ifany"))
cat("'žádný vliv' in analysis rows:", sum(grepl("žádný vliv", s)), "\n")
cat("Perspektiva in analysis rows:\n"); print(table(getkey(s, "Perspektiva lokality"), useNA = "ifany"))
cat("Year of rows with no plant key in data_with_imputed (targeted):\n")
pw <- dplyr::coalesce(getkey(wi$STRUKT_POZN, "Přítomnost rostlin"), getkey(wi$STRUKT_POZN, "sta_pritomnostrostlin"))
print(table(year = substr(wi$DATUM_OD, 1, 4), plant = pw, useNA = "ifany"))
cat("sta_pritomnostrostlin by year:\n"); print(table(substr(wi$DATUM_OD, 1, 4), !is.na(getkey(wi$STRUKT_POZN, "sta_pritomnostrostlin"))))
# PLANT_QUANT old where new parse says NA => what matched?
odd <- which(!is.na(d$PLANT_QUANT) & is.na(plant_new))
cat("\nold PLANT_QUANT set but no plant key:", length(odd), "\n"); if (length(odd)) writeLines(head(s[odd], 5))
dis <- which(!is.na(plant_new) & !is.na(d$PLANT_QUANT) &
  c(žádné = 0, jednotlivě = 1, hojně = 2, dominantně = 3)[plant_new] != d$PLANT_QUANT)
cat("old PLANT_QUANT disagrees with key:", length(dis), "\n"); if (length(dis)) writeLines(head(s[dis], 5))
# HET_INN, TTP etc from POP_BIOT
cat("\nPOP_BIOT values (top):\n"); print(head(sort(table(d$POP_BIOT, useNA = "ifany"), decreasing = TRUE), 20))
cat("HET_INN:\n"); print(table(d$HET_INN, useNA = "ifany"))
cat("TTP NA:", sum(is.na(d$TTP)), "\n")
cat("SUM_THREATS:\n"); print(table(d$SUM_THREATS))
cat("threat col sums:\n"); print(colSums(d[, c("LandUseChange","Abandonment","HarmfulMow","HarmfulGrazing","GrazingByeffects","FertilizerUse","Afforestation","Invasives","NativeDominants","AbioticNaturalProcesses","Encroachment","BiomassAccumulation","Eutrophization","None")]))
cat("threat key present (vliv or Negativní):", sum(!is.na(getkey(s, "vliv")) | !is.na(getkey(s, "Negativní vlivy na lokalitě"))), " of", nrow(d), "\n")

#----------------------------------------------------------#
# Section r08_biot_pa_hab -----
#----------------------------------------------------------#

suppressPackageStartupMessages(library(magrittr))
SPECIES_NAU <- "Phengaris nausithous"; SPECIES_TEL <- "Phengaris teleius"
d <- readr::read_csv("Data/Processed/data_analysis.csv", show_col_types = FALSE, guess_max = 1e5)
## POP_BIOT vocabulary
parts <- trimws(unlist(strsplit(d$POP_BIOT[!is.na(d$POP_BIOT)], ",")))
cat("POP_BIOT items:\n"); print(sort(table(parts), decreasing = TRUE))
cat("PRIKOP=0 but 'příkop' present:", sum(d$PRIKOP == 0 & grepl("příkop", d$POP_BIOT), na.rm = TRUE), "\n")
cat("HET_INN==0 with POP_BIOT non-NA:", sum(d$HET_INN == 0 & !is.na(d$POP_BIOT)), " HET_INN==0 with POP_BIOT NA:", sum(d$HET_INN == 0 & is.na(d$POP_BIOT)), "\n")
cat("JINY==1:", sum(d$JINY == 1, na.rm = TRUE), "\n")
## protection
pa <- readr::read_csv("Data/Processed/protected_area_id.csv", show_col_types = FALSE)
cat("\nprotected_area_id rows:", nrow(pa), "\n"); print(pa %>% dplyr::count(PA_TYPE, PA_SPECIES))
cat("duplicated (ID_LOKAL, PA_TYPE, PA_SPECIES) rows:", sum(duplicated(pa[, c("ID_LOKAL", "PA_TYPE", "PA_SPECIES")])), "\n")
spec_key <- unique(paste(pa$ID_LOKAL, pa$PA_SPECIES)[pa$PA_TYPE == "EVL_target"])
d$EVL_target_sp <- as.integer(paste(d$ID_LOKAL, d$DRUH) %in% spec_key)
print(d %>% dplyr::group_by(DRUH) %>% dplyr::summarise(n = dplyr::n(), EVL = sum(EVL), EVL_target_blind = sum(EVL_target), EVL_target_species = sum(EVL_target_sp), MZCHU = sum(MZCHU), PROTECT = sum(PROTECT)))
cat("EVL_target==1 but EVL==0 (should be impossible):", sum(d$EVL_target == 1 & d$EVL == 0), "\n")
cat("EVL_comb:\n"); print(table(d$EVL_comb, d$DRUH))
cat("crosstab EVL x EVL_target per species:\n"); print(table(d$DRUH, d$EVL, d$EVL_target))
## habitat
hv <- readr::read_csv2("Data/Processed/data_lokal_vmb.csv", show_col_types = FALSE)
cat("\ndata_lokal_vmb rows:", nrow(hv), " distinct ID_LOKAL:", dplyr::n_distinct(hv$ID_LOKAL), "\n"); print(names(hv))
cat("AREA_real summary:\n"); print(summary(hv$AREA_real))
cat("BIOTOP NA in vmb:", sum(is.na(hv$BIOTOP)), " BIOTOP == '-1':", sum(hv$BIOTOP == "-1", na.rm = TRUE), "\n")
print(head(sort(table(hv$BIOTOP, useNA = "ifany"), decreasing = TRUE), 25))
cat("analysis rows whose site has any VMB row:", sum(d$ID_LOKAL %in% hv$ID_LOKAL), " of", nrow(d), "\n")
cat("BIOTOP present in analysis:", sum(!is.na(d$BIOTOP)), "\n")
seg_nat <- hv %>% dplyr::mutate(NATURAL = as.integer(BIOTOP != "-1" & !grepl("X", BIOTOP) & !is.na(BIOTOP)))
per_site <- seg_nat %>% dplyr::group_by(ID_LOKAL) %>% dplyr::summarise(any_nat = any(NATURAL == 1), any_non = any(NATURAL == 0), n = dplyr::n(), .groups = "drop")
cat("sites with >=1 natural segment:", sum(per_site$any_nat), " with both natural and non-natural:", sum(per_site$any_nat & per_site$any_non), "\n")
cat("analysis rows whose site has a natural segment:", sum(d$ID_LOKAL %in% per_site$ID_LOKAL[per_site$any_nat]), "\n")
cat("AREA_SITE in analysis summary:\n"); print(summary(d$AREA_SITE)); cat("AREA_SITE == 0:", sum(d$AREA_SITE == 0, na.rm = TRUE), "\n")
lok <- sf::st_read("Data/Processed/lokal_new.gpkg", quiet = TRUE)
gt <- setNames(as.character(sf::st_geometry_type(lok)), lok$ID_LOKAL)
d$gtype <- gt[as.character(d$ID_LOKAL)]
cat("geometry types in analysis rows:\n"); print(table(d$gtype, useNA = "ifany"))
print(d %>% dplyr::group_by(gtype) %>% dplyr::summarise(area_na = sum(is.na(AREA_SITE)), area0 = sum(AREA_SITE == 0, na.rm = TRUE), area_pos = sum(AREA_SITE > 0, na.rm = TRUE)))
# true site area vs AREA_SITE for polygons
sa <- setNames(as.numeric(sf::st_area(lok)), lok$ID_LOKAL)
pol <- d %>% dplyr::filter(gtype %in% c("POLYGON", "MULTIPOLYGON"), !is.na(AREA_SITE))
r <- pol$AREA_SITE / sa[as.character(pol$ID_LOKAL)]
cat("AREA_SITE / true polygon area (polygons):\n"); print(summary(r)); cat("share < 0.5:", mean(r < 0.5), "\n")
cat("FSB in analysis:\n"); print(table(d$FSB, useNA = "ifany"))
cat("HET_OUT:\n"); print(table(d$HET_OUT, useNA = "ifany"))

#----------------------------------------------------------#
# Section r09_models -----
#----------------------------------------------------------#

suppressPackageStartupMessages(library(magrittr))
rd <- function(f) utils::read.csv(file.path("Outputs/Tables", f), fileEncoding = "Windows-1250")
for (f in c(list.files("Outputs/Tables", "aic"))) {
  x <- rd(f); cat("\n==", f, "\n"); print(x[, c("model", "n_obs", "df", "AIC", "delta_AIC")], row.names = FALSE)
}
for (f in c("09_nausithous_fit_statistics.csv", "10_teleius_fit_statistics.csv", "11_both_species_fit_statistics.csv")) {
  x <- rd(f); cat("\n==", f, "\n"); print(x[, c("model", "engine", "n_obs", "AIC", "status")], row.names = FALSE)
}
re <- rd("09_nausithous_random_effects.csv"); cat("\nnau random effects (X:Y and YEAR var):\n"); print(re[, c("model", "group", "variance")], row.names = FALSE)
re <- rd("10_teleius_random_effects.csv"); cat("\ntel random effects:\n"); print(re[, c("model", "group", "variance")], row.names = FALSE)
co <- rd("09_nausithous_coefficients.csv"); cat("\nnau coef (management/protection):\n")
print(co[co$model %in% c("nau_method_timing", "nau_evl", "nau_evl_combined", "nau_timing", "nau_method", "nau_graze", "nau_spatial", "nau_plant"), c("model", "term", "estimate", "std_error", "p_value")], row.names = FALSE)
co <- rd("10_teleius_coefficients.csv"); cat("\ntel coef:\n")
print(co[co$model %in% c("tel_method_timing_mow_subset", "tel_evl", "tel_evl_combined", "tel_timing_mow_subset", "tel_graze", "tel_spatial", "tel_area"), c("model", "term", "estimate", "std_error", "p_value")], row.names = FALSE)

#----------------------------------------------------------#
# Section r10_misc -----
#----------------------------------------------------------#

suppressPackageStartupMessages(library(magrittr))
SPECIES_NAU <- "Phengaris nausithous"; SPECIES_TEL <- "Phengaris teleius"
rd <- function(f) utils::read.csv(file.path("Outputs/Tables", f), fileEncoding = "Windows-1250")
for (f in list.files("Outputs/Tables", "^13_rda")) { cat("\n==", f, "\n"); print(rd(f)) }
d <- readr::read_csv("Data/Processed/data_analysis.csv", show_col_types = FALSE, guess_max = 1e5)
cat("\nrows entering ordination (SUM_THREATS>0):", sum(d$SUM_THREATS > 0), " -> groups:", nrow(dplyr::distinct(d[d$SUM_THREATS > 0, c("DRUH", "POSITIVE")])), "\n")
lok <- sf::st_read("Data/Processed/lokal_new.gpkg", quiet = TRUE)
gt <- setNames(as.character(sf::st_geometry_type(lok)), lok$ID_LOKAL)
d$gtype <- gt[as.character(d$ID_LOKAL)]
cat("geometry type by year:\n"); print(table(d$YEAR, d$gtype))
cat("positive share by geometry type & species:\n"); print(d %>% dplyr::group_by(DRUH, gtype) %>% dplyr::summarise(n = dplyr::n(), pos = mean(POSITIVE), .groups = "drop"))
cat("positive share by year & species:\n"); print(d %>% dplyr::group_by(DRUH, YEAR) %>% dplyr::summarise(n = dplyr::n(), pos = round(mean(POSITIVE), 3), imp = sum(IMPUTED), .groups = "drop"), n = 20)
cat("mowing assessed by year:\n"); print(table(d$YEAR, !is.na(d$METHOD)))
cat("GRAZE by year:\n"); print(table(d$YEAR, d$GRAZE))
cat("tel: PLANT_QUANT by POSITIVE & IMPUTED:\n"); print(d %>% dplyr::filter(DRUH == SPECIES_TEL) %>% dplyr::count(IMPUTED, POSITIVE, PLANT_QUANT))
cat("tel: GRAZE by POSITIVE:\n"); print(d %>% dplyr::filter(DRUH == SPECIES_TEL) %>% dplyr::count(GRAZE, POSITIVE))
# day of year
doy <- as.integer(format(as.Date(as.character(d$DATUM_OD), "%Y%m%d"), "%j"))
cat("day-of-year summary (DATUM_OD):\n"); print(summary(doy)); print(quantile(doy, c(.01, .05, .5, .95, .99)))
cat("DOY by species & positive:\n"); print(d %>% dplyr::mutate(doy = doy) %>% dplyr::group_by(DRUH, POSITIVE) %>% dplyr::summarise(q05 = quantile(doy, .05), med = median(doy), q95 = quantile(doy, .95), .groups = "drop"))
cat("multi-day records (DATUM_OD != DATUM_DO):", sum(d$DATUM_OD != d$DATUM_DO), "\n")
cat("records per observer: \n"); print(summary(as.numeric(table(d$AUTOR))))
# repeat visits at same ID_LOKAL x species in a year (proper visits)
v <- d %>% dplyr::distinct(ID_NALEZ, DRUH, .keep_all = TRUE) %>% dplyr::group_by(ID_LOKAL, DRUH) %>% dplyr::summarise(nd = dplyr::n_distinct(DATUM_OD), n = dplyr::n(), .groups = "drop")
cat("site(ID_LOKAL)-species combos:", nrow(v), " with >1 date:", sum(v$nd > 1), " with >1 record:", sum(v$n > 1), "\n")
v2 <- d %>% dplyr::distinct(ID_NALEZ, DRUH, .keep_all = TRUE) %>% dplyr::group_by(ID_LOKAL, YEAR, DRUH) %>% dplyr::summarise(n = dplyr::n(), .groups = "drop")
cat("ID_LOKAL-year-species combos:", nrow(v2), " one record:", sum(v2$n == 1), "\n")
v3 <- d %>% dplyr::group_by(ID_LOKAL, YEAR, DRUH) %>% dplyr::summarise(n = dplyr::n(), .groups = "drop")
cat("(not deduplicated) ID_LOKAL-year-species combos:", nrow(v3), " one record:", sum(v3$n == 1), "\n")
cat("POCET in observed records by POSITIVE:\n"); print(d %>% dplyr::filter(!IMPUTED) %>% dplyr::group_by(DRUH, POSITIVE) %>% dplyr::summarise(n = dplyr::n(), pocet_nonNA = sum(!is.na(POCET)), pocet0 = sum(POCET == 0, na.rm = TRUE), odhad1 = sum(ODHAD == 1), relpoc = sum(!is.na(REL_POC)), .groups = "drop"))
cat("POCET observed positive summary:\n"); print(d %>% dplyr::filter(!IMPUTED, POSITIVE == 1) %>% dplyr::group_by(DRUH) %>% dplyr::summarise(min = min(POCET, na.rm = T), q50 = median(POCET, na.rm = T), q90 = quantile(POCET, .9, na.rm = T), max = max(POCET, na.rm = T)))
cat("POCITANO (positive, observed):\n"); print(table(d$POCITANO[!d$IMPUTED & d$POSITIVE == 1], useNA = "ifany"))
cat("NEGATIV==1 with POCET>0 (observed):", sum(!d$IMPUTED & d$NEGATIV == 1 & d$POCET > 0, na.rm = TRUE), "\n")

#----------------------------------------------------------#
# Section r11_flow -----
#----------------------------------------------------------#

suppressPackageStartupMessages(library(magrittr))
SPECIES_NAU <- "Phengaris nausithous"; SPECIES_TEL <- "Phengaris teleius"
tm <- readRDS(file.path(Sys.getenv("SP"), "tm.rds"))
wi <- readr::read_csv("Data/Processed/data_with_imputed.csv", show_col_types = FALSE, guess_max = 1e5)
d <- readr::read_csv("Data/Processed/data_analysis.csv", show_col_types = FALSE, guess_max = 1e5)
obs <- wi %>% dplyr::filter(!IMPUTED)
cat("raw targeted records:", nrow(tm), " by species:", table(tm$DRUH), "\n")
cat("observed rows in data_with_imputed:", nrow(obs), " distinct (ID_NALEZ,DRUH):", nrow(dplyr::distinct(obs, ID_NALEZ, DRUH)), "\n")
lost <- tm %>% dplyr::filter(!as.numeric(ID_NALEZ) %in% obs$ID_NALEZ)
cat("targeted records outside own reporting range (dropped in step 02):", nrow(lost), "\n"); print(table(lost$DRUH, lost$NEGATIV))
cat("range_both values:\n"); print(table(wi$range_both, useNA = "ifany")); cat("range_Pnau:\n"); print(table(wi$range_Pnau, useNA = "ifany"))
cat("step 04 filter: data_with_imputed", nrow(wi), "-> data_clean", nrow(d), "\n")
# corrected coordinate levels
loc <- readr::locale(encoding = "Windows-1250")
raw <- dplyr::bind_rows(lapply(c("Data/Input/Phengaris_nausithous_2019_2024.csv", "Data/Input/Phengaris_teleius_2019_2024.csv"),
  function(x) readr::read_delim(x, delim = ";", locale = loc, col_types = readr::cols(.default = "c"))))
xy <- raw %>% dplyr::transmute(ID_NALEZ = as.numeric(ID_NALEZ), Xc = as.numeric(X), Yc = as.numeric(Y))
dd <- d %>% dplyr::left_join(xy, by = "ID_NALEZ")
cat("X:Y levels corrupted:", dplyr::n_distinct(paste(d$X, d$Y)), " corrected:", dplyr::n_distinct(paste(dd$Xc, dd$Yc)), "\n")
cat("corrected X:Y singletons:", sum(table(paste(dd$Xc, dd$Yc)) == 1), "\n")
cat("corrected coordinates within CZ S-JTSK bbox (-905000..-430000, -1230000..-930000):", all(dd$Xc > -905000 & dd$Xc < -430000 & dd$Yc > -1230000 & dd$Yc < -930000), "\n")
cat("corrupted coordinates outside that bbox:", sum(!(d$X > -905000 & d$X < -430000 & d$Y > -1230000 & d$Y < -930000)), "of", nrow(d), "\n")
# imputed absences: imputed source within same 10 km cell but which year? does the cell have a target-species record in ANY year?
cat("\nImputed P. teleius absences by year:\n"); print(table(d$YEAR[d$IMPUTED & d$DRUH == SPECIES_TEL]))
# visit-level imputation prototype: same ID_LOKAL & DATUM_OD has a record of the other species but none of this species, site inside this species' range
vis <- obs %>% dplyr::distinct(ID_LOKAL, DATUM_OD, DRUH)
vw <- vis %>% dplyr::mutate(v = 1) %>% tidyr::pivot_wider(names_from = DRUH, values_from = v, values_fill = 0)
cat("\nvisits (ID_LOKAL x date) in observed targeted-in-range records:", nrow(vw), "\n"); print(table(nau = vw[[SPECIES_NAU]], tel = vw[[SPECIES_TEL]]))

#----------------------------------------------------------#
# Section r12_visit_impute -----
#----------------------------------------------------------#

suppressPackageStartupMessages(library(magrittr))
SPECIES_NAU <- "Phengaris nausithous"; SPECIES_TEL <- "Phengaris teleius"
tm <- readRDS(file.path(Sys.getenv("SP"), "tm.rds")) %>% dplyr::mutate(ID_LOKAL = as.numeric(ID_LOKAL))
lok <- sf::st_read("Data/Processed/lokal_new.gpkg", quiet = TRUE)
rng <- lapply(c(nau = "nausithous", tel = "teleius"), function(f) sf::st_read(sprintf("Data/Input/Reporting_range/%s.shp", f), quiet = TRUE) %>% sf::st_transform(5514) %>% sf::st_make_valid())
in_range <- lapply(rng, function(r) lok$ID_LOKAL[lengths(sf::st_intersects(lok, r)) > 0])
vis <- tm %>% dplyr::distinct(ID_LOKAL, DATUM_OD, DRUH) %>% dplyr::mutate(v = 1) %>%
  tidyr::pivot_wider(names_from = DRUH, values_from = v, values_fill = 0)
names(vis)[names(vis) == SPECIES_NAU] <- "nau"; names(vis)[names(vis) == SPECIES_TEL] <- "tel"
vis <- vis %>% dplyr::mutate(in_nau = ID_LOKAL %in% in_range$nau, in_tel = ID_LOKAL %in% in_range$tel)
cat("all targeted visits (ID_LOKAL x date):", nrow(vis), "\n")
print(vis %>% dplyr::count(nau, tel, in_nau, in_tel))
cat("\nVisit-level rule candidates:\n")
cat(" P. teleius absences (nau-only visit, site in teleius range):", sum(vis$nau == 1 & vis$tel == 0 & vis$in_tel), "\n")
cat(" P. nausithous absences (tel-only visit, site in nausithous range):", sum(vis$tel == 1 & vis$nau == 0 & vis$in_nau), "\n")
# negative records explicitly entered
cat("\nExplicit negative records by species (targeted):\n"); print(table(tm$DRUH, tm$NEGATIV))
# does a nau-only visit happen on a date where the same observer entered teleius records elsewhere? (proxy that observer was recording both species that day)
tm2 <- tm %>% dplyr::mutate(od = paste(AUTOR, DATUM_OD))
tel_obs_days <- unique(tm2$od[tm2$DRUH == SPECIES_TEL])
nau_only <- vis %>% dplyr::filter(nau == 1, tel == 0, in_tel) %>% dplyr::left_join(tm2 %>% dplyr::filter(DRUH == SPECIES_NAU) %>% dplyr::distinct(ID_LOKAL, DATUM_OD, od), by = c("ID_LOKAL", "DATUM_OD"))
cat(" of those, observer recorded P. teleius somewhere on the same day:", sum(nau_only$od %in% tel_obs_days), "of", nrow(nau_only), "\n")
# what about current imputation: imputed rows' source visits
cat("\nZDROJ of nau-only in-teleius-range visits:\n")
print(table(tm %>% dplyr::semi_join(vis %>% dplyr::filter(nau == 1, tel == 0, in_tel), by = c("ID_LOKAL", "DATUM_OD")) %>% dplyr::filter(DRUH == SPECIES_NAU) %>% dplyr::pull(ZDROJ)))

#----------------------------------------------------------#
# Section r13_threats -----
#----------------------------------------------------------#

suppressPackageStartupMessages(library(magrittr))
SPECIES_NAU <- "Phengaris nausithous"; SPECIES_TEL <- "Phengaris teleius"
d <- readr::read_csv("Data/Processed/data_analysis.csv", show_col_types = FALSE, guess_max = 1e5)
kv <- readRDS(file.path(Sys.getenv("SP"), "kv.rds"))
# threat items per codebook (key) - full list
thr <- kv %>% dplyr::filter(key %in% c("vliv", "Negativní vlivy na lokalitě"))
items <- thr %>% dplyr::mutate(item = strsplit(value, ",(?![^()]*\\))", perl = TRUE)) %>% tidyr::unnest(item) %>%
  dplyr::mutate(item = trimws(sub(":.*$", "", trimws(item)))) %>% dplyr::filter(nzchar(item))
tab <- items %>% dplyr::count(key, item) %>% dplyr::arrange(key, dplyr::desc(n))
legacy <- c(LandUseChange = "změna zemědělského využívání půdy a terénní úpravy", Abandonment = "absence či nedostatek péče",
  HarmfulMow = "nevhodná seč", HarmfulGrazing = "nevhodná pastva", GrazingByeffects = "dopady chovu dobytka mimo pastvu",
  FertilizerUse = "aplikace hnojiv", Afforestation = "zalesňování bezlesí", Invasives = "invazní druhy",
  NativeDominants = "expanzní druhy", AbioticNaturalProcesses = "abiotické přírodní procesy", Encroachment = "sukcese",
  BiomassAccumulation = "hromadění organického materiálu", Eutrophization = "eutrofizace či okyselování", None = "žádné")
tab$legacy_var <- vapply(tab$item, function(it) { h <- names(legacy)[vapply(legacy, function(p) grepl(p, it, fixed = TRUE), TRUE)]; if (length(h)) paste(h, collapse = "+") else "" }, "")
options(width = 200)
cat("Threat vocabulary, items with n >= 3, and the legacy variable that captures them:\n")
print(as.data.frame(tab %>% dplyr::filter(n >= 3)), row.names = FALSE)
cat("\nItems not captured by any legacy pattern (count of item occurrences) by key:\n")
print(tab %>% dplyr::filter(legacy_var == "") %>% dplyr::group_by(key) %>% dplyr::summarise(items = dplyr::n(), occurrences = sum(n)))
cat("Captured occurrences by key:\n"); print(tab %>% dplyr::filter(legacy_var != "") %>% dplyr::group_by(key) %>% dplyr::summarise(occurrences = sum(n)))
# 2021 rows with no STRUKT_POZN
tm <- readRDS(file.path(Sys.getenv("SP"), "tm.rds"))
cat("\nTargeted rows with NA STRUKT_POZN by year x source:\n"); print(table(substr(tm$DATUM_OD, 1, 4)[is.na(tm$STRUKT_POZN)], sub(" Monitoring druhů.*", "", tm$ZDROJ[is.na(tm$STRUKT_POZN)])))
cat("\nDOY > 260 records:\n"); doy <- as.integer(format(as.Date(as.character(d$DATUM_OD), "%Y%m%d"), "%j")); print(d[doy > 260, c("ID_NALEZ", "DRUH", "DATUM_OD", "AUTOR", "POSITIVE", "IMPUTED")])
loc <- readr::locale(encoding = "Windows-1250")
rn <- readr::read_delim("Data/Input/Phengaris_nausithous_2019_2024.csv", delim = ";", locale = loc, col_types = readr::cols(.default = "c"))
nd <- ifelse(grepl("[.]", rn$X), nchar(sub(".*[.]", "", rn$X)), 0)
cat("\nP. nausithous file, X decimals:\n"); print(table(nd))
cat("teleius POCET non-NA in analysis: observed", sum(!is.na(d$POCET) & d$DRUH == SPECIES_TEL & !d$IMPUTED), " imputed (inherited nau counts)", sum(!is.na(d$POCET) & d$DRUH == SPECIES_TEL & d$IMPUTED), "\n")
print(d %>% dplyr::filter(!is.na(POCET)) %>% dplyr::group_by(DRUH, IMPUTED) %>% dplyr::summarise(n = dplyr::n(), mean = mean(POCET), median = median(POCET), .groups = "drop"))

#----------------------------------------------------------#
# Section r14_sites -----
#----------------------------------------------------------#

suppressPackageStartupMessages(library(magrittr))
d <- readr::read_csv("Data/Processed/data_analysis.csv", show_col_types = FALSE, guess_max = 1e5)
lok <- sf::st_read("Data/Processed/lokal_new.gpkg", quiet = TRUE) %>% dplyr::filter(ID_LOKAL %in% d$ID_LOKAL)
yr <- d %>% dplyr::distinct(ID_LOKAL, YEAR)
lok <- lok %>% dplyr::left_join(yr, by = "ID_LOKAL")
cat("site geometries in analysis:", nrow(lok), "\n")
components <- function(adj) {
  n <- length(adj); id <- rep(NA_integer_, n); k <- 0L
  for (i in seq_len(n)) if (is.na(id[i])) { k <- k + 1L; st <- i; while (length(st)) { j <- st[1]; st <- st[-1]; if (is.na(id[j])) { id[j] <- k; st <- c(st, adj[[j]][is.na(id[adj[[j]]])]) } } }
  id
}
for (dist in c(0, 10, 25, 50, 100, 200)) {
  adj <- if (dist == 0) sf::st_intersects(lok) else sf::st_is_within_distance(lok, lok, dist = dist)
  sid <- components(adj)
  s <- data.frame(sid = sid, YEAR = lok$YEAR) %>% dplyr::group_by(sid) %>% dplyr::summarise(ny = dplyr::n_distinct(YEAR), ng = dplyr::n(), .groups = "drop")
  rec <- d %>% dplyr::left_join(data.frame(ID_LOKAL = lok$ID_LOKAL, sid = sid), by = "ID_LOKAL")
  cat(sprintf("dist %4d m: sites %5d | in >1 year %4d (%.1f%%) | max geometries/site %3d | records per site median %.0f, singleton-record sites %d\n",
    dist, nrow(s), sum(s$ny > 1), 100 * mean(s$ny > 1), max(s$ng), median(table(rec$sid)), sum(table(rec$sid) == 1)))
}

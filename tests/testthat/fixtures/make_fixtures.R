#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Build the file fixtures of the test suite
#
#----------------------------------------------------------#
#
# Writes occurrence_export.csv, a five-record stand-in for an AOPK occurrence
# export with the edge cases found in the real exports:
#
#   * Windows-1250 encoding, semicolons, CRLF line endings;
#   * "." as the decimal mark, with 2, 1 and 0 decimals in X and Y, which
#     read_csv2() inflated by 100, 10 and 1;
#   * rows that leave out their two empty trailing fields (48 instead of 50);
#   * a free-text column (GAR_POZN) that is empty except in one later row,
#     so that guessing its type would lose the value;
#   * Czech text in DRUH-related columns and in STRUKT_POZN.
#
# The records are made up. Run from the project root:
#
#   Rscript tests/testthat/fixtures/make_fixtures.R
#
#----------------------------------------------------------#

header <- c(
  "PORADI", "ID_NALEZ", "DRUH", "CESKE_JMENO", "KAT_TAX", "AUTOR", "DATUM_OD",
  "DATUM_DO", "CAS", "ID_LOKAL", "NAZ_LOKAL", "SITMAP", "KATASTR", "X", "Y",
  "ZDROJ", "NEGATIV", "POCET", "ODHAD", "POCITANO", "REL_POC", "METOD_SB",
  "POKRYVN", "PATROVITOST", "FENOFAZE", "KODHN", "ETOLOGIE",
  "KOMPLETNI_SEZNAM", "POZNAMKA", "UMIST_NAL", "POP_BIOT", "TAX_POZN",
  "PROJEKT", "DAT_SADA", "ZAPSAL", "VEROH", "VALIDACE", "VAL_POZN", "GARANCE",
  "GAR_POZN", "VYHLASKA", "EVD", "REDLIST", "STRUKT_POZN", "CXLOKAL_POZN",
  "CXLOKAL_Z", "CXPRESNOST", "DATI_INSERT", "DATI_UPDATE", "UPDATOR"
)

#' One export row; `fields` overrides the empty default of any column.
row <- function(..., drop_trailing = TRUE) {
  values <- stats::setNames(rep("", length(header)), header)
  fields <- list(...)
  values[names(fields)] <- unlist(fields)
  if (drop_trailing) values <- values[seq_len(48)]
  paste(values, collapse = ";")
}

src <- "Kolektiv autorů (2022) Monitoring motýlů."
rows <- c(
  paste(header, collapse = ";"),
  row(PORADI = "1", ID_NALEZ = "101", DRUH = "Phengaris nausithous",
      CESKE_JMENO = "modrásek bahenní", DATUM_OD = "20220715", DATUM_DO = "20220715",
      CAS = "12:30", ID_LOKAL = "11", SITMAP = "6852", X = "-759673.12", Y = "-1145366.59",
      ZDROJ = src, NEGATIV = "0", POCET = "3", ODHAD = "0", KOMPLETNI_SEZNAM = "0",
      VEROH = "1", POP_BIOT = "TTP s pravidelným managementem, příkop",
      STRUKT_POZN = "Seč vhodně načasovaná: ano, Seč celoplošná: ne, vliv: žádný vliv, Perspektiva lokality: dobrá, Přítomnost rostlin: hojně",
      CXPRESNOST = "1", DATI_INSERT = "20251209"),
  row(PORADI = "2", ID_NALEZ = "102", DRUH = "Phengaris teleius",
      CESKE_JMENO = "modrásek očkovaný", DATUM_OD = "20220715", DATUM_DO = "20220715",
      ID_LOKAL = "12", SITMAP = "6852", X = "-759484.1", Y = "-1145268.2",
      ZDROJ = src, NEGATIV = "1", ODHAD = "0", KOMPLETNI_SEZNAM = "0", VEROH = "1",
      CXPRESNOST = "1", DATI_INSERT = "20251209"),
  row(PORADI = "3", ID_NALEZ = "103", DRUH = "Phengaris nausithous",
      CESKE_JMENO = "modrásek bahenní", DATUM_OD = "20210629", DATUM_DO = "20210629",
      ID_LOKAL = "13", SITMAP = "6852", X = "-759650", Y = "-1145329",
      ZDROJ = src, NEGATIV = "0", POCET = "1", ODHAD = "0", KOMPLETNI_SEZNAM = "0",
      VEROH = "0", CXPRESNOST = "1", DATI_INSERT = "20251123"),
  row(PORADI = "4", ID_NALEZ = "104", DRUH = "Phengaris nausithous",
      CESKE_JMENO = "modrásek bahenní", DATUM_OD = "20240805", DATUM_DO = "20240805",
      ID_LOKAL = "14", SITMAP = "5360", X = "-644580.26", Y = "-993889.1",
      ZDROJ = src, NEGATIV = "0", POCET = "1", ODHAD = "0", KOMPLETNI_SEZNAM = "0",
      VEROH = "1", GAR_POZN = "Kvůli dešti jsem je nestihl ani hledat.",
      CXPRESNOST = "8", DATI_INSERT = "20251123", DATI_UPDATE = "20251201",
      UPDATOR = "4", drop_trailing = FALSE)
)

out <- file.path("tests", "testthat", "fixtures", "occurrence_export.csv")
con <- file(out, open = "wb")
writeBin(iconv(paste0(paste(rows, collapse = "\r\n"), "\r\n"), "UTF-8", "CP1250", toRaw = TRUE)[[1]], con)
close(con)
message("Written ", out)

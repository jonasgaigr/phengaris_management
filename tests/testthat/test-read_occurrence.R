#----------------------------------------------------------#
# read_occurrence_csv(): the AOPK occurrence exports -----
#----------------------------------------------------------#

test_that("coordinates keep '.' as the decimal mark, whatever their number of decimals", {
  x <- read_occurrence_csv(fixture("occurrence_export.csv"))
  # 2, 1 and 0 decimals: read_csv2() turned these into 100x, 10x and 1x.
  expect_equal(x$X, c(-759673.12, -759484.1, -759650, -644580.26))
  expect_equal(x$Y, c(-1145366.59, -1145268.2, -1145329, -993889.1))
})

test_that("the bug being fixed: read_csv2() inflates the coordinates", {
  legacy <- suppressWarnings(suppressMessages(readr::read_csv2(
    fixture("occurrence_export.csv"),
    locale = readr::locale(encoding = "Windows-1250"),
    show_col_types = FALSE
  )))
  expect_equal(legacy$X / c(-759673.12, -759484.1, -759650, -644580.26), c(100, 10, 1, 100))
})

test_that("every row is read, short rows get NA in their missing trailing fields", {
  x <- read_occurrence_csv(fixture("occurrence_export.csv"))
  expect_equal(nrow(x), 4)
  expect_equal(ncol(x), 50)
  # Rows 1-3 leave out DATI_UPDATE and UPDATOR; row 4 has them.
  expect_equal(x$DATI_UPDATE, c(NA, NA, NA, 20251201))
  # The last field before the cut keeps its value, without a carriage return.
  expect_equal(x$DATI_INSERT, c(20251209, 20251209, 20251123, 20251123))
})

test_that("text is decoded from Windows-1250 and free-text values are not lost", {
  x <- read_occurrence_csv(fixture("occurrence_export.csv"))
  expect_equal(x$CESKE_JMENO[2], "modrásek očkovaný")
  expect_true(grepl("Přítomnost rostlin: hojně", x$STRUKT_POZN[1], fixed = TRUE))
  # Empty in the first rows: guessing the type would make it logical and
  # drop the one value.
  expect_type(x$GAR_POZN, "character")
  expect_equal(x$GAR_POZN[4], "Kvůli dešti jsem je nestihl ani hledat.")
})

test_that("the column types are the declared ones", {
  x <- read_occurrence_csv(fixture("occurrence_export.csv"))
  expect_type(x$ID_NALEZ, "double")
  expect_type(x$NEGATIV, "double")
  expect_type(x$DRUH, "character")
  expect_s3_class(x$CAS, "hms")
})

#----------------------------------------------------------#
# drop_excluded_records(): records excluded in config.yml -----
#----------------------------------------------------------#

test_that("the records excluded in config.yml are dropped, and only those", {
  x <- data.frame(ID_NALEZ = c(1, 2, 3, 4), DRUH = "Phengaris teleius")
  out <- drop_excluded_records(x, list(list(id = 2, reason = "wrong place"), list(id = 9, reason = "not here")))
  expect_equal(out$data$ID_NALEZ, c(1, 3, 4))
  expect_equal(out$dropped, 2)
})

test_that("without an exclusion list every record is kept", {
  x <- data.frame(ID_NALEZ = c(1, 2))
  out <- drop_excluded_records(x, NULL)
  expect_equal(nrow(out$data), 2)
  expect_length(out$dropped, 0)
})

test_that("the excluded records of config.yml carry an id and a reason", {
  for (e in CONFIG$records$excluded) {
    expect_true(is.numeric(e$id))
    expect_true(nzchar(e$reason))
  }
})

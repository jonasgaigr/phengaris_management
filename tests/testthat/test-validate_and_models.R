#----------------------------------------------------------#
# validate_table(), comparison_data(), model diagnostics, step inputs -----
#----------------------------------------------------------#

good <- data.frame(
  ID_RECORD = c("1_nau", "2_nau", "1_tel"),
  X = c(-700000, -650000, -700000),
  Y = c(-1100000, -1050000, -1100000),
  POSITIVE = c(1, 0, 0),
  stringsAsFactors = FALSE
)

test_that("a valid table passes", {
  expect_silent(validate_table(
    good, "good",
    columns = c(ID_RECORD = "character", X = "numeric"),
    key = "ID_RECORD", not_na = "POSITIVE",
    values = list(POSITIVE = c(0, 1)), xy = c("X", "Y"), n_rows = 3
  ))
})

test_that("every problem is caught and reported together", {
  bad <- good
  bad$ID_RECORD[2] <- "1_nau"
  bad$POSITIVE[3] <- NA
  bad$X[1] <- -75967312            # a coordinate inflated by read_csv2()
  err <- tryCatch(
    validate_table(
      bad, "bad", key = "ID_RECORD", not_na = "POSITIVE",
      xy = c("X", "Y"), n_rows = 4
    ),
    error = conditionMessage
  )
  expect_match(err, "duplicate the key")
  expect_match(err, "unexpected NA")
  expect_match(err, "outside the S-JTSK bounding box")
  expect_match(err, "expected 4")
})

test_that("unexpected values and wrong types are caught", {
  bad <- good
  bad$POSITIVE[1] <- 2
  expect_error(validate_table(bad, "bad", values = list(POSITIVE = c(0, 1))), "unexpected value")
  expect_error(validate_table(good, "good", columns = c(X = "character")), "X")
})

#--------------------------------------------------#
## AIC comparisons on common data -----
#--------------------------------------------------#

set.seed(1)
d <- data.frame(
  ID_RECORD = paste0("r", 1:60),
  POSITIVE = rep(c(0, 1), 30),
  AREA = c(0, rep(10, 29), rep(100, 30)),
  PLANT = c(rep(NA, 5), sample(1:3, 55, replace = TRUE))
)

test_that("the common data are complete and finite on every variable of the set", {
  specs <- list(
    list(id = "area", formula = as.factor(POSITIVE) ~ log10(AREA), data = d, engine = "glm"),
    list(id = "plant", formula = as.factor(POSITIVE) ~ PLANT, data = d[d$AREA > 0, ], engine = "glm")
  )
  common <- comparison_data(specs)
  # Row 1 has log10(0) = -Inf (and is not in the second subset); rows 2-5
  # have no PLANT.
  expect_equal(nrow(common), 55)
  expect_false(any(is.na(common$PLANT)))
  tab <- suppressMessages(aic_comparison(lapply(specs, function(s) c(s, label = s$id, group = "test")), c("area", "plant")))
  expect_equal(unique(tab$n_obs), 55)
  expect_setequal(tab$n_own_data, c(60, 55))
})

#--------------------------------------------------#
## Diagnostics flag separation -----
#--------------------------------------------------#

test_that("a perfectly separated model is flagged", {
  sep <- data.frame(POSITIVE = c(0, 0, 0, 1, 1, 1), X = c(1, 2, 3, 4, 5, 6))
  m <- fit_binomial("sep", "separated", as.factor(POSITIVE) ~ X, sep, engine = "glm")
  diag <- model_diagnostics(m)
  expect_match(diag$flags, "possible separation")
})

#--------------------------------------------------#
## A step with a missing input stops with a clear message -----
#--------------------------------------------------#

test_that("step_begin() names the missing input and the step that writes it", {
  old <- CONFIG$paths$processed
  CONFIG$paths$processed <<- file.path(tempdir(), "no_such_folder")
  on.exit(CONFIG$paths$processed <<- old, add = TRUE)
  err <- tryCatch(step_begin("04"), error = conditionMessage)
  expect_match(err, "Step 04 cannot run")
  expect_match(err, "data_with_imputed.csv")
  expect_match(err, "written by step 02")
})

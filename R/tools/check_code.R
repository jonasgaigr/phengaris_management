#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#     Tool - Test, style and lint the code
#
#----------------------------------------------------------#
#
# NOT part of the cascade. Run before committing:
#
#   Rscript R/tools/check_code.R
#
# 1. runs the test suite in tests/testthat;
# 2. restyles R/, tests/ and run_all.R with styler (tidyverse style), and
#    says which files it changed;
# 3. lints them with the rules in .lintr and fails when anything is left.
#
#----------------------------------------------------------#

testthat::test_dir("tests/testthat", stop_on_failure = TRUE)

styled <- rbind(
  styler::style_dir("R", recursive = TRUE),
  styler::style_dir("tests", recursive = TRUE),
  styler::style_file("run_all.R")
)
if (any(styled$changed)) {
  message("styler changed: ", paste(styled$file[styled$changed], collapse = ", "))
}

lints <- rbind(
  as.data.frame(lintr::lint_dir("R")),
  as.data.frame(lintr::lint_dir("tests")),
  as.data.frame(lintr::lint("run_all.R"))
)
if (nrow(lints)) {
  print(lints[, c("filename", "line_number", "linter", "message")])
  stop(nrow(lints), " lint(s) left.", call. = FALSE)
}
message("Tests pass, code styled, no lints.")

library(testthat)

run_compare_help <- function(args) {
  script <- testthat::test_path("..", "..", "scripts", "compare")
  output <- system2(script, args, stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"))
  expect_true(any(grepl("usage:", output, ignore.case = TRUE)))
}

test_that("compare driver exposes expected help commands", {
  run_compare_help("--help")
  run_compare_help(c("collect", "--help"))
  run_compare_help(c("collect", "base", "--help"))
  run_compare_help(c("collect", "target", "--help"))
  run_compare_help(c("collect", "external", "--help"))
  run_compare_help(c("version", "--help"))
  run_compare_help(c("external", "--help"))
})

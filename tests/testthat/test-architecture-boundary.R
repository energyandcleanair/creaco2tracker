test_that("front layer files do not bypass data-access boundary", {
  cfg <- architecture_get_boundary_config()

  for (file_name in cfg$front_layer_files) {
    file_path <- testthat::test_path("..", "..", "R", file_name)
    expect_true(file.exists(file_path), info = paste("Missing front-layer file:", file_name))

    code <- paste(readLines(file_path, warn = FALSE), collapse = "\n")

    for (pattern in cfg$forbidden_front_patterns) {
      expect_false(
        grepl(pattern, code, perl = TRUE),
        info = paste("Forbidden pattern", pattern, "found in", file_name)
      )
    }
  }
})


test_that("configured data-access files exist", {
  cfg <- architecture_get_boundary_config()

  for (file_name in cfg$data_access_files) {
    expect_true(
      file.exists(testthat::test_path("..", "..", "R", file_name)),
      info = paste("Missing data-access file:", file_name)
    )
  }
})


test_that("configured client files exist", {
  cfg <- architecture_get_boundary_config()

  for (file_name in cfg$client_files) {
    expect_true(
      file.exists(testthat::test_path("..", "..", "R", file_name)),
      info = paste("Missing client file:", file_name)
    )
  }
})

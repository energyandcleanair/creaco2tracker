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

  expect_true(length(cfg$data_access_files) > 0, info = "No access_* files found in R/")

  for (file_name in cfg$data_access_files) {
    expect_true(
      file.exists(testthat::test_path("..", "..", "R", file_name)),
      info = paste("Missing data-access file:", file_name)
    )
  }
})


test_that("configured client files exist", {
  cfg <- architecture_get_boundary_config()

  expect_true(length(cfg$client_files) > 0, info = "No client_* files found in R/")

  for (file_name in cfg$client_files) {
    expect_true(
      file.exists(testthat::test_path("..", "..", "R", file_name)),
      info = paste("Missing client file:", file_name)
    )
  }
})


test_that("every R file belongs to exactly one configured layer", {
  cfg <- architecture_get_boundary_config()

  all_r_files <- basename(list.files(
    path = testthat::test_path("..", "..", "R"),
    pattern = "\\.R$",
    full.names = TRUE
  ))

  layer_match_matrix <- sapply(cfg$layer_names, function(layer_name) {
    pattern <- cfg$layer_file_patterns[[layer_name]]
    grepl(pattern, all_r_files, perl = TRUE)
  })
  if (is.null(dim(layer_match_matrix))) {
    layer_match_matrix <- matrix(layer_match_matrix, ncol = 1)
  }
  match_counts <- rowSums(layer_match_matrix)

  bad_files <- all_r_files[match_counts != 1]
  bad_counts <- match_counts[match_counts != 1]

  expect_true(
    all(match_counts == 1),
    info = paste(
      "Files must match exactly one layer pattern.",
      paste0(bad_files, " (matches=", bad_counts, ")", collapse = ", ")
    )
  )
})


test_that("each configured layer has at least one file", {
  cfg <- architecture_get_boundary_config()
  all_r_files <- basename(list.files(
    path = testthat::test_path("..", "..", "R"),
    pattern = "\\.R$",
    full.names = TRUE
  ))

  for (layer_name in cfg$layer_names) {
    layer_pattern <- cfg$layer_file_patterns[[layer_name]]

    expect_false(
      is.null(layer_pattern),
      info = paste("Missing naming pattern for layer:", layer_name)
    )

    expect_true(
      sum(grepl(layer_pattern, all_r_files, perl = TRUE)) > 0,
      info = paste("No files found for layer pattern:", layer_name, "(", layer_pattern, ")")
    )
  }
})

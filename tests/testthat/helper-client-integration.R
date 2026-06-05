client_integration_enabled <- function() {
  toupper(Sys.getenv("RUN_CLIENT_INTEGRATION_TESTS", unset = "FALSE")) == "TRUE"
}

.client_integration_fixture_dir <- normalizePath(
  testthat::test_path("fixtures", "client-integration"),
  mustWork = FALSE
)

skip_if_client_integration_disabled <- function() {
  skip_if_not(
    client_integration_enabled(),
    "Set RUN_CLIENT_INTEGRATION_TESTS=TRUE to run client integration tests"
  )
}

skip_if_client_envvar_missing <- function(name) {
  skip_if(
    Sys.getenv(name, unset = "") == "",
    paste0("Set ", name, " to run this client integration test")
  )
}

client_integration_workdir <- function() {
  temp_dir <- tempfile("client-integration-test-")
  dir.create(temp_dir, recursive = TRUE)
  old_dir <- setwd(temp_dir)
  file.create(".Renviron")

  list(temp_dir = temp_dir, old_dir = old_dir)
}

cleanup_client_integration_workdir <- function(workdir) {
  setwd(workdir$old_dir)
  unlink(workdir$temp_dir, recursive = TRUE)
}

read_client_integration_fixture <- function(file) {
  result <- read.csv(
    file.path(.client_integration_fixture_dir, file),
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )

  if ("date" %in% names(result)) {
    result$date <- as.Date(result$date)
  }

  result
}

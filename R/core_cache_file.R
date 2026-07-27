#' Test whether a cache file was modified today
#'
#' Compares the file modification date with the current date using the R
#' process's local calendar date.
#'
#' @param filepath Path to the cache file.
#' @param current_date Date to compare against. Defaults to
#'   \code{Sys.Date()}.
#'
#' @return \code{TRUE} when \code{filepath} exists and was modified on
#'   \code{current_date}; otherwise \code{FALSE}.
#' @keywords internal
cache_file_modified_today <- function(filepath, current_date = Sys.Date()) {
  if (!file.exists(filepath)) {
    return(FALSE)
  }

  modified_at <- file.info(filepath)$mtime
  modified_date <- as.Date(format(modified_at, "%Y-%m-%d"))
  !is.na(modified_at) && identical(modified_date, as.Date(current_date))
}


#' Populate, validate, and consume a file-backed cache
#'
#' Manages the lifecycle of one cached file. When a persistent cache is missing
#' or stale, \code{populate_fun} writes a staging file and
#' \code{validate_fun} checks it
#' before it replaces the cache. The staging file is created beside the cache
#' so that promotion with \code{file.rename()} stays on the same filesystem.
#'
#' When caching is disabled, the file is populated under \code{tempdir()},
#' consumed, and removed before this function returns. Consequently,
#' \code{consume_fun} must finish reading the file during the call and must not
#' return an object that depends on the temporary file remaining available.
#'
#' Errors from population or validation leave an existing persistent cache
#' untouched. Errors are propagated to the caller.
#'
#' @param filepath Persistent cache path. When \code{use_cache} is
#'   \code{FALSE}, its basename and extension are used to name the temporary
#'   file.
#' @param populate_fun Function accepting one staging-file path. It must
#'   completely write that file or raise an error.
#' @param consume_fun Function accepting the populated file path. Its return
#'   value becomes the return value of
#'   \code{cache_file_get_or_refresh()}.
#' @param use_cache Whether to read and write the persistent cache at
#'   \code{filepath}. When \code{FALSE}, use a temporary file for this call
#'   only.
#' @param is_fresh_fun Function accepting an existing persistent cache path and
#'   returning one logical value. It is not called when the cache is absent.
#' @param validate_fun Function accepting a newly populated staging-file path.
#'   It must raise an error when the file must not replace the existing cache.
#'
#' @return The value returned by \code{consume_fun}.
#' @keywords internal
cache_file_get_or_refresh <- function(
  filepath,
  populate_fun,
  consume_fun,
  use_cache = TRUE,
  is_fresh_fun = function(path) TRUE,
  validate_fun = function(path) invisible(path)
) {
  cache_exists <- use_cache && file.exists(filepath)
  cache_name <- basename(filepath)

  if (cache_exists && isTRUE(is_fresh_fun(filepath))) {
    log_debug("File cache hit for {cache_name}: {filepath}")
    return(consume_fun(filepath))
  }

  file_extension <- tools::file_ext(filepath)
  temporary_extension <- if (nzchar(file_extension)) {
    paste0(".", file_extension)
  } else {
    ".tmp"
  }

  if (use_cache) {
    dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)
    temporary_dir <- dirname(filepath)
    cache_state <- if (cache_exists) "stale" else "missing"
    log_debug(
      "File cache {cache_state} for {cache_name}; refreshing {filepath}"
    )
  } else {
    temporary_dir <- tempdir()
    log_debug("File cache disabled for {cache_name}")
  }

  temporary_path <- tempfile(
    pattern = paste0(basename(filepath), "."),
    tmpdir = temporary_dir,
    fileext = temporary_extension
  )
  on.exit(unlink(temporary_path), add = TRUE)

  populate_fun(temporary_path)
  validate_fun(temporary_path)

  if (!use_cache) {
    return(consume_fun(temporary_path))
  }

  if (!file.rename(temporary_path, filepath)) {
    stop(
      paste0("Unable to replace file cache at ", filepath),
      call. = FALSE
    )
  }

  consume_fun(filepath)
}

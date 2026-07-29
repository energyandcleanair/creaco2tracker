#' Resolve the package cache directory
#'
#' Uses the first configured value from the
#' \code{creaco2tracker.cache_dir} option, the
#' \code{CREACO2TRACKER_CACHE_DIR} environment variable, or the relative
#' directory \code{"cache"}.
#'
#' @return A length-one character path with user-directory expansion applied.
#' @keywords internal
creaco2tracker_cache_dir <- function() {
  option_value <- getOption("creaco2tracker.cache_dir")
  env_value <- Sys.getenv("CREACO2TRACKER_CACHE_DIR", unset = "")

  cache_dir <- if (!is.null(option_value)) {
    option_value
  } else if (nzchar(env_value)) {
    env_value
  } else {
    "cache"
  }

  if (!is.character(cache_dir) || length(cache_dir) != 1 || !nzchar(cache_dir)) {
    stop(
      "The creaco2tracker cache directory must be one non-empty path.",
      call. = FALSE
    )
  }

  path.expand(cache_dir)
}


#' Construct a path for a hashed parquet cache
#'
#' @param cache_prefix Human-readable prefix used in the cache filename.
#' @param cache_hash Hash identifying the cached request and schema.
#' @param use_cache Whether to return a persistent path in
#'   \code{creaco2tracker_cache_dir()} or a unique temporary path.
#'
#' @return A length-one character path ending in \code{.parquet}.
#' @keywords internal
cache_parquet_path <- function(cache_prefix, cache_hash, use_cache = TRUE) {
  if (use_cache) {
    cache_dir <- creaco2tracker_cache_dir()
    create_dir(cache_dir)
    file.path(cache_dir, paste0(cache_prefix, "_", cache_hash, ".parquet"))
  } else {
    tempfile(
      pattern = paste0(cache_prefix, "_", cache_hash, "_"),
      tmpdir = tempdir(),
      fileext = ".parquet"
    )
  }
}

#' Read a parquet cache file
#'
#' @param filepath Path to a parquet file.
#'
#' @return The value returned by \code{arrow::read_parquet()}.
#' @keywords internal
cache_parquet_read <- function(filepath) {
  arrow::read_parquet(filepath)
}

#' Write a parquet cache file
#'
#' Creates the parent directory when necessary before writing \code{value}.
#'
#' @param value Tabular value accepted by \code{arrow::write_parquet()}.
#' @param filepath Destination parquet path.
#'
#' @return \code{filepath}, invisibly.
#' @keywords internal
cache_parquet_write <- function(value, filepath) {
  dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(value, filepath)
  invisible(filepath)
}

#' Fetch or reuse a keyed parquet cache
#'
#' Hashes \code{cache_key} together with \code{cache_schema_version} to identify a
#' persistent parquet file. Existing files are reused indefinitely. On a cache
#' miss, \code{fetch_fun} is evaluated and its result is written through the
#' generic file-cache lifecycle. Setting \code{use_cache = FALSE} fetches
#' through a temporary parquet file and does not read or update the persistent
#' cache.
#'
#' Increment \code{cache_schema_version} whenever a change makes existing
#' cached files incompatible with the current reader or expected result.
#'
#' @param cache_prefix Human-readable prefix used in the cache filename.
#' @param cache_key Object describing all inputs that affect the fetched value.
#' @param fetch_fun Zero-argument function that returns a tabular value.
#' @param use_cache Whether to reuse and populate the persistent cache.
#' @param cache_schema_version Version included in the cache hash to invalidate
#'   incompatible files.
#'
#' @return The fetched or cached value after a parquet write/read round trip.
#' @keywords internal
cache_parquet_get_or_fetch <- function(
  cache_prefix,
  cache_key,
  fetch_fun,
  use_cache = TRUE,
  cache_schema_version = "v1_parquet"
) {
  cache_hash <- digest::digest(list(cache_schema_version, cache_key))
  filepath <- if (use_cache) {
    cache_parquet_path(cache_prefix, cache_hash, use_cache = TRUE)
  } else {
    file.path(
      tempdir(),
      paste0(cache_prefix, "_", cache_hash, ".parquet")
    )
  }

  cache_file_get_or_refresh(
    filepath = filepath,
    populate_fun = function(temporary_path) {
      cache_parquet_write(fetch_fun(), temporary_path)
    },
    consume_fun = cache_parquet_read,
    use_cache = use_cache
  )
}

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

cache_parquet_read <- function(filepath) {
  arrow::read_parquet(filepath)
}

cache_parquet_write <- function(value, filepath) {
  dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(value, filepath)
  invisible(filepath)
}

cache_parquet_get_or_fetch <- function(
  cache_prefix,
  cache_key,
  fetch_fun,
  use_cache = TRUE,
  cache_schema_version = "v1_parquet"
) {
  cache_hash <- digest::digest(list(cache_schema_version, cache_key))
  filepath <- cache_parquet_path(cache_prefix, cache_hash, use_cache = use_cache)

  if (use_cache && file.exists(filepath)) {
    log_debug("Parquet cache hit for {cache_prefix} ({cache_hash}): {filepath}")
    return(cache_parquet_read(filepath))
  }

  if (use_cache) {
    log_debug("Parquet cache miss for {cache_prefix} ({cache_hash}); fetching into {filepath}")
  } else {
    log_debug("Parquet cache disabled for {cache_prefix} ({cache_hash}); fetching into {filepath}")
  }

  value <- fetch_fun()
  cache_parquet_write(value, filepath)
  cache_parquet_read(filepath)
}

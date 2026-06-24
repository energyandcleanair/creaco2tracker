cache_parquet_path <- function(cache_prefix, cache_hash, use_cache = TRUE) {
  if (use_cache) {
    create_dir("cache")
    file.path("cache", paste0(cache_prefix, "_", cache_hash, ".parquet"))
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
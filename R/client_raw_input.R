# Fetching owns the original response file; parsing always runs after cache reuse.
.raw_http_download <- function(url, query, path, max_attempts = 3L, initial_sleep = 1) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(
      {
        response <- httr::GET(url,
          query = query, httr::write_disk(path, overwrite = TRUE),
          httr::timeout(120)
        )
        httr::stop_for_status(response)
        invisible(path)
      },
      error = identity
    )
    if (!inherits(result, "error")) {
      return(invisible(path))
    }
    if (attempt == max_attempts) stop(result)
    Sys.sleep(initial_sleep * 2^(attempt - 1L))
  }
}

.raw_input_get <- function(name, key, extension, populate_fun, decode_fun, stage,
                           use_cache = TRUE, validate_fun = decode_fun) {
  path <- file.path(
    creaco2tracker_cache_dir(), "raw",
    paste0(name, "_", digest::digest(key), ".", extension)
  )
  cache_file_get_or_refresh(
    filepath = path,
    populate_fun = populate_fun,
    validate_fun = function(path) {
      .pipeline_input_file(path, stage, paste0(name, "_000_raw.", extension))
      validate_fun(path)
      invisible(path)
    },
    consume_fun = function(path) {
      .pipeline_input_file(path, stage, paste0(name, "_000_raw.", extension))
      .pipeline_snapshot(decode_fun(path), stage, paste0(name, "_010_decoded"))
    },
    use_cache = use_cache
  )
}

.raw_read_csv <- function(path) {
  if (file.info(path)$size == 0) {
    return(tibble::tibble())
  }
  readr::read_csv(path, col_types = readr::cols(), show_col_types = FALSE)
}

# CREA CSV endpoints share splitting and decoding, without creahelpers' processed cache.
.crea_raw_get <- function(endpoint, date_from = NULL, date_to = NULL, split_by = NULL,
                          ..., params = list(), use_cache = TRUE, stage,
                          max_retries = 3L, initial_sleep = 1, verbose = FALSE) {
  if (!grepl("^https?://", endpoint)) endpoint <- paste0("https://", endpoint)
  params <- utils::modifyList(
    list(date_from = date_from, date_to = date_to, format = "csv"),
    utils::modifyList(params, list(...))
  )
  implicit_date_to <- is.null(params$date_to)
  if (implicit_date_to) params$date_to <- Sys.Date()
  intervals <- list(list(params$date_from, params$date_to))
  if (!is.null(params$date_from) && !is.null(split_by)) {
    dates <- seq(as.Date(params$date_from), as.Date(params$date_to), by = split_by)
    ends <- c(dates[-1], as.Date(params$date_to) + 1) - 1
    intervals <- Map(function(start, end) list(start, end), dates, ends)
  }
  dplyr::bind_rows(lapply(intervals, function(interval) {
    query <- params
    query$date_from <- interval[[1]]
    query$date_to <- interval[[2]]
    query <- lapply(query, function(x) paste(as.character(x), collapse = ","))
    safe <- query[!names(query) %in% c("api_key", "token", "key")]
    # Timeless endpoints (such as IEA conversion factors) keep a stable cache key.
    if (implicit_date_to && is.null(params$date_from)) safe$date_to <- NULL
    name <- paste0(basename(endpoint), "_", .pipeline_variant(safe))
    .raw_input_get(
      name, list(endpoint, safe), "csv",
      function(path) .raw_http_download(endpoint, query, path, max_retries, initial_sleep),
      .raw_read_csv, stage, use_cache
    )
  }))
}

.eurostat_retry_delay_seconds <- function(
  retry_number, initial_delay_seconds = 1, backoff_multiplier = 2
) {
  initial_delay_seconds * backoff_multiplier^(retry_number - 1)
}

# JSON-stat decoding keeps the source dimension codes and missing values.
.eurostat_decode_json <- function(path) {
  data <- jsonlite::fromJSON(path)
  if (!is.null(data$error)) stop("Eurostat returned an error response.", call. = FALSE)
  ids <- data$id
  if (is.null(ids) || is.null(data$dimension)) {
    stop("Invalid Eurostat JSON-stat response.", call. = FALSE)
  }
  dims <- lapply(data$dimension[rev(ids)], function(dimension) {
    index <- unlist(dimension$category$index)
    names(index)[order(index)]
  })
  grid <- expand.grid(dims, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  result <- tibble::as_tibble(grid[rev(names(grid))])
  result$values <- rep(NA_real_, nrow(result))
  values <- unlist(data$value, use.names = FALSE)
  positions <- 1L + as.integer(names(data$value))
  if (length(values)) {
    if (length(values) != length(positions)) stop("Invalid Eurostat value indices.")
    result$values[positions] <- values
  }
  result
}

.eurostat_read_tsv <- function(path) {
  readr::read_tsv(gzfile(path), na = ":", progress = FALSE,
    col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
}

.fetch_eurostat_with_retry <- function(code, filters = NULL, use_cache = TRUE,
                                       max_attempts = 3, initial_delay_seconds = 1) {
  filtered <- !is.null(filters)
  name <- paste0(code, if (filtered) paste0("_", .pipeline_variant(filters)) else "_all")
  if (filtered) {
    url <- paste0("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/", code)
    query <- as.list(unlist(filters))
    names(query) <- rep(names(filters), lengths(filters))
    query$format <- "JSON"
    query$lang <- "en"
  } else {
    url <- paste0(getOption("eurostat_url", "https://ec.europa.eu/eurostat/"),
      "api/dissemination/sdmx/2.1/data/", code)
    query <- list(format = "TSV", compressed = "true")
  }
  decoded <- .raw_input_get(name, list(code, filters), if (filtered) "json" else "tsv.gz",
    function(path) .raw_http_download(url, query, path, max_attempts, initial_delay_seconds),
    if (filtered) .eurostat_decode_json else .eurostat_read_tsv,
    "eurostat", use_cache)
  if (filtered) {
    # Use the same calendar conversion as the Eurostat package, without its fetch/cache layer.
    decoded$time <- getFromNamespace("convert_time_col", "eurostat")(
      factor(decoded$time), time_format = "date")
  } else {
    decoded <- getFromNamespace("tidy_eurostat", "eurostat")(
      decoded, time_format = "date", select_time = NULL,
      stringsAsFactors = FALSE, keepFlags = FALSE, use.data.table = FALSE)
  }
  .pipeline_snapshot(decoded, "eurostat", paste0(name, "_020_normalized"))
}

get_eurostat_from_code <- function(code, iso2s = NULL, use_cache = TRUE, filters = NULL) {
  if (!is.null(iso2s)) filters$geo <- iso2s
  .fetch_eurostat_with_retry(code, filters, use_cache) %>%
    dplyr::rename(dplyr::any_of(c(time = "TIME_PERIOD")))
}

.eurostat_retry_delay_seconds <- function(
  retry_number,
  initial_delay_seconds = 1,
  backoff_multiplier = 2
) {
  initial_delay_seconds * backoff_multiplier^(retry_number - 1)
}


.fetch_eurostat_with_retry <- function(
  code,
  filters = NULL,
  max_attempts = 3,
  initial_delay_seconds = 1,
  backoff_multiplier = 2
) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(
      eurostat::get_eurostat(
        code,
        filters = filters,
        keepFlags = FALSE
      ),
      error = function(e) e
    )

    if (!inherits(result, "error")) {
      if (attempt > 1) {
        log_info(glue::glue(
          "EUROSTAT request for {code} succeeded on attempt {attempt}/{max_attempts}"
        ))
      }
      return(result)
    }

    if (attempt == max_attempts) {
      log_warn(glue::glue(
        "EUROSTAT request failed for {code} on final attempt {attempt}/{max_attempts}: ",
        "{conditionMessage(result)}"
      ))
      stop(result)
    }

    delay_seconds <- .eurostat_retry_delay_seconds(
      retry_number = attempt,
      initial_delay_seconds = initial_delay_seconds,
      backoff_multiplier = backoff_multiplier
    )

    log_warn(glue::glue(
      "EUROSTAT request failed for {code} on attempt {attempt}/{max_attempts}: ",
      "{conditionMessage(result)}. Retrying in {delay_seconds}s."
    ))
    Sys.sleep(delay_seconds)
  }
}


get_eurostat_from_code <- function(code, iso2s = NULL, use_cache = TRUE, filters = NULL) {
  normalize_eurostat <- function(x) {
    x %>%
      # Column changed with new EUROSTAT version
      dplyr::rename(dplyr::any_of(c(time = "TIME_PERIOD")))
  }

  effective_filters <- filters
  if (!is.null(iso2s)) {
    effective_filters$geo <- iso2s
  }

  cache_parquet_get_or_fetch(
    cache_prefix = glue::glue("eurostat_{code}"),
    cache_key = list(iso2s = iso2s, filters = effective_filters),
    use_cache = use_cache,
    cache_schema_version = "v5_parquet_cache",
    fetch_fun = function() {
      raw <- .fetch_eurostat_with_retry(
        code = code,
        filters = effective_filters
      )

      normalize_eurostat(raw)
    }
  )
}

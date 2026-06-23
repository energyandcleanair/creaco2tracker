.agsi_retry_delay_seconds <- function(
  retry_number,
  initial_delay_seconds = 1,
  backoff_multiplier = 2
) {
  initial_delay_seconds * backoff_multiplier^(retry_number - 1)
}

.agsi_json_field_has_value <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(FALSE)
  }

  value <- unlist(value, use.names = FALSE)
  any(!is.na(value) & nzchar(as.character(value)))
}

.agsi_response_error_message <- function(parsed_response) {
  error_value <- parsed_response$error
  if (!.agsi_json_field_has_value(error_value)) {
    return(NULL)
  }

  message_parts <- c(parsed_response$message, error_value)
  message_parts <- unlist(message_parts, use.names = FALSE)
  message_parts <- as.character(message_parts[!is.na(message_parts) & nzchar(message_parts)])

  paste(unique(message_parts), collapse = "; ")
}


.fetch_agsi_storage_change_with_retry <- function(
  url,
  api_key,
  iso2,
  date_from,
  date_to,
  max_attempts = 4,
  initial_delay_seconds = 1,
  backoff_multiplier = 2
) {
  for (attempt in seq_len(max_attempts)) {
    http_response <- tryCatch(
      httr::GET(url, httr::add_headers("x-key" = api_key), httr::accept_json()),
      error = function(e) e
    )

    if (inherits(http_response, "error")) {
      if (attempt == max_attempts) {
        warning(glue::glue(
          "AGSI request failed for {iso2} from {date_from} to {date_to} on ",
          "final attempt {attempt}/{max_attempts}: {conditionMessage(http_response)}"
        ))
        return(tibble())
      }

      delay_seconds <- .agsi_retry_delay_seconds(
        retry_number = attempt,
        initial_delay_seconds = initial_delay_seconds,
        backoff_multiplier = backoff_multiplier
      )

      log_warn(paste0(
        "AGSI request failed for {iso2} from {date_from} to {date_to} on ",
        "attempt {attempt}/{max_attempts}: {conditionMessage(http_response)}. ",
        "Retrying in {delay_seconds}s."
      ))
      Sys.sleep(delay_seconds)
      next
    }

    status <- httr::status_code(http_response)
    response_text <- httr::content(http_response, "text", encoding = "UTF-8")
    log_debug(paste0(
      "AGSI response for {iso2} from {date_from} to {date_to} ",
      "returned HTTP {status}: {response_text}"
    ))

    if (!identical(status, 200L)) {
      if (attempt == max_attempts) {
        warning(glue::glue(
          "AGSI request returned HTTP {status} for {iso2} from {date_from} to {date_to} ",
          "on final attempt {attempt}/{max_attempts}."
        ))
        return(tibble())
      }

      delay_seconds <- .agsi_retry_delay_seconds(
        retry_number = attempt,
        initial_delay_seconds = initial_delay_seconds,
        backoff_multiplier = backoff_multiplier
      )

      log_warn(paste0(
        "AGSI request returned HTTP {status} for {iso2} from {date_from} to {date_to} ",
        "on attempt {attempt}/{max_attempts}. Retrying in {delay_seconds}s."
      ))
      Sys.sleep(delay_seconds)
      next
    }

    parsed_response <- tryCatch(
      jsonlite::fromJSON(response_text),
      error = function(e) e
    )

    if (inherits(parsed_response, "error")) {
      warning(glue::glue(
        "Unable to parse AGSI response for {iso2} from {date_from} to {date_to}: ",
        "{conditionMessage(parsed_response)}"
      ))
      return(tibble())
    }

    error_message <- .agsi_response_error_message(parsed_response)
    if (!is.null(error_message)) {
      stop(glue::glue(
        "AGSI response returned error for {iso2} from {date_from} to {date_to}: ",
        "{error_message}"
      ), call. = FALSE)
    }

    data <- parsed_response$data
    if (is.null(data)) {
      return(tibble())
    }

    data <- tryCatch(
      as_tibble(data),
      error = function(e) e
    )

    if (inherits(data, "error")) {
      warning(glue::glue(
        "AGSI response data is not tabular for {iso2} from {date_from} to {date_to}: ",
        "{conditionMessage(data)}"
      ))
      return(tibble())
    }

    return(data)
  }

  tibble()
}


agsi.get_storage_change <- function(date_from, date_to, iso2, use_cache = TRUE, verbose = FALSE) {
  pbapply::pblapply(iso2, function(iso2) {
    log_info("Getting storage change data for {iso2} from {date_from} to {date_to}")

    MAX_PAGE_SIZE <- as.integer(100000)

    url <- glue(
      "https://agsi.gie.eu/api",
      "?country={iso2}",
      "&from={date_from}",
      "&to={date_to}",
      "&page=1",
      "&size={MAX_PAGE_SIZE}"
    )

    log_debug("AGSI request URL: {url}")

    cache_key <- list(date_from = date_from, date_to = date_to, iso2 = iso2, url = url)
    cache_schema_version <- "v1_parquet_cache"

    data <- cache_parquet_get_or_fetch(
      cache_prefix = "agsi_storage_change",
      cache_key = cache_key,
      use_cache = use_cache,
      cache_schema_version = cache_schema_version,
      fetch_fun = function() {
        # Add api key in header only for network calls.
        api_key <- Sys.getenv("AGSI_API_KEY")
        if (api_key == "") {
          warning("AGSI_API_KEY not set. Returning empty table")
          return(tibble())
        }

        .fetch_agsi_storage_change_with_retry(
          url = url,
          api_key = api_key,
          iso2 = iso2,
          date_from = date_from,
          date_to = date_to
        )
      }
    )

    if (nrow(data) == 0 || !"netWithdrawal" %in% names(data)) {
      log_info("No data for {iso2} from {date_from} to {date_to}")
      return(NULL)
    }
    # Add a check for the size is near the limit of 100,000 records (let's do one extra)
    if (nrow(data) >= MAX_PAGE_SIZE - 1) {
      warning(
        glue(
          "Data for {iso2} from {date_from} to {date_to}",
          " may be truncated ({MAX_PAGE_SIZE} record limit)"
        )
      )
    }

    data %>%
      select(
        iso2 = code,
        date = gasDayStart,
        value_gwh = netWithdrawal
      ) %>%
      mutate(
        date = lubridate::date(date),
        value_gwh = suppressWarnings(as.numeric(value_gwh)),
        value_m3 = value_gwh * 1e6 / gcv_kwh_m3,
        type = "storage_drawdown"
      ) %>%
      tibble()
  }) %>%
    bind_rows()
}

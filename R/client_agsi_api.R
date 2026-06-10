agsi.get_storage_change <- function(date_from, date_to, iso2, use_cache = TRUE, verbose = FALSE) {
  pbapply::pblapply(iso2, function(iso2) {
    log_info(glue::glue("Getting storage change data for {iso2} from {date_from} to {date_to}"))

    MAX_PAGE_SIZE <- 100000

    url <- glue(
      "https://agsi.gie.eu/api?country={iso2}",
      "&from={date_from}",
      "&to={date_to}",
      "&page=1",
      "&size={MAX_PAGE_SIZE}"
    )

    # Naive URL-based cache for AGSI requests.
    create_dir("cache")
    cache_schema_version <- "v1_url"
    cache_hash <- digest::digest(list(cache_schema_version, url))
    cache_file <- file.path("cache", paste0("agsi_", cache_hash, ".json"))

    response_text <- NULL
    if (use_cache && file.exists(cache_file)) {
      response_text <- readChar(cache_file, file.info(cache_file)$size)
    } else {
      # Add api key in header only for network calls.
      api_key <- Sys.getenv("AGSI_API_KEY")
      if (api_key == "") {
        warning("AGSI_API_KEY not set. Returning NULL")
        return(NULL)
      }

      http_response <- httr::GET(url, httr::add_headers("x-key" = api_key), httr::accept_json())
      response_text <- httr::content(http_response, "text", encoding = "UTF-8")
      writeChar(response_text, cache_file, eos = NULL)
    }

    data <- jsonlite::fromJSON(response_text)
    data <- data$data

    if (nrow(data) == 0 || !"netWithdrawal" %in% names(data)) {
      log_info(glue::glue("No data for {iso2} from {date_from} to {date_to}"))
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

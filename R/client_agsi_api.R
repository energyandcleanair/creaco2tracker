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

        http_response <- httr::GET(url, httr::add_headers("x-key" = api_key), httr::accept_json())
        response_text <- httr::content(http_response, "text", encoding = "UTF-8")
        jsonlite::fromJSON(response_text)$data
      }
    )

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

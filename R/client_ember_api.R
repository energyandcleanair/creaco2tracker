ember.get_power_generation <- function(frequency = "yearly", iso2s = "EU", use_cache = TRUE) {
  cache_parquet_get_or_fetch(
    cache_prefix = glue::glue("ember_power_{frequency}"),
    cache_key = iso2s,
    use_cache = use_cache,
    cache_schema_version = "v1_parquet_cache",
    fetch_fun = function() {
      readRenviron(".Renviron")
      key <- Sys.getenv("EMBER_KEY")

      # To get entities
      pbapply::pblapply(iso2s, function(iso2) {
        base_url <- "https://api.ember-energy.org"
        entity_field <- ifelse(iso2 == "EU", "entity", "entity_code")
        entity_value <- ifelse(iso2 == "EU", "EU", countrycode::countrycode(iso2, "iso2c", "iso3c"))

        query_url <- glue(
          "{base_url}/v1/electricity-generation/{frequency}/",
          "?{entity_field}={entity_value}",
          "&is_aggregate_series=false",
          "&start_date=2000",
          "&api_key={key}"
        )
        tryCatch(
          {
            response <- httr::GET(query_url)
            # Read as json from the data argument
            data <- (httr::content(response, "text", encoding = "UTF-8") %>%
              jsonlite::fromJSON())$data
            data %>%
              mutate(
                iso2 = iso2,
                value_mwh = generation_twh * 1e6,
                source = series
              ) %>%
              select(iso2, source, value_mwh, date) %>%
              arrange(desc(date)) %>%
              mutate(
                date = case_when(
                  frequency == "yearly" ~ paste0(date, "-01-01"),
                  T ~ date
                )
              ) %>%
              mutate(date = as.Date(date))
          },
          error = function(e) {
            log_warn(glue::glue("Error for {iso2}: {e$message}"))
            return(NULL)
          }
        )
      }) %>%
        bind_rows()
    }
  )
}

ember.get_installed_capacity <- function(frequency = "monthly", iso2s = "EU", use_cache = TRUE) {
  cache_parquet_get_or_fetch(
    cache_prefix = glue::glue("ember_capacity_{frequency}"),
    cache_key = iso2s,
    use_cache = use_cache,
    cache_schema_version = "v1_parquet_cache",
    fetch_fun = function() {
      readRenviron(".Renviron")
      key <- Sys.getenv("EMBER_KEY")

      # To get entities
      lapply(iso2s, function(iso2) {
        base_url <- "https://api.ember-energy.org"
        entity_field <- ifelse(iso2 == "EU", "entity", "entity_code")
        entity_value <- ifelse(iso2 == "EU", "EU", countrycode::countrycode(iso2, "iso2c", "iso3c"))

        query_url <- paste0(
          glue("{base_url}/v1/installed-capacity/{frequency}"),
          glue(
            paste0(
              "/?{entity_field}={entity_value}&is_aggregate_series=fal",
              "se&start_date=2000&api_key={key}"
            )
          )
        )
        tryCatch(
          {
            response <- httr::GET(query_url)
            # Read as json from the data argument
            data <- (httr::content(response, "text", encoding = "UTF-8") %>%
              jsonlite::fromJSON())$data

            data %>%
              mutate(
                iso2 = iso2,
                value_mw = capacity_gw * 1e3,
                source = series
              ) %>%
              select(iso2, source, value_mw, date) %>%
              arrange(desc(date)) %>%
              mutate(
                date = case_when(
                  frequency == "yearly" ~ paste0(date, "-01-01"),
                  T ~ date
                )
              ) %>%
              mutate(date = as.Date(date))
          },
          error = function(e) {
            return(tibble())
          }
        )
      }) %>%
        bind_rows()
    }
  )
}

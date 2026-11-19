.ember_get <- function(frequency, iso2s, use_cache, capacity = FALSE) {
  if (file.exists(".Renviron")) readRenviron(".Renviron")
  endpoint <- if (capacity) "installed-capacity" else "electricity-generation"
  url <- paste0("https://api.ember-energy.org/v1/", endpoint, "/", frequency, "/")
  dplyr::bind_rows(lapply(iso2s, function(iso2) {
    query <- list(is_aggregate_series = "false", start_date = "2000")
    if (iso2 == "EU") {
      query$entity <- "EU"
    } else {
      query$entity_code <- countrycode::countrycode(iso2, "iso2c", "iso3c")
    }
    name <- paste("ember", endpoint, frequency, iso2, sep = "_")
    query$api_key <- Sys.getenv("EMBER_KEY")
    data <- tryCatch(
      .raw_input_get(name, list(endpoint, frequency, iso2), "json",
        function(path) .raw_http_download(url, query, path),
        function(path) {
          payload <- jsonlite::fromJSON(path)
          if (!is.null(payload$error)) stop("Ember returned an error response.")
          tibble::as_tibble(payload$data)
        }, "ember", use_cache),
      error = function(e) {
        if (inherits(e, "pipeline_artifact_error")) stop(e)
        log_warn(paste0("Ember fetch failed for ", iso2, ": ", conditionMessage(e)))
        NULL
      }
    )
    if (is.null(data) || nrow(data) == 0L) return(tibble::tibble())
    data <- data %>%
      mutate(iso2 = iso2, source = series) %>%
      arrange(desc(date)) %>%
      mutate(date = as.Date(if (frequency == "yearly") paste0(date, "-01-01") else date))
    if (capacity) {
      data <- data %>% mutate(value_mw = capacity_gw * 1e3) %>%
        select(iso2, source, value_mw, date)
    } else {
      data <- data %>% mutate(value_mwh = generation_twh * 1e6) %>%
        select(iso2, source, value_mwh, date)
    }
    .pipeline_snapshot(data, "ember", paste0(name, "_020_normalized"))
  }))
}

ember.get_power_generation <- function(frequency = "yearly", iso2s = "EU", use_cache = TRUE) {
  .ember_get(frequency, iso2s, use_cache)
}

ember.get_installed_capacity <- function(frequency = "monthly", iso2s = "EU", use_cache = TRUE) {
  .ember_get(frequency, iso2s, use_cache, capacity = TRUE)
}

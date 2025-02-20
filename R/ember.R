
ember.get_eu_generation <- function(frequency='yearly'){

    readRenviron(".Renviron")
    key <- Sys.getenv("EMBER_KEY")

    base_url <- "https://api.ember-energy.org"
    query_url <- paste0(
      base_url, "/v1/electricity-generation/", frequency,
      "?entity=EU&is_aggregate_series=false&start_date=2000&api_key=", key
    )
    response <- httr::GET(query_url)
    # Read as json from the data argument
    data <- (httr::content(response, "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON())$data

    # data %>% distinct(is_aggregate_entity, series)

    data %>% mutate(iso2=entity,
                    value_mwh=generation_twh*1e6,
                    source=series
                    ) %>%
      select(iso2, source, value_mwh, date) %>%
      arrange(desc(date)) %>%
      mutate(date=case_when(
        frequency=='yearly' ~ paste0(date, "-01-01"),
        T ~ date
      ))
}


ember.get_power_generation <- function(frequency='yearly', iso2s="EU", use_cache=TRUE){

  # Generate a hash of the parameters
  param_hash <- digest::digest(c(frequency, iso2s))

  # Create a filename using the hash
  cache_dir <- "cache"
  filepath <- file.path(cache_dir, paste0("ember_power_", frequency, "_", param_hash, ".RDS"))

  # Check if the file exists in cache and use_cache is TRUE
  if (use_cache && file.exists(filepath)) {
    return(readRDS(filepath))
  }

  readRenviron(".Renviron")
  key <- Sys.getenv("EMBER_KEY")

  # To get entities
  # glue("https://api.ember-energy.org/options/electricity-generation/yearly/entity?api_key={key}")
  result <- pbapply::pblapply(iso2s, function(iso2){
      base_url <- "https://api.ember-energy.org"
      entity_field <- ifelse(iso2=="EU", "entity", "entity_code")
      entity_value <- ifelse(iso2=="EU", "EU", countrycode::countrycode(iso2, "iso2c", "iso3c"))

      query_url <- paste0(
        glue("{base_url}/v1/electricity-generation/{frequency}"),
        glue("/?{entity_field}={entity_value}&is_aggregate_series=false&start_date=2000&api_key={key}")
      )
      tryCatch({
        response <- httr::GET(query_url)
        # Read as json from the data argument
        data <- (httr::content(response, "text", encoding = "UTF-8") %>%
                   jsonlite::fromJSON())$data
        data %>% mutate(iso2=iso2,
                        value_mwh=generation_twh*1e6,
                        source=series
        ) %>%
          select(iso2, source, value_mwh, date) %>%
          arrange(desc(date)) %>%
          mutate(date=case_when(
            frequency=='yearly' ~ paste0(date, "-01-01"),
            T ~ date
          )) %>%
          mutate(date=as.Date(date))
      }, error=function(e){
        message("Error for ", iso2, ": ", e$message)
        return(NULL)
      })
    }) %>%
      bind_rows()

  # Save the fetched data to cache
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  saveRDS(result, filepath)

  return(result)
}

ember.get_installed_capacity <- function(frequency='monthly', iso2s="EU", use_cache=TRUE){

  # Generate a hash of the parameters
  param_hash <- digest::digest(c(frequency, iso2s))

  # Create a filename using the hash
  cache_dir <- "cache"
  filepath <- file.path(cache_dir, paste0("ember_capacity_", frequency, "_", param_hash, ".RDS"))

  # Check if the file exists in cache and use_cache is TRUE
  if (use_cache && file.exists(filepath)) {
    return(readRDS(filepath))
  }

  readRenviron(".Renviron")
  key <- Sys.getenv("EMBER_KEY")

  # To get entities
  # glue("https://api.ember-energy.org/options/electricity-generation/yearly/entity?api_key={key}")
  result <- lapply(iso2s, function(iso2){
    base_url <- "https://api.ember-energy.org"
    entity_field <- ifelse(iso2=="EU", "entity", "entity_code")
    entity_value <- ifelse(iso2=="EU", "EU", countrycode::countrycode(iso2, "iso2c", "iso3c"))

    query_url <- paste0(
      glue("{base_url}/v1/installed-capacity/{frequency}"),
      glue("/?{entity_field}={entity_value}&is_aggregate_series=false&start_date=2000&api_key={key}")
    )
    tryCatch({
      response <- httr::GET(query_url)
      # Read as json from the data argument
      data <- (httr::content(response, "text", encoding = "UTF-8") %>%
                 jsonlite::fromJSON())$data

      data %>% mutate(iso2=iso2,
                      value_mw=capacity_gw*1e3,
                      source=series
      ) %>%
        select(iso2, source, value_mw, date) %>%
        arrange(desc(date)) %>%
        mutate(date=case_when(
          frequency=='yearly' ~ paste0(date, "-01-01"),
          T ~ date
        )) %>%
        mutate(date=as.Date(date))
    }, error=function(e){
      # message("Error for ", iso2, ": ", e$message)
      return(tibble())
    })
  }) %>%
    bind_rows()

  # Save the fetched data to cache
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  saveRDS(result, filepath)

  return(result)
}

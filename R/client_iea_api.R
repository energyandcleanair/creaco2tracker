iea.get_balance <- function(year_from = 2000, year_to = 2022, iso2, use_cache = TRUE) {
  cache_parquet_get_or_fetch(
    cache_prefix = "ieabalance",
    cache_key = list(year_from = year_from, year_to = year_to, iso2 = iso2),
    use_cache = use_cache,
    cache_schema_version = "v1_parquet_cache",
    fetch_fun = function() {
      pbapply::pblapply(seq(year_from, year_to), function(year) {
        creahelpers::api.get(
          "api.energyandcleanair.org/energy/iea_balance",
          year_from = year,
          year_to = year,
          country = iso2,
          api_key = Sys.getenv("API_KEY")
        )
      }) %>%
        bind_rows()
    }
  )
}


iea.get_conversion_factors <- function(year_from = 2000, year_to = 2022, iso2, use_cache = TRUE) {
  cache_parquet_get_or_fetch(
    cache_prefix = "ieaconversion",
    cache_key = list(year_from = year_from, year_to = year_to, iso2 = iso2),
    use_cache = use_cache,
    cache_schema_version = "v1_parquet_cache",
    fetch_fun = function() {
      log_info(
        glue::glue(
          "Getting conversion factors from IEA for ",
          "{paste(iso2, collapse = ',')} from {year_from} to {year_to}"
        )
      )
      result <- creahelpers::api.get(
        "api.energyandcleanair.org/energy/iea_conversion",
        year_from = year_from,
        year_to = year_to,
        country = iso2,
        api_key = Sys.getenv("API_KEY")
      )

      log_info(glue::glue("Got {nrow(result)} records"))
      result
    }
  )
}

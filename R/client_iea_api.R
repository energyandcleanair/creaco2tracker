iea.get_balance <- function(year_from = 2000, year_to = 2022, iso2, use_cache = TRUE) {
  dplyr::bind_rows(lapply(seq(year_from, year_to), function(year) {
    .crea_raw_get(
      "api.energyandcleanair.org/energy/iea_balance",
      year_from = year, year_to = year, country = iso2,
      api_key = Sys.getenv("API_KEY"), use_cache = use_cache, stage = "iea"
    )
  }))
}

iea.get_conversion_factors <- function(year_from = 2000, year_to = 2022, iso2, use_cache = TRUE) {
  .crea_raw_get(
    "api.energyandcleanair.org/energy/iea_conversion",
    year_from = year_from, year_to = year_to, country = iso2,
    api_key = Sys.getenv("API_KEY"), use_cache = use_cache, stage = "iea"
  )
}

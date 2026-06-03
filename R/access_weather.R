#' Fetch weather data, apply source masking, and fill gaps
#'
#' @param variable Weather variables to fetch.
#' @param region_id Region IDs to fetch.
#' @param date_from Start date.
#' @param date_to End date.
#' @param use_cache Whether to use source-level cache.
#' @param data_masking Optional named list of masking rules.
#'
#' @return Tibble with filled weather data.
#' @keywords internal
weather_data_access_get_filled <- function(
  variable = "HDD,CDD",
  region_id,
  date_from,
  date_to,
  use_cache = TRUE,
  data_masking = NULL
) {
  get_weather(
    variable = variable,
    region_id = region_id,
    date_from = date_from,
    date_to = date_to,
    use_cache = use_cache
  ) %>%
    apply_source_data_mask(
      source_name = "weather",
      data_masking = data_masking
    ) %>%
    fill_weather()
}

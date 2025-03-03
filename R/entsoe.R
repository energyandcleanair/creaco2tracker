entsoe.get_pwr_generation <- function(date_from="2015-01-01", region="EU", use_cache=T, refresh_cache=!use_cache, use_local=F) {

  base_url <- ifelse(use_local, "http://localhost:8080", "https://api.energyandcleanair.org")
  pwr <- creahelpers::api.get(glue('{base_url}/power/generation'),
                              date_from=date_from,
                              aggregate_by='country,source,date',
                              region=region,
                              data_source='entsoe',
                              split_by = 'year',
                              use_cache = use_cache,
                              refresh_cache = refresh_cache,
                              cache_folder = "cache",
                              verbose = T)

  #add total generation
  pwr <- pwr %>%
    filter(source!='Total') %>%
    group_by(iso2, region, country, date) %>%
    dplyr::summarise_at(c("value_mw", "value_mwh"), sum, na.rm=T) %>%
    mutate(source='Total',
           data_source='entsoe'
    ) %>%
    bind_rows(pwr %>% filter(source!='Total'))

  # Assume missing data is zero if the series ended with 0s (e.g. coal in Belgium)
  fill_with_zero_if_last_is_zero <- function(x) {
    if(all(is.na(x))) return(x)
    # Find position of last non-NA value
    last_pos <- max(which(!is.na(x)))
    # Get the last non-NA value
    last_val <- x[last_pos]

    if (last_val == 0 & last_pos < length(x)) {
      # Only fill NAs that come after the last position
      x[(last_pos + 1):length(x)] <- 0
    }
    x
  }

  pwr <- pwr %>%
    ungroup() %>%
    complete(date,
             source,
             data_source="entsoe",
             nesting(iso2, region, country),
             fill = list(value_mw = NA_real_, value_mwh = NA_real_)) %>%
    group_by(iso2, source) %>%
    arrange(date) %>%
    mutate_at(c("value_mw", "value_mwh"),
              fill_with_zero_if_last_is_zero) %>%
    ungroup()


  #add EU total
  pwr <- pwr %>%
    filter(country!='EU total') %>%
    group_by(date, source) %>%
    filter(region=='EU') %>%
    dplyr::summarise_at(c("value_mw", "value_mwh"), sum, na.rm=T) %>%
    mutate(country='EU total',
           iso2='EU') %>%
    bind_rows(pwr %>% filter(country!='EU total')) %>%
    ungroup()


  return(pwr)
}

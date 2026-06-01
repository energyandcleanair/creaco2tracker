entsoe.get_power_generation <- function(
  date_from="2015-01-01",
  date_to=today(),
  iso2s="EU",
  use_cache=T,
  use_local=F) {

  if(all(iso2s=="EU")){
    iso2s_fetch <- get_eu_iso2s(include_eu = F)
  }else{
    iso2s_fetch <- iso2s
  }

  base_url <- ifelse(use_local, "http://localhost:8080", "https://api.energyandcleanair.org")
  pwr <- creahelpers::api.get(glue('{base_url}/energy/power_generation'),
                              date_from=date_from,
                              date_to=date_to,
                              aggregate_by='country,source,date',
                              country=paste(sort(iso2s_fetch), collapse=','),
                              data_source='entsoe',
                              split_by = 'year',
                              # The meaning of use_cache is different
                              # for creahelpers, use_cache means whether or not to use memoise, and refresh_cache means weather or not to invalidate it
                              use_cache = TRUE,
                              refresh_cache = !use_cache,
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


  # add EU total if we have all EU countries, except Cyprus and Malta that are missing from ENTSOE
  if(all(setdiff(get_eu_iso2s(include_eu = F), c("CY", "MT")) %in% pwr$iso2)) {
    pwr <- pwr %>%
      filter(iso2 %in% get_eu_iso2s(include_eu = F)) %>%
      group_by(date, source) %>%
      dplyr::summarise_at(c("value_mw", "value_mwh"), sum, na.rm=T) %>%
      mutate(country='EU total',
             iso2='EU') %>%
      bind_rows(pwr %>% filter(iso2 != 'EU')) %>%
      ungroup()
  }

  pwr <- pwr %>%
    filter(iso2 %in% iso2s)

  return(pwr)
}


entsoe.get_installed_capacity <- function(date_from="2015-01-01", date_to=today(), iso2s="EU", use_cache=T, refresh_cache=!use_cache, use_local=F) {

  if(all(iso2s=="EU")){
    iso2s <- get_eu_iso2s(include_eu = F)
  }

  base_url <- ifelse(use_local, "http://localhost:8080", "https://api.energyandcleanair.org")
  capacity <- creahelpers::api.get(glue('{base_url}/power/installed_capacity'),
                                   date_from=date_from,
                                   date_to=date_to,
                                   aggregate_by='country,source,date',
                                   country=paste(iso2s, collapse=','),
                                   data_source='entsoe',
                                   split_by = 'year',
                                   # The meaning of use_cache is different
                                   # for creahelpers, use_cache means whether or not to use memoise, and refresh_cache means weather or not to invalidate it
                                   use_cache = TRUE,
                                   refresh_cache = !use_cache,
                                   cache_folder = "cache",
                                   verbose = T)

  # Add EU total if we have all EU countries, except Cyprus and Malta that are missing from ENTSOE
  if(all(setdiff(get_eu_iso2s(include_eu = F), c("CY", "MT")) %in% capacity$iso2)) {
    capacity <- capacity %>%
      filter(iso2 %in% get_eu_iso2s(include_eu = F)) %>%
      group_by(date, source) %>%
      summarise(value_mw = sum(value_mw, na.rm = TRUE), .groups = "drop") %>%
      mutate(country = 'EU total',
             iso2 = 'EU',
             data_source = 'entsoe') %>%
      bind_rows(capacity %>% filter(iso2 != 'EU'))
  }

  return(capacity)
}

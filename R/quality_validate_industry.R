validate_industry <- function(industrial_indexes, industrial_prodtrade, industrial_trade){



  # Validate production
  plt_data <- bind_rows(
    industrial_prodtrade %>%
      filter(indicator=="production") %>%
      filter(unit=="kg") %>% ungroup() %>%
      select(iso2, year, nace_r2_code, value) %>%
      mutate(type="production"),
    industrial_indexes %>%
      ungroup() %>%
      filter(estimate=="central") %>%
      group_by(iso2, nace_r2_code, year=year(date)) %>%
      summarise(value=sum(energy_tj),
                type="index")
  ) %>%
    filter(year >= 2015, year < 2025) %>%
    group_by(iso2, nace_r2_code, type) %>%
    mutate(value=value/value[year==2020]) %>%
    filter(iso2=="EU") %>%
    left_join(
      industrial_indexes %>%
        distinct(nace_r2_code, nace_r2)
    )

  ggplot(plt_data, aes(year, value, col=type)) +
    geom_line() +
    facet_wrap(~glue("{nace_r2_code}-{nace_r2}"), scales="free_y") +
    rcrea::scale_y_zero()


  # Validate trade
  plt_data <- bind_rows(
    industrial_prodtrade %>%
      filter(indicator %in% c("export", "import")) %>%
      filter(unit=="kg") %>% ungroup() %>%
      spread(indicator, value) %>%
      mutate(value=import-export) %>%
      select(iso2, year, nace_r2_code, value) %>%
      mutate(source="prodtrade"),
    industrial_trade %>%
      ungroup() %>%
      filter(unit=="kg") %>%
      select(iso2, year, nace_r2_code, value=net_import) %>%
      mutate(source="trade")
  ) %>%
    filter(year >= 2015, year < 2025) %>%
    # group_by(iso2, nace_r2_code, source) %>%
    # mutate(value=value/value[year==2020]) %>%
    filter(iso2=="EU") %>%
    left_join(
      industrial_indexes %>%
        distinct(nace_r2_code, nace_r2)
    )

  ggplot(plt_data, aes(year, value, col=source)) +
    geom_line() +
    facet_wrap(~glue("{nace_r2_code}-{nace_r2}"), scales="free_y")
    # rcrea::scale_y_zero()

}

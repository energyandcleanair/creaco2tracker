validate_power <- function(pwr_generation = NULL, folder = "validation") {
  sources <- power_data_access_get_validation_sources(include_entsoe = is.null(pwr_generation))
  if (is.null(pwr_generation)) pwr_generation <- sources$entsoe
  ember_explorer <- sources$ember
  ember_1 <- ember_explorer %>%
    mutate(
      source = recode(
        source,
        Gas = "Fossil Gas",
        Bioenergy = "Other",
        `Other Fossil` = "Other",
        `Other Renewables` = "Other"
      )
    ) %>%
    group_by(year = year(date), source) %>%
    summarise(
      value_twh = sum(value_mwh) / 1e6,
      data_source = "EMBER (Data Explorer)"
    )


  ember_catalogue <- validation_data_access_get_ember_catalogue()
  ember_2 <- ember_catalogue %>%
    rename(iso3 = `ISO 3 code`) %>%
    mutate(iso2 = countrycode::countrycode(iso3, "iso3c", "iso2c")) %>%
    filter(
      iso2 %in% get_eu_iso2s(),
      EU == 1,
      Unit == "TWh",
      Category == "Electricity generation"
    ) %>%
    filter(
      Variable %in% c(
        "Coal", "Gas", "Hydro", "Nuclear", "Other Fossil", "Other Renewables",
        "Solar", "Wind", "Bioenergy"
      )
    ) %>%
    mutate(
      source = recode(
        Variable,
        Gas = "Fossil Gas",
        Bioenergy = "Other",
        `Other Fossil` = "Other",
        `Other Renewables` = "Other"
      )
    ) %>%
    group_by(
      year = Year,
      source
    ) %>%
    summarise(
      value_twh = sum(Value),
      data_source = "EMBER (Data Catalogue)"
    ) %>%
    ungroup()


  data <- bind_rows(
    ember_1,
    ember_1 %>%
      group_by(year, data_source) %>%
      summarise(value_twh = sum(value_twh), source = "Total"),
    ember_2,
    ember_2 %>%
      group_by(year, data_source) %>%
      summarise(value_twh = sum(value_twh), source = "Total"),
    pwr_generation %>%
      filter(country == "EU total", date < "2026-01-01") %>%
      group_by(year = year(date), source) %>%
      summarise(
        value_twh = sum(value_mwh, na.rm = TRUE) / 1e6,
        data_source = "ENTSOE"
      )
  )

  # EU Comparison
  ggplot(data) +
    geom_line(aes(year, value_twh, col = data_source)) +
    facet_wrap(~source) +
    rcrea::scale_y_crea_zero() +
    labs(
      title = "Comparison of EU power generation",
      subtitle = "TWh",
      x = NULL,
      y = NULL
    )


  # Check why we're missing gas
  ember_catalogue %>%
    filter(
      Variable == "Gas",
      EU == 1,
      Year == max(Year)
    ) %>%
    mutate(
      iso2 = countrycode::countrycode(`Country code`, "iso3c", "iso2c"),
      source = "Fossil Gas",
      year = Year
    ) %>%
    select(iso2, source, year, value_ember = Value) %>%
    left_join(
      pwr_generation %>%
        group_by(iso2, year = year(date), source) %>%
        summarise(value_crea = sum(value_mwh, na.rm = TRUE) / 1e6)
    ) %>%
    pivot_longer(c(value_ember, value_crea), names_to = "data_source", values_to = "value_twh") %>%
    ggplot() +
    geom_col(aes(iso2, value_twh, fill = data_source),
      position = "dodge"
    )


  # Compare countries
  ember_1_per_country <- ember_explorer %>%
    mutate(
      source = recode(
        source,
        Gas = "Fossil Gas",
        Bioenergy = "Other",
        `Other Fossil` = "Other",
        `Other Renewables` = "Other"
      )
    ) %>%
    group_by(iso2, year = year(date)) %>%
    summarise(
      value_twh = sum(value_mwh) / 1e6,
      data_source = "EMBER (Data Explorer)"
    )

  bind_rows(
    ember_1_per_country,
    pwr_generation %>%
      filter(date < "2026-01-01", source == "Total") %>%
      group_by(iso2, year = year(date)) %>%
      summarise(
        value_twh = sum(value_mwh, na.rm = TRUE) / 1e6,
        data_source = "ENTSOE"
      )
  ) %>%
    filter(iso2 != "EU") %>%
    ggplot() +
    geom_line(aes(year, value_twh, col = data_source)) +
    facet_wrap(~iso2) +
    rcrea::scale_y_crea_zero() +
    labs(
      title = "Comparison of EU power generation per country",
      subtitle = "TWh",
      x = NULL,
      y = NULL
    )
}

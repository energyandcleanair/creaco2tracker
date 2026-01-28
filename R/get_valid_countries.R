get_valid_countries <- function(co2,
                                validation_data=NULL,
                                min_year=2020,
                                min_correlation=0.9,
                                max_mae=0.03) {

  get_validity_metrics(co2, validation_data, min_year, min_correlation, max_mae) %>%
    filter(ok) %>%
    pull(iso2)
}

get_validity_metrics <- function(
    co2,
    validation_data=NULL,
    min_year=2020,
    min_correlation=0.9,
    max_mae=0.03) {

  # Get validation data first (similar to validate_co2 function)
  if (is.null(validation_data) | !all(unique(co2$iso2) %in% unique(validation_data$iso2))){
    validation_data <- get_validation_data(region=unique(co2$iso2))
  }


  # Calculate yearly totals for CREA data
  co2_crea <- co2 %>%
    filter(fuel == FUEL_TOTAL,
           sector == SECTOR_ALL,
           estimate == "central") %>%
    group_by(iso2, year = year(date)) %>%
    summarise(value = sum(value)/1e6,
              source = 'CREA',
              .groups = "drop")

  # Filter GCP2 data from validation
  co2_gcp <- validation_data %>%
    filter(source == "Global Carbon Budget 2025",
           sector == SECTOR_ALL,
           fuel == FUEL_TOTAL)

  # Calculate YOY changes for both datasets (focusing on 2020 onwards)
  yoy_comparison <- bind_rows(
    # CREA YOY
    co2_crea %>%
      group_by(iso2) %>%
      arrange(year) %>%
      mutate(yoy = (value / lag(value) - 1)) %>%
      filter(year >= min_year) %>%
      select(iso2, year, yoy, source),

    # GCP2 YOY
    co2_gcp %>%
      group_by(iso2) %>%
      arrange(year) %>%
      mutate(yoy = (value / lag(value) - 1)) %>%
      filter(year >= min_year) %>%
      select(iso2, year, yoy, source)
  )

  # Calculate metrics by country
  metrics <- yoy_comparison %>%
    group_by(iso2, year) %>%
    filter(n()==2 & all(!is.na(yoy))) %>%
    pivot_wider(names_from = source, values_from = yoy) %>%
    group_by(iso2) %>%
    summarise(
      rmse = sqrt(mean((CREA - `Global Carbon Budget 2025`)^2, na.rm = TRUE)),  # Root Mean Square Error
      mae = mean(abs(CREA - `Global Carbon Budget 2025`), na.rm = TRUE),        # Mean Absolute Error
      correlation = cor(CREA, `Global Carbon Budget 2025`, use = "complete.obs"), # Correlation
      n_years = sum(!is.na(CREA) & !is.na(`Global Carbon Budget 2025`)),        # Number of comparable years
      .groups = "drop"
    ) %>%
    mutate(
      ok = correlation >= min_correlation & mae <= max_mae
    )

  return(metrics)
}

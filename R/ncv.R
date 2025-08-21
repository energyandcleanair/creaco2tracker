# Helper function to create SIEC fuel mapping
create_siec_fuel_mapping <- function() {
  tribble(
    ~siec_code, ~product,
    SIEC_HARD_COAL, "Anthracite",
    SIEC_BROWN_COAL, "Lignite",
    SIEC_BROWN_COAL_BRIQUETTES, "BKB/peat briquette plants",
    SIEC_PEAT, "Peat and peat products",
    SIEC_OIL_SHALE, "Oil shale and oil sands",
    SIEC_CRUDE_OIL, "Crude oil",
    SIEC_NATURAL_GAS, "Natural Gas",
    SIEC_COKE_OVEN_COKE, "Coke oven coke",
    SIEC_OIL_PRODUCTS, "Other oil products",
    SIEC_ROAD_DIESEL, "Gas/diesel oil excl. biofuels",
    SIEC_GASOIL_DIESEL, "Gas/diesel oil excl. biofuels",
    SIEC_AVIATION_GASOLINE, "Aviation gasoline",
    SIEC_MOTOR_GASOLINE_XBIO, "Motor gasoline excl. biofuels",
    SIEC_FUEL_OIL, "Fuel oil",
    SIEC_HEATING_GASOIL, "Gas/diesel oil excl. biofuels",
    SIEC_KEROSENE_XBIO, "Kerosene type jet fuel excl. biofuels"
  )
}

# Function to add SIEC codes to IEA data
add_siec_code_to_iea <- function(conversion_raw) {
  siec_fuel <- create_siec_fuel_mapping()

  conversion_raw %>%
    right_join(siec_fuel, by="product", relationship='many-to-many')
}

# Function to process and clean conversion factors
process_conversion_factors <- function(conversion_raw) {
  # Brown coal consists of the addition of lignite and sub-bituminous coal.
  # In 2021, lignite made up 99.6 % of the brown coal consumed in the EU,and sub-bituminous coal 0.4 %.
  # https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Production_of_lignite_in_the_Western_Balkans_-_statistics&oldid=627763
  conversion <- conversion_raw %>%
    filter(flow_raw=="NAVERAGE", unit=="KJKG", iso2 %in% get_eu_iso2s()) %>%
    add_siec_code_to_iea() %>%
    filter(!is.na(value)) %>%
    mutate(source="IEA") %>%
    group_by(iso2, siec_code, year) %>%
    summarise(ncv_kjkg=mean(value),
              source="IEA",
              .groups="drop"
    ) %>%
    filter(!is.na(siec_code))

  # Remove outliers using z-score method
  conversion <- conversion %>%
    group_by(siec_code, iso2) %>%
    summarise(ncv_kjkg=mean(ncv_kjkg),
              .groups="drop") %>%
    group_by(siec_code) %>%
    mutate(zscore=
             case_when(
               sd(ncv_kjkg)==0 ~ 0,
               T ~ (ncv_kjkg-mean(ncv_kjkg))/sd(ncv_kjkg))
    ) %>%
    arrange(desc(zscore)) %>%
    filter(abs(zscore)<2) %>%
    select(-zscore, -ncv_kjkg) %>%
    left_join(conversion) %>%
    ungroup()

  return(conversion)
}

# Function to calculate weighted means for missing values
calculate_weighted_means <- function(conversion, x) {
  conversion %>%
    left_join(
      x %>%
        group_by(iso2, year=year(time), siec_code) %>%
        summarise(qty=sum(values),
                  .groups="drop")
    ) %>%
    mutate(qty=tidyr::replace_na(qty, 0)) %>%
    group_by(siec_code, year) %>%
    summarise(ncv_kjkg_wmean=weighted.mean(ncv_kjkg, qty),
              .groups="drop") %>%
    filter(!is.na(ncv_kjkg_wmean))
}

# Function to fill missing conversion factors
fill_missing_conversion_factors <- function(conversion, conversion_wmean, x) {
  result <- conversion %>%
    ungroup() %>%
    tidyr::complete(
      iso2=unique(add_iso2(x)$iso2),
      siec_code,
      year=seq(min(year(x$time)), max(year(x$time)), by=1)
    ) %>%
    # Fill time wise
    group_by(iso2, siec_code) %>%
    fill(ncv_kjkg, .direction = "downup") %>%
    left_join(conversion_wmean) %>%
    mutate(ncv_kjkg=coalesce(ncv_kjkg, ncv_kjkg_wmean)) %>%
    fill(ncv_kjkg, .direction = "downup") %>%
    select(-c(ncv_kjkg_wmean)) %>%
    ungroup() %>%
    mutate(source=coalesce(source, "IEA (weighted averaged)")) %>%
    # Fill missing years
    group_by(iso2, siec_code) %>%
    arrange(year) %>%
    tidyr::fill(ncv_kjkg, .direction = "downup") %>%
    ungroup()

  # Fill any remaining NA ncv_kjkg with the global average for that siec_code
  global_avg <- conversion %>%
    group_by(siec_code) %>%
    summarise(global_ncv_kjkg = mean(ncv_kjkg, na.rm = TRUE), .groups = "drop")

  result <- result %>%
    left_join(global_avg, by = "siec_code") %>%
    mutate(ncv_kjkg = coalesce(ncv_kjkg, global_ncv_kjkg)) %>%
    select(-global_ncv_kjkg)

  result
}

# Function to add NCV values to input data
add_ncv_to_data <- function(x, conversion_filled) {
  x %>%
    mutate(year=year(time)) %>%
    add_iso2() %>%
    left_join(conversion_filled, by=c("iso2", "siec_code", "year")) %>%
    group_by(iso2, siec_code) %>%
    arrange(time) %>%
    tidyr::fill(ncv_kjkg, .direction = "downup")
}

# Function to validate NCV completeness
validate_ncv_completeness <- function(x_with_ncv) {
  # Check which records are missing NCV values
  missing_ncv <- x_with_ncv %>%
    filter(iso2!='ME', !grepl("Terajoule", unit)) %>%
    filter(is.na(ncv_kjkg))

  if(nrow(missing_ncv) > 0) {
    # Create detailed diagnostic information
    diagnostic_info <- missing_ncv %>%
      group_by(iso2, siec_code, unit) %>%
      summarise(
        n_missing = n(),
        time_range = paste(min(time), "to", max(time)),
        .groups = "drop"
      ) %>%
      arrange(desc(n_missing))

    # Print diagnostic information
    cat("Missing NCV values found:\n")
    print(diagnostic_info)

    # Check if there are any SIEC codes that don't have mappings
    unmapped_siec <- missing_ncv %>%
      distinct(siec_code) %>%
      anti_join(create_siec_fuel_mapping(), by = "siec_code")

    if(nrow(unmapped_siec) > 0) {
      cat("\nUnmapped SIEC codes:\n")
      print(unmapped_siec)
    }

    # Check if there are any countries without conversion factors
    countries_without_ncv <- missing_ncv %>%
      distinct(iso2) %>%
      filter(!iso2 %in% get_eu_iso2s())

    if(nrow(countries_without_ncv) > 0) {
      cat("\nCountries without NCV data:\n")
      print(countries_without_ncv)
    }

    stop("NCV validation failed: ", nrow(missing_ncv), " records are missing NCV values")
  }

  return(TRUE)
}

# Function to make NCV time-insensitive
make_ncv_time_insensitive <- function(x_with_ncv) {
  x_with_ncv %>%
    group_by(iso2, siec_code) %>%
    mutate(ncv_kjkg=mean(ncv_kjkg, na.rm=T))
}

add_ncv_iea <- function(x, diagnostics_folder=NULL){

  conversion_raw <- iea.get_conversion_factors(iso2=c(get_eu_iso2s(), "EU"))

  # Process conversion factors
  conversion <- process_conversion_factors(conversion_raw)

  # Calculate weighted means for missing values
  conversion_wmean <- calculate_weighted_means(conversion, x)

  # Fill missing conversion factors
  conversion_filled <- fill_missing_conversion_factors(conversion, conversion_wmean, x)

  # Run comprehensive diagnostics if folder is provided
  if(!is_null_or_empty(diagnostics_folder)){
    diagnose_ncv_data(conversion_filled, x, diagnostics_folder)
  }

  # Add ncv
  x_with_ncv <- add_ncv_to_data(x, conversion_filled)

  # Validate NCV completeness with detailed diagnostics
  validate_ncv_completeness(x_with_ncv)

  # Make it time insensitive. It otherwise introduces weird patterns
  x_with_ncv <- make_ncv_time_insensitive(x_with_ncv)

  x_with_ncv
}

# New function to get SIEC fuel mapping
get_siec_ipcc_fuel_mapping <- function() {
  tribble(
    ~siec_code, ~fuel,
    SIEC_HARD_COAL, "Anthracite",
    SIEC_BROWN_COAL, "Lignite",
    SIEC_BROWN_COAL_BRIQUETTES, "Brown Coal Briquettes",
    SIEC_CRUDE_OIL, "Crude Oil",
    SIEC_NATURAL_GAS, "Natural Gas",
    SIEC_COKE_OVEN_COKE, "Coke Oven Coke and Lignite Coke",
    SIEC_OIL_PRODUCTS, "Other Petroleum Products",
    SIEC_ROAD_DIESEL, "Diesel Oil",
    SIEC_GASOIL_DIESEL, "Gas Oil",
    SIEC_AVIATION_GASOLINE, "Aviation Gasoline",
    SIEC_MOTOR_GASOLINE_XBIO, "Motor Gasoline",
    SIEC_FUEL_OIL, "Residual Fuel Oil",
    SIEC_HEATING_GASOIL, "Gas Oil",
    SIEC_KEROSENE_XBIO, "Jet Kerosene"
  )
}


get_ipcc_data <- function() {
  read_csv(get_data_filepath('EFDB_output.csv'))
}

get_ipcc_ncv <- function() {
  get_ipcc_data() %>%
    filter(grepl('2006', `Type of parameter`)) %>%
    filter(Unit %in% ("TJ/Gg"),
           Description=="Net Calorific Value (NCV)") %>%
    mutate(ncv_kjkg = as.numeric(Value) * 1e3) %>%
    select(fuel = `Fuel 2006`, ncv_kjkg)
}


add_ncv_ipcc <- function(x, diagnostics_folder=NULL){

  # Get IPCC data
  ipcc <- get_ipcc_ncv()

  # Get SIEC fuel mapping
  siec_fuel <- get_siec_ipcc_fuel_mapping()

  # Map SIEC codes to IPCC fuels
  ncvs <- siec_fuel %>%
    left_join(ipcc, by="fuel") %>%
    select(-c(fuel))

  # Write diagnostics if folder is provided
  if(!is_null_or_empty(diagnostics_folder)){
    ncvs %>%
      write_csv(file.path(diagnostics_folder, "ncv_ipcc.csv"))
  }

  # Add NCVs to the input data
  x_with_ncv <- x %>%
    left_join(ncvs, by = "siec_code")

  # Check for missing NCVs
  missing_ncvs <- x_with_ncv %>%
    filter(is.na(ncv_kjkg)) %>%
    distinct(siec_code)

  if(nrow(missing_ncvs) > 0) {
    warning("Missing NCVs for the following SIEC codes: ",
            paste(missing_ncvs$siec_code, collapse=", "))
  }

  return(x_with_ncv)
}

# Function to diagnose NCV data with visualizations
diagnose_ncv_data <- function(conversion_filled, x = NULL, diagnostics_folder = NULL) {
  library(ggplot2)
  library(dplyr)
  library(lubridate)
  library(ggrepel)

  cat("=== NCV DATA DIAGNOSTIC ===\n")

  # 1. Summary statistics
  cat("1. SUMMARY STATISTICS:\n")
  summary_stats <- conversion_filled %>%
    group_by(siec_code) %>%
    summarise(
      n_total = n(),
      n_missing = sum(is.na(ncv_kjkg)),
      n_present = sum(!is.na(ncv_kjkg)),
      mean_ncv = mean(ncv_kjkg, na.rm = TRUE),
      sd_ncv = sd(ncv_kjkg, na.rm = TRUE),
      min_ncv = min(ncv_kjkg, na.rm = TRUE),
      max_ncv = max(ncv_kjkg, na.rm = TRUE),
      .groups = "drop"
    )

  print(summary_stats)
  cat("\n")

  # 2. Create time series plot with country labels at the end
  # Get the latest year for each country/SIEC combination for labeling
  latest_data <- conversion_filled %>%
    filter(!is.na(ncv_kjkg)) %>%
    group_by(iso2, siec_code) %>%
    filter(year == max(year)) %>%
    ungroup()

  p1 <- conversion_filled %>%
    filter(!is.na(ncv_kjkg)) %>%
    ggplot(aes(x = year, y = ncv_kjkg, color = iso2)) +
    geom_line(alpha = 0.7, size = 0.5, show.legend = F) +
    geom_point(alpha = 0.8, size = 1, show.legend = F) +
    geom_text_repel(
      data = latest_data,
      aes(label = iso2),
      size = 2.5,
      direction = "y",
      hjust = 0,
      segment.size = 0.2,
      segment.alpha = 0.5,
      max.overlaps = 20,
      show.legend = F
    ) +
    facet_wrap(~siec_code, scales = "free_y", ncol = 4) +
    rcrea::scale_y_zero() +
    labs(
      title = "NCV Time Series by Commodity and Country",
      subtitle = "Net Calorific Values (kJ/kg) over time",
      x = "Year",
      y = "NCV (kJ/kg)"
    ) +
    theme_minimal()

  # Save plot and summary stats if diagnostics folder is provided
  if(!is_null_or_empty(diagnostics_folder)) {
    ggsave(file.path(diagnostics_folder, "ncv_time_series.png"), p1,
           width = 16, height = 12, dpi = 300)

    # Save summary statistics
    write_csv(summary_stats, file.path(diagnostics_folder, "ncv_summary_stats.csv"))

    cat("2. PLOT AND SUMMARY STATS SAVED TO:", diagnostics_folder, "\n")
  }

  # Display plot
  # print(p1)
  return(p1)
}

# Generic data source loader
get_validation_data <- function(region="EU", source_name = NULL, ...) {

  # Map source names to their loader functions
  source_loaders <- list(
    # External validation sources
    "Climate Watch" = load_climatewatch_csv,
    "UNFCCC" = load_climatewatch_csv,
    # "GCP" = load_climatewatch_csv,
    "PIK" = load_climatewatch_csv,
    "Global Carbon Budget 2024" = load_gcb,

    # Diagnostic sources
    "Carbon Monitor" = load_carbonmonitor,
    "PRIMAP Energy and Industry" = load_primap,
    "PRIMAP Energy and Industry (excl. Mineral industry)" = load_primap
  )

  # If no source specified, load all available for the region
  if (is.null(source_name)) {
    sources_to_load <- names(source_loaders)
  } else {
    sources_to_load <- source_name
  }

  # Load and combine all requested sources

    lapply(sources_to_load, function(src) {
      message(sprintf("Loading %s data for %s", src, paste(region, collapse = ",")))
      source_loaders[[src]](src, region) %>%
        mutate(source=src)
    }) %>%
      bind_rows() %>%
      # Fill/homogenise country
      mutate(country = countrycode::countrycode(iso2, "iso2c", "country.name",
                                               custom_match = c("EU" = "European Union",
                                                                "EU28" = "EU28"))) %>%
      filter(is.null(region) | iso2 %in% region)
}

load_gcb <- function(source_name, region, ...) {

  # Downloaded from https://zenodo.org/records/17417124

  filepath <- get_data_filepath("GCB2025v15_MtCO2_flat")
  raw <- read_csv(filepath)
  gcb <- raw %>%
    select(iso3=`ISO 3166-1 alpha-3`, Year, Total, Coal, Oil, Gas, Cement, Flaring, Other) %>%
    pivot_longer(-c(Year,iso3),
                 names_prefix='Fuels_',
                 names_to='fuel'
                 ) %>%
    filter(fuel %in% c('Coal', 'Oil', 'Gas')) %>%
    mutate(iso2 = countrycode(iso3, "iso3c", "iso2c"),
      fuel=tolower(fuel),
      source='GCB2025',
      sector=SECTOR_ALL,
      unit='mt',
      # value = value * 44/12, #C to CO2
      country=countrycode(iso2, "iso2c", "country.name")
    ) %>%
    rename(year=Year) %>%
    select(-c(iso3)) %>%
    # Add total
    bind_rows(
      .,
      group_by(., iso2, country, year, sector) %>%
        summarise(value=sum(value),
                  fuel = FUEL_TOTAL,
                  .groups="drop")
    )

  # Add EU
  gcb <- bind_rows(
    gcb,
    gcb %>%
      filter(iso2 %in% get_eu_iso2s()) %>%
      group_by(year, sector, fuel) %>%
      summarise(value=sum(value, na.rm=TRUE), .groups="drop") %>%
      mutate(iso2="EU")
  )

  # Lower case fuel
  gcb$fuel <- tolower(gcb$fuel)
  return(gcb)
}

# Load Carbon Monitor data
load_carbonmonitor <- function(source_name, region, ...) {
  url <- "https://datas.carbonmonitor.org/API/downloadFullDataset.php?source=carbon_eu"
  filepath <- "data/CM_EU.csv"
  if(!file.exists(filepath)){
    dir.create(dirname(filepath), showWarnings = FALSE, recursive = T)
    download.file(url, filepath)
  }

  suppressWarnings(read_csv(filepath, col_types = cols())) %>%
    distinct(country, date, sector, .keep_all = T) %>%
    add_iso2("country") %>%
    # Add EU values
    {
        bind_rows({.}, {.} %>%
                    filter(iso2 %in% get_eu_iso2s()) %>%
                    group_by(date, sector) %>%
                    summarise(value=sum(value, na.rm=TRUE), .groups="drop") %>%
                    mutate(iso2="EU"))
    } %>%
    # parse MM/DD/YYYY
    mutate(date = as.Date(date, format="%d/%m/%Y")) %>%
    group_by(iso2, year=year(date)) %>%
    summarise(
      value=sum(value, na.rm=TRUE),
      unit='mt',
      source=source_name,
      sector=SECTOR_ALL,
      fuel=FUEL_TOTAL,
      distinct_dates=n_distinct(date)
    ) %>%
    filter(distinct_dates >= 365) %>%
    select(-distinct_dates) %>%
    ungroup()
}

# Load PRIMAP data
load_primap <- function(source_name, region, version="2.6", ...) {
  filepath <- "data/Guetschow_et_al_2024a-PRIMAP-hist_v2.6_final_13-Sep-2024.csv"
  url <- "https://zenodo.org/records/13752654/files/Guetschow_et_al_2024a-PRIMAP-hist_v2.6_final_13-Sep-2024.csv?download=1"

  if(!file.exists(filepath)){
    dir.create(dirname(filepath), showWarnings = FALSE, recursive = T)
    download.file(url, filepath)
  }

  with_mineral <- !grepl("excl", source_name, ignore.case=TRUE)

  suppressWarnings(read_csv(filepath, col_types = cols())) %>%
    mutate(iso2 = countrycode(`area (ISO3)`, "iso3c", "iso2c",
                             custom_match = c("EU27BX"="EU"))) %>%
    rename(category=`category (IPCC2006_PRIMAP)`,
           scenario=`scenario (PRIMAP-hist)`) %>%
    filter(
      entity=="CO2",
      iso2 %in% region,
      scenario=='HISTCR'
    ) %>%
    filter(category %in% c(1, 2, "2.A")) %>%
    select(matches("iso2|category|\\d+$")) %>%
    tidyr::gather(key = "year", value = "value", -c(iso2, category)) %>%
    mutate(year=as.numeric(year),
           value=value/1e3) %>%
    {
      if(with_mineral) {
        group_by(., iso2, year) %>%
          filter(category %in% c(1,2)) %>%
          summarise(value=sum(value), .groups = "drop")
      } else {
        group_by(., iso2, year) %>%
          summarise(value=sum(value * case_when(category=="2.A"~-1, T ~1)),
                   .groups="drop")
      }
    } %>%
    mutate(
      unit = "mt",
      source = source_name,
      sector = SECTOR_ALL,
      fuel = FUEL_TOTAL
    )
}

# Generic CSV loader for Climate Watch style sources
load_climatewatch_csv <- function(source_name, region, ...) {

  # Base path for validation data
  base_path <- "ghg-emissions"
  filename <- sprintf("%s-%s.csv", base_path, tolower(gsub(" ", "", source_name)))
  filepath <- get_data_filepath(filename)

  # Load and process data
  suppressWarnings(read_csv(filepath, col_types = cols())) %>%
    filter(!grepl("Data source", iso)) %>%
    mutate(iso2=countrycode(iso, "iso3c", "iso2c", custom_match = c("EUU"="EU"))) %>%
    select(-iso) %>%
    rename(country=`Country/Region`) %>%
    tidyr::gather(key = "year", value = "value", -c(iso2, country, unit)) %>%
    mutate(year = as.numeric(year),
           fuel = FUEL_TOTAL,
           sector = SECTOR_ALL,
           value = as.numeric(value))
}


#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

VALID_OPTIONS <- c(
  "output",
  "source_status",
  "sources",
  "periods",
  "date_to",
  "allow_source_failures",
  "sha"
)

usage <- function() {
  cat(paste(
    "Usage:",
    "  collect_external_co2_sources.R --output <external.csv>",
    "    --source-status <source_status.csv> --date-to <YYYY-MM-DD>",
    "    --sources <all|source-id,...> --periods <annual,monthly>",
    "    --allow-source-failures <true|false> --sha <sha>",
    sep = "\n"
  ), "\n")
}

stop_usage <- function(...) {
  message("Error: ", paste(..., collapse = ""))
  usage()
  quit(status = 1)
}

parse_options <- function(args) {
  opts <- list()
  i <- 1

  while (i <= length(args)) {
    arg <- args[[i]]
    if (!startsWith(arg, "--")) {
      stop_usage("unexpected argument: ", arg)
    }
    if (i == length(args)) {
      stop_usage("missing value for option: ", arg)
    }

    name <- gsub("-", "_", sub("^--", "", arg))
    if (!name %in% VALID_OPTIONS) {
      stop_usage("unknown option: ", arg)
    }
    opts[[name]] <- args[[i + 1]]
    i <- i + 2
  }

  opts
}

require_option <- function(opts, name) {
  value <- opts[[name]]
  if (is.null(value) || !nzchar(value)) {
    stop_usage("missing required option --", gsub("_", "-", name))
  }
  value
}

truthy <- function(value) {
  tolower(value) %in% c("1", "true", "yes", "y")
}

source_catalog <- function() {
  tibble::tribble(
    ~source_id, ~source, ~annual, ~monthly,
    "climate-watch", "Climate Watch", TRUE, FALSE,
    "unfccc", "UNFCCC", TRUE, FALSE,
    "pik", "PIK", TRUE, FALSE,
    "global-carbon-budget-2025", "Global Carbon Budget 2025", TRUE, FALSE,
    "carbon-monitor", "Carbon Monitor", FALSE, TRUE,
    "primap-energy-and-industry", "PRIMAP Energy and Industry", TRUE, FALSE,
    "primap-energy-and-industry-excl-mineral-industry",
    "PRIMAP Energy and Industry (excl. Mineral industry)", TRUE, FALSE
  )
}

split_csv_option <- function(value) {
  value %>%
    strsplit(",", fixed = TRUE) %>%
    unlist(use.names = FALSE) %>%
    trimws() %>%
    discard(~ .x == "")
}

resolve_sources <- function(value) {
  catalog <- source_catalog()
  if (identical(tolower(trimws(value)), "all")) {
    return(catalog)
  }

  requested <- split_csv_option(value)
  matched <- lapply(requested, function(item) {
    item_lower <- tolower(item)
    row <- catalog %>%
      filter(tolower(source_id) == item_lower | tolower(source) == item_lower)
    if (nrow(row) != 1) {
      stop("Unknown source: ", item)
    }
    row
  }) %>%
    bind_rows()

  matched %>% distinct(source_id, .keep_all = TRUE)
}

resolve_periods <- function(value) {
  periods <- split_csv_option(value)
  invalid <- setdiff(periods, c("annual", "monthly"))
  if (length(invalid) > 0) {
    stop("Unknown period(s): ", paste(invalid, collapse = ", "))
  }
  unique(periods)
}

empty_external <- function() {
  tibble(
    source_id = character(),
    source = character(),
    period = character(),
    iso2 = character(),
    date = as.Date(character()),
    year = integer(),
    value_mt = numeric(),
    unit = character()
  )
}

source_status_row <- function(source_id, source, period, status, message = "", rows = 0L) {
  tibble(
    source_id = source_id,
    source = source,
    period = period,
    status = status,
    message = message,
    rows = as.integer(rows)
  )
}

normalise_annual_validation_source <- function(source_id, source, region) {
  get_validation_data(region = region, source_name = source) %>%
    filter(
      iso2 %in% region,
      fuel == FUEL_TOTAL,
      sector == SECTOR_ALL,
      !is.na(year),
      !is.na(value)
    ) %>%
    transmute(
      source_id = source_id,
      source = source,
      period = "annual",
      iso2 = iso2,
      date = as.Date(paste0(as.integer(year), "-01-01")),
      year = as.integer(year),
      value_mt = as.numeric(value),
      unit = "Mt"
    ) %>%
    distinct(source_id, period, iso2, date, .keep_all = TRUE)
}

download_carbonmonitor_raw <- function() {
  url <- "https://datas.carbonmonitor.org/API/downloadFullDataset.php?source=carbon_eu"
  filepath <- "data/CM_EU.csv"
  if (!file.exists(filepath)) {
    dir.create(dirname(filepath), showWarnings = FALSE, recursive = TRUE)
    download.file(url, filepath)
  }

  suppressWarnings(read_csv(filepath, col_types = cols()))
}

normalise_carbonmonitor_monthly <- function(source_id, source, region) {
  country_lookup <- c(
    "AUSTRIA" = "AT",
    "BELGIUM" = "BE",
    "BULGARIA" = "BG",
    "CROATIA" = "HR",
    "CYPRUS" = "CY",
    "CZECH REPUBLIC" = "CZ",
    "DENMARK" = "DK",
    "ESTONIA" = "EE",
    "FINLAND" = "FI",
    "FRANCE" = "FR",
    "GERMANY" = "DE",
    "GREECE" = "GR",
    "HUNGARY" = "HU",
    "IRELAND" = "IE",
    "ITALY" = "IT",
    "LATVIA" = "LV",
    "LITHUANIA" = "LT",
    "LUXEMBOURG" = "LU",
    "MALTA" = "MT",
    "NETHERLANDS" = "NL",
    "POLAND" = "PL",
    "PORTUGAL" = "PT",
    "ROMANIA" = "RO",
    "SLOVAKIA" = "SK",
    "SLOVENIA" = "SI",
    "SPAIN" = "ES",
    "SWEDEN" = "SE",
    "UNITED KINGDOM" = "GB",
    "EU27 & UK" = "EU28"
  )

  raw <- download_carbonmonitor_raw() %>%
    distinct(country, date, sector, .keep_all = TRUE) %>%
    mutate(country_key = str_squish(str_to_upper(country)))

  country_rows <- raw %>%
    mutate(iso2 = recode(country_key, !!!country_lookup, .default = NA_character_)) %>%
    filter(iso2 %in% region, iso2 != "EU")

  eu_rows <- raw %>%
    filter(country_key %in% c("EU27 & UK", "UNITED KINGDOM")) %>%
    group_by(date, sector) %>%
    summarise(
      value = sum(value * if_else(country_key == "UNITED KINGDOM", -1, 1), na.rm = TRUE),
      country = "EU27",
      iso2 = "EU",
      .groups = "drop"
    )

  bind_rows(country_rows, eu_rows) %>%
    mutate(day = as.Date(lubridate::fast_strptime(date, "%d/%m/%Y"))) %>%
    filter(!is.na(day), iso2 %in% region) %>%
    group_by(iso2, date = floor_date(day, "month")) %>%
    summarise(
      value_mt = sum(value, na.rm = TRUE),
      distinct_dates = n_distinct(day),
      days_in_period = days_in_month(first(date)),
      .groups = "drop"
    ) %>%
    filter(distinct_dates >= days_in_period) %>%
    transmute(
      source_id = source_id,
      source = source,
      period = "monthly",
      iso2 = iso2,
      date = date,
      year = year(date),
      value_mt = value_mt,
      unit = "Mt"
    )
}

collect_one_source_period <- function(source_row, period, region) {
  source_id <- source_row$source_id[[1]]
  source <- source_row$source[[1]]

  if (period == "annual" && !isTRUE(source_row$annual[[1]])) {
    return(list(
      data = empty_external(),
      status = source_status_row(
        source_id,
        source,
        period,
        "skipped",
        "Annual comparison is not supported for this source in v1."
      )
    ))
  }

  if (period == "monthly" && !isTRUE(source_row$monthly[[1]])) {
    return(list(
      data = empty_external(),
      status = source_status_row(
        source_id,
        source,
        period,
        "skipped",
        "Monthly comparison is only supported for Carbon Monitor in v1."
      )
    ))
  }

  data <- if (period == "annual") {
    normalise_annual_validation_source(source_id, source, region)
  } else if (source_id == "carbon-monitor") {
    normalise_carbonmonitor_monthly(source_id, source, region)
  } else {
    empty_external()
  }

  if (nrow(data) == 0) {
    stop("No normalized rows produced.")
  }

  list(
    data = data,
    status = source_status_row(source_id, source, period, "ok", rows = nrow(data))
  )
}

collect_external_sources <- function(sources, periods, allow_source_failures) {
  region <- get_eu_iso2s(include_eu = TRUE)

  results <- list()
  statuses <- list()
  index <- 1L

  for (i in seq_len(nrow(sources))) {
    source_row <- sources[i, ]
    for (period in periods) {
      result <- tryCatch(
        collect_one_source_period(source_row, period, region),
        error = function(e) {
          list(
            data = empty_external(),
            status = source_status_row(
              source_row$source_id[[1]],
              source_row$source[[1]],
              period,
              "failed",
              conditionMessage(e)
            )
          )
        }
      )

      results[[index]] <- result$data
      statuses[[index]] <- result$status
      index <- index + 1L
    }
  }

  data <- bind_rows(results)
  status <- bind_rows(statuses)

  failed <- status %>% filter(status == "failed")
  if (nrow(failed) > 0 && !allow_source_failures) {
    stop(
      "Failed to collect external source(s): ",
      paste(glue::glue("{failed$source} [{failed$period}]: {failed$message}"), collapse = "; ")
    )
  }

  list(data = data, status = status)
}

run_collect <- function(opts) {
  output <- require_option(opts, "output")
  source_status <- require_option(opts, "source_status")
  date_to <- as.Date(require_option(opts, "date_to"))
  sources <- resolve_sources(require_option(opts, "sources"))
  periods <- resolve_periods(require_option(opts, "periods"))
  allow_source_failures <- truthy(require_option(opts, "allow_source_failures"))
  sha <- require_option(opts, "sha")

  if (is.na(date_to)) {
    stop("Invalid --date-to value.")
  }

  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(source_status), recursive = TRUE, showWarnings = FALSE)
  dir.create("cache", recursive = TRUE, showWarnings = FALSE)

  message("[collect_external_co2_sources.R] Loading package at ", sha)
  devtools::load_all(".", quiet = TRUE)
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_threshold(logger::TRACE)
  }

  message(
    "[collect_external_co2_sources.R] Collecting sources: ",
    paste(sources$source_id, collapse = ", "),
    " for periods: ",
    paste(periods, collapse = ", ")
  )

  collected <- collect_external_sources(
    sources = sources,
    periods = periods,
    allow_source_failures = allow_source_failures
  )

  write_csv(collected$data, output, na = "")
  write_csv(collected$status, source_status, na = "")

  if (!file.exists(output) || file.info(output)$size <= 0) {
    stop("Failed to write usable external source output: ", output)
  }
  if (!file.exists(source_status) || file.info(source_status)$size <= 0) {
    stop("Failed to write usable source status output: ", source_status)
  }

  message("[collect_external_co2_sources.R] Wrote ", output)
  message("[collect_external_co2_sources.R] Wrote ", source_status)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) {
    usage()
    quit(status = 1)
  }

  run_collect(parse_options(args))
}

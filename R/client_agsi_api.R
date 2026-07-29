AGSI_COUNTRY_DAILY_URL <- paste0(
  "https://storage.googleapis.com/crea-aq-data-agsi-public/agsi/current/",
  "request_version%3D1/country_daily.parquet"
)

AGSI_COUNTRY_DAILY_CACHE_FILENAME <- "agsi_country_daily.parquet"


.agsi_validate_bundle <- function(filepath) {
  dataset <- arrow::open_dataset(filepath, format = "parquet")
  required_columns <- c("country_code", "gas_day", "net_withdrawal")
  missing_columns <- setdiff(required_columns, dataset$schema$names)

  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "AGSI country-daily parquet is missing required columns: ",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(filepath)
}

.agsi_check_recent_values <- function(
  data,
  date_to,
  days = 3,
  reference_date = lubridate::today()
) {
  date_to <- as.Date(date_to)
  reference_date <- as.Date(reference_date)
  recent_date_from <- reference_date - lubridate::days(days)

  if (length(date_to) != 1 || is.na(date_to) || date_to < recent_date_from) {
    return(invisible(data))
  }

  has_recent_values <- nrow(data) > 0 &&
    all(c("date", "value_gwh") %in% names(data)) &&
    any(
      !is.na(data$date) &
        data$date >= recent_date_from &
        !is.na(data$value_gwh)
    )

  if (!has_recent_values) {
    stop(
      paste0(
        "AGSI storage data has no values since ", recent_date_from,
        "; the latest value must be within ", days, " days of ", reference_date, "."
      ),
      call. = FALSE
    )
  }

  invisible(data)
}


.agsi_download_bundle <- function(filepath, url = AGSI_COUNTRY_DAILY_URL) {
  status <- utils::download.file(
    url = url,
    destfile = filepath,
    mode = "wb",
    quiet = TRUE
  )

  if (!identical(status, 0L)) {
    stop(
      paste0("Failed to download AGSI country-daily parquet; status ", status),
      call. = FALSE
    )
  }

  invisible(filepath)
}


.agsi_build_bundle_query <- function(filepath, date_from, date_to, iso2) {
  date_from <- as.Date(date_from)
  date_to <- as.Date(date_to)

  arrow::open_dataset(filepath, format = "parquet") %>%
    filter(
      .data$country_code %in% iso2,
      .data$gas_day >= date_from,
      .data$gas_day <= date_to
    ) %>%
    select(
      iso2 = "country_code",
      date = "gas_day",
      value_gwh = "net_withdrawal"
    )
}


.agsi_query_bundle <- function(filepath, date_from, date_to, iso2) {
  .agsi_build_bundle_query(
    filepath = filepath,
    date_from = date_from,
    date_to = date_to,
    iso2 = iso2
  ) %>%
    collect()
}


agsi.get_storage_change <- function(date_from, date_to, iso2, use_cache = TRUE, verbose = FALSE) {
  log_info("Getting AGSI storage change data from {date_from} to {date_to}")

  bundle_path <- if (use_cache) {
    file.path(
      creaco2tracker_cache_dir(),
      AGSI_COUNTRY_DAILY_CACHE_FILENAME
    )
  } else {
    file.path(
      tempdir(),
      AGSI_COUNTRY_DAILY_CACHE_FILENAME
    )
  }

  data <- cache_file_get_or_refresh(
    filepath = bundle_path,
    populate_fun = .agsi_download_bundle,
    consume_fun = function(filepath) {
      .agsi_query_bundle(
        filepath = filepath,
        date_from = date_from,
        date_to = date_to,
        iso2 = iso2
      )
    },
    use_cache = use_cache,
    is_fresh_fun = cache_file_modified_today,
    validate_fun = .agsi_validate_bundle
  ) %>%
    mutate(
      value_gwh = suppressWarnings(as.numeric(.data$value_gwh)),
      value_m3 = .data$value_gwh * 1e6 / gcv_kwh_m3,
      type = "storage_drawdown"
    ) %>%
    tibble()

  .agsi_check_recent_values(data = data, date_to = date_to)
  data
}

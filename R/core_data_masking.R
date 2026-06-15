#' Return default data masking configuration
#'
#' `get_data_masking_config()` returns a template configuration that can be
#' extended per scenario. Rules can be defined globally (`all`) and/or per source.
#'
#' `date_from` removes matching rows on and after that date. `date_to` removes
#' matching rows on and before that date. When both are present, the inclusive
#' date range between them is removed.
#'
#' Supported source keys include granular pre-blend sources:
#' - `entsoe_power_daily`, `ember_power_monthly`, `ember_power_yearly`
#' - `entsog_flow_raw`, `agsi_storage_daily`, `eurostat_gas_monthly_for_correction`
#' - `eurostat_oil_monthly`, `eurostat_oil_yearly`
#' - `eurostat_solid_monthly`, `eurostat_solid_yearly`
#' - `eurostat_gas_monthly`, `eurostat_gas_yearly`
#' - `eurostat_indprod`, `weather`
#'
#' Each rule is a named list. Reserved rule fields:
#' - `date_from`
#' - `date_to`
#' - `name`
#' - `comment`
#'
#' Unknown source keys, removed legacy fields, and filter columns that are not
#' present in the corresponding dataset raise errors.
#'
#' Any other field is interpreted as a column filter that must match values in
#' the corresponding dataset (for example `iso2`, `source`, `fuel`, `variable`).
#'
#' @return Named list with masking slots for all known sources.
#' @export
get_data_masking_config <- function() {
  list(
    all = list(),
    entsog_flow_raw = list(),
    agsi_storage_daily = list(),
    eurostat_gas_monthly_for_correction = list(),
    entsoe_power_daily = list(),
    ember_power_monthly = list(),
    ember_power_yearly = list(),
    eurostat_oil_monthly = list(),
    eurostat_oil_yearly = list(),
    eurostat_solid_monthly = list(),
    eurostat_solid_yearly = list(),
    eurostat_gas_monthly = list(),
    eurostat_gas_yearly = list(),
    eurostat_indprod = list(),
    weather = list()
  )
}


#' Default publication lags for forward validation masking
#'
#' Returns conservative typical publication delays, in days, for the source
#' slots supported by [`get_data_masking_config()`]. These defaults are meant
#' to approximate what would have been available on a given reference date and
#' can be overridden with empirically verified values.
#'
#' The returned values are interpreted against each source's reporting cadence.
#' Daily sources use the raw day cutoff; monthly sources are truncated to the
#' previous complete month; yearly sources are truncated to the previous
#' complete year.
#'
#' @return Named integer vector of publication lags in days.
#' @export
default_source_lags <- function() {
  c(
    entsoe_power_daily = 2L,
    ember_power_monthly = 60L,
    ember_power_yearly = 195L,
    entsog_flow_raw = 2L,
    agsi_storage_daily = 2L,
    eurostat_gas_monthly_for_correction = 100L,
    eurostat_oil_monthly = 100L,
    eurostat_oil_yearly = 365L,
    eurostat_solid_monthly = 100L,
    eurostat_solid_yearly = 365L,
    eurostat_gas_monthly = 100L,
    eurostat_gas_yearly = 365L,
    eurostat_indprod = 60L,
    weather = 2L
  )
}


#' Default annual publication months for forward validation masking
#'
#' Returns conservative month-of-year defaults for annual sources supported by
#' [`get_data_masking_config()`]. The values indicate when year `Y` data first
#' becomes available in year `Y + 1`.
#'
#' @return Named integer vector of annual publication months.
#' @export
default_source_publication_months <- function() {
  c(
    ember_power_yearly = 2L,
    eurostat_oil_yearly = 7L,
    eurostat_solid_yearly = 7L,
    eurostat_gas_yearly = 7L
  )
}


#' Build a masking config as of a reference date
#'
#' This helper constructs a [`get_data_masking_config()`]-compatible list that
#' masks each source independently after the last period that would have been
#' available on `date` after accounting for its publication lag.
#' The generated rules use `date_from` as an inclusive lower bound on rows to
#' remove, so data on `date_from` and every later date is masked out.
#'
#' @param date Reference date for the historical snapshot.
#' @param lags Named integer vector of publication lags in days. Defaults to
#'   [`default_source_lags()`]. Unspecified sources fall back to the defaults.
#' @param publication_months Named integer vector of annual publication months.
#'   Defaults to [`default_source_publication_months()`]. For annual sources
#'   listed here, year `Y` becomes available on the first day of the specified
#'   month in year `Y + 1`, and the corresponding lag is ignored. Set to `NULL`
#'   to use lag-based handling for all annual sources.
#' @return Masking configuration in the same structure as
#'   [`get_data_masking_config()`].
#' @export
data_masking_as_of <- function(
  date,
  lags = default_source_lags(),
  publication_months = default_source_publication_months()
) {
  reference_date <- as.Date(date)
  if (length(reference_date) != 1 || is.na(reference_date)) {
    stop("date must be coercible to Date")
  }

  defaults <- default_source_lags()
  if (!is.null(lags)) {
    lags <- .validate_lags(lags)
    defaults[names(lags)] <- lags
  }

  publication_months <- .validate_publication_months(publication_months)
  config <- get_data_masking_config()
  cadence_map <- .default_source_publication_cadence()
  for (source_key in names(defaults)) {
    if (!source_key %in% names(config)) {
      next
    }

    source_cadence <- cadence_map[[source_key]]
    if (is.null(source_cadence)) {
      source_cadence <- "day"
    }

    source_publication_month <- publication_months[source_key]
    if (!is.null(source_publication_month) && !is.na(source_publication_month)) {
      unavailable_from <- .annual_unavailable_from(
        reference_date = reference_date,
        publication_month = source_publication_month
      )
      config[[source_key]] <- list(date_from = as.character(unavailable_from))
      next
    }

    cutoff_date <- .publication_cutoff_date(
      reference_date = reference_date,
      lag_days = defaults[[source_key]],
      cadence = source_cadence
    )
    unavailable_from <- .first_unavailable_date(
      cutoff_date = cutoff_date,
      cadence = source_cadence
    )
    config[[source_key]] <- list(date_from = as.character(unavailable_from))
  }

  config
}


#' Build masking configs for a vector of reference dates
#'
#' Applies [`data_masking_as_of()`] to each date and returns the results in a
#' named list keyed by ISO date string.
#'
#' @param dates Vector of reference dates.
#' @param lags Named integer vector of publication lags in days.
#' @param publication_months Named integer vector of annual publication months.
#' @return Named list of masking configurations.
#' @export
data_masking_as_of_batch <- function(
  dates,
  lags = default_source_lags(),
  publication_months = default_source_publication_months()
) {
  reference_dates <- as.Date(dates)
  if (anyNA(reference_dates)) {
    stop("dates must be coercible to Date")
  }

  configs <- lapply(
    as.character(reference_dates),
    data_masking_as_of,
    lags = lags,
    publication_months = publication_months
  )
  names(configs) <- as.character(reference_dates)
  configs
}


.resolve_data_masking_config <- function(
  data_masking,
  reference_date = NULL,
  lags = default_source_lags(),
  publication_months = default_source_publication_months()
) {
  valid_modes_message <- paste(
    "data_masking must be DATA_MASKING_NONE, DATA_MASKING_HISTORICAL_DEFAULTS,",
    "or a named masking config list"
  )

  if (is.null(data_masking)) {
    return(NULL)
  }

  if (is.list(data_masking)) {
    .validate_data_masking_config(data_masking)
    return(data_masking)
  }

  if (!is.character(data_masking) || length(data_masking) != 1) {
    stop(valid_modes_message)
  }

  if (identical(data_masking, DATA_MASKING_NONE)) {
    return(NULL)
  }

  if (identical(data_masking, DATA_MASKING_HISTORICAL_DEFAULTS)) {
    if (is.null(reference_date)) {
      reference_date <- lubridate::today()
    }
    return(
      data_masking_as_of(
        reference_date,
        lags = lags,
        publication_months = publication_months
      )
    )
  }

  stop(valid_modes_message)
}


.validate_lags <- function(lags) {
  if (is.null(names(lags))) {
    stop("lags must be a named vector")
  }

  valid_sources <- names(default_source_lags())
  invalid_sources <- setdiff(names(lags), valid_sources)
  if (length(invalid_sources) > 0) {
    stop(
      glue::glue(
        "lags can only include sources with default publication lags: ",
        "{paste(invalid_sources, collapse = ', ')}"
      )
    )
  }

  invalid_lags <- is.na(lags) | (lags %% 1 != 0) | lags < 0
  if (any(invalid_lags)) {
    stop("lags values must be whole numbers greater than or equal to 0")
  }

  stats::setNames(as.integer(lags), names(lags))
}


.validate_publication_months <- function(publication_months) {
  if (is.null(publication_months)) {
    return(NULL)
  }

  if (is.null(names(publication_months))) {
    stop("publication_months must be a named vector")
  }

  cadence_map <- .default_source_publication_cadence()
  valid_yearly_sources <- names(cadence_map[cadence_map == "year"])
  invalid_sources <- setdiff(names(publication_months), valid_yearly_sources)
  if (length(invalid_sources) > 0) {
    stop(
      glue::glue(
        "publication_months can only include yearly sources: ",
        "{paste(invalid_sources, collapse = ', ')}"
      )
    )
  }

  invalid_months <- is.na(publication_months) |
    (publication_months %% 1 != 0) |
    publication_months < 1 |
    publication_months > 12
  if (any(invalid_months)) {
    stop("publication_months values must be whole months between 1 and 12")
  }

  stats::setNames(as.integer(publication_months), names(publication_months))
}


.default_source_publication_cadence <- function() {
  c(
    entsoe_power_daily = "day",
    ember_power_monthly = "month",
    ember_power_yearly = "year",
    entsog_flow_raw = "day",
    agsi_storage_daily = "day",
    eurostat_gas_monthly_for_correction = "month",
    eurostat_oil_monthly = "month",
    eurostat_oil_yearly = "year",
    eurostat_solid_monthly = "month",
    eurostat_solid_yearly = "year",
    eurostat_gas_monthly = "month",
    eurostat_gas_yearly = "year",
    eurostat_indprod = "month",
    weather = "day"
  )
}


.publication_cutoff_date <- function(reference_date, lag_days, cadence) {
  cutoff_date <- reference_date - as.difftime(lag_days, units = "days")

  if (cadence == "day") {
    return(cutoff_date)
  }

  if (cadence == "month") {
    if (lag_days <= 0) {
      return(lubridate::floor_date(reference_date, "month"))
    }
    return(.shift_months(lubridate::floor_date(cutoff_date, "month"), -1))
  }

  if (cadence == "year") {
    if (lag_days <= 0) {
      return(lubridate::floor_date(reference_date, "year"))
    }
    return(
      as.Date(
        sprintf(
          "%d-01-01",
          as.integer(
            format(
              lubridate::floor_date(
                cutoff_date,
                "year"
              ),
              "%Y"
            )
          ) - 1L
        )
      )
    )
  }

  cutoff_date
}


.first_unavailable_date <- function(cutoff_date, cadence) {
  cutoff_date <- as.Date(cutoff_date)

  if (cadence == "month") {
    return(.shift_months(cutoff_date, 1))
  }

  if (cadence == "year") {
    return(
      as.Date(sprintf("%d-01-01", as.integer(format(cutoff_date, "%Y")) + 1L))
    )
  }

  cutoff_date + 1
}


.annual_unavailable_from <- function(reference_date, publication_month) {
  reference_date <- as.Date(reference_date)
  year_offset <- ifelse(lubridate::month(reference_date) >= publication_month, 0L, -1L)
  unavailable_year <- lubridate::year(reference_date) + year_offset

  as.Date(sprintf("%d-01-01", unavailable_year))
}


.shift_months <- function(date, months) {
  date <- as.Date(date)
  if (is.na(date) || length(date) != 1) {
    return(date)
  }

  date_time <- as.POSIXlt(date)
  date_time$mon <- date_time$mon + as.integer(months)
  as.Date(date_time)
}


.data_source_mask_specs <- function() {
  list(
    entsog_flow_raw = list(
      date_col = "date"
    ),
    agsi_storage_daily = list(
      date_col = "date"
    ),
    eurostat_gas_monthly_for_correction = list(
      date_col = "date"
    ),
    entsoe_power_daily = list(
      date_col = "date"
    ),
    ember_power_monthly = list(
      date_col = "date"
    ),
    ember_power_yearly = list(
      date_col = "date"
    ),
    eurostat_oil_monthly = list(
      date_col = "time"
    ),
    eurostat_oil_yearly = list(
      date_col = "time"
    ),
    eurostat_solid_monthly = list(
      date_col = "time"
    ),
    eurostat_solid_yearly = list(
      date_col = "time"
    ),
    eurostat_gas_monthly = list(
      date_col = "time"
    ),
    eurostat_gas_yearly = list(
      date_col = "time"
    ),
    eurostat_indprod = list(
      date_col = "time"
    ),
    weather = list(
      date_col = "date"
    )
  )
}


.validate_mask_source_name <- function(source_name) {
  if (is.null(source_name) || length(source_name) != 1) {
    stop("source_name must be a single character value")
  }

  source_name <- as.character(source_name)
  specs <- .data_source_mask_specs()

  if (!source_name %in% names(specs)) {
    stop(
      glue::glue(
        "Unknown masking source '{source_name}'. Use a canonical source key from ",
        "get_data_masking_config()."
      )
    )
  }

  source_name
}


.reserved_mask_rule_fields <- function() {
  c(
    "date_from", "date_to",
    "name", "comment"
  )
}


.validate_data_masking_config <- function(data_masking) {
  if (!is.list(data_masking)) {
    stop("data_masking must be a list")
  }

  config_names <- names(data_masking)
  if (is.null(config_names) || anyNA(config_names) || any(config_names == "")) {
    stop("data_masking must be a named list")
  }

  valid_keys <- names(get_data_masking_config())
  invalid_sources <- setdiff(config_names, valid_keys)
  if (length(invalid_sources) > 0) {
    stop(
      glue::glue(
        "Unknown masking source keys: {paste(invalid_sources, collapse = ', ')}. ",
        "Use canonical source keys from get_data_masking_config()."
      )
    )
  }
}


.rule_list_from_config <- function(config) {
  if (is.null(config)) {
    return(list())
  }

  if (!is.list(config)) {
    stop("Masking config must be a list")
  }

  if (length(config) == 0) {
    return(list())
  }

  reserved <- .reserved_mask_rule_fields()
  nm <- names(config)
  has_rule_names <- !is.null(nm) && any(nm %in% reserved)
  any_non_list <- any(!vapply(config, is.list, logical(1)))

  if (has_rule_names || any_non_list) {
    return(list(config))
  }

  config
}


.validate_mask_rule_date <- function(value, field_name, source_name) {
  if (is.null(value)) {
    return(invisible(NULL))
  }

  parsed_value <- as.Date(value)
  if (length(parsed_value) != 1 || is.na(parsed_value)) {
    stop(glue::glue("Masking rule for source '{source_name}' has invalid {field_name}."))
  }

  invisible(NULL)
}


.validate_mask_rule <- function(rule, source_name, date_col, data_columns) {
  if (!is.list(rule)) {
    stop(glue::glue("Masking rules for source '{source_name}' must be lists."))
  }

  if (length(rule) == 0) {
    stop(glue::glue("Masking rules for source '{source_name}' must not be empty."))
  }

  rule_names <- names(rule)
  if (is.null(rule_names) || anyNA(rule_names) || any(rule_names == "")) {
    stop(glue::glue("Masking rules for source '{source_name}' must be named lists."))
  }

  legacy_fields <- intersect(rule_names, c("available_from", "available_to"))
  if (length(legacy_fields) > 0) {
    stop(
      glue::glue(
        "Masking rule for source '{source_name}' uses removed legacy fields: ",
        "{paste(legacy_fields, collapse = ', ')}. Use date_from/date_to instead."
      )
    )
  }

  .validate_mask_rule_date(rule$date_from, "date_from", source_name)
  .validate_mask_rule_date(rule$date_to, "date_to", source_name)
  if (!is.null(rule$date_from) && !is.null(rule$date_to)) {
    if (as.Date(rule$date_from) > as.Date(rule$date_to)) {
      stop(glue::glue(
        "Masking rule for source '{source_name}' must satisfy date_from <= date_to."
      ))
    }
  }

  filter_fields <- setdiff(rule_names, .reserved_mask_rule_fields())
  invalid_fields <- setdiff(filter_fields, data_columns)
  if (length(invalid_fields) > 0) {
    stop(
      glue::glue(
        "Masking rule for source '{source_name}' references missing columns: ",
        "{paste(invalid_fields, collapse = ', ')}."
      )
    )
  }

  if (!date_col %in% data_columns) {
    stop(glue::glue(
      "Date column '{date_col}' is missing for source '{source_name}'."
    ))
  }
}


.build_mask_index <- function(x, date_col, rule) {
  idx <- rep(TRUE, nrow(x))

  if (!date_col %in% names(x)) {
    return(rep(FALSE, nrow(x)))
  }

  date_values <- as.Date(x[[date_col]])

  if (!is.null(rule$date_from)) {
    idx <- idx & date_values >= as.Date(rule$date_from)
  }
  if (!is.null(rule$date_to)) {
    idx <- idx & date_values <= as.Date(rule$date_to)
  }

  filter_fields <- setdiff(names(rule), .reserved_mask_rule_fields())
  for (field in filter_fields) {
    if (!field %in% names(x)) {
      next
    }
    values <- unlist(rule[[field]])
    idx <- idx & x[[field]] %in% values
  }

  idx
}


#' Mask source data to simulate historical data availability
#'
#' Applies source-specific masking rules by removing matching source rows. This
#' simulates historical unavailability as absent data, avoiding downstream
#' processing that can interpret unavailable `NA` values as observed zeros.
#' `date_from` removes rows on and after that date; `date_to` removes rows on
#' and before that date. Invalid source keys, removed legacy fields, and
#' missing filter columns raise errors.
#'
#' @param x Data frame to mask.
#' @param source_name Canonical source key (for example `"entsoe_power_daily"`).
#' @param data_masking Named list of masking rules.
#' @return Data frame with unavailable rows removed.
#' @export
apply_source_data_mask <- function(x, source_name, data_masking = NULL) {
  if (is.null(data_masking)) {
    return(x)
  }

  .validate_data_masking_config(data_masking)
  source_key <- .validate_mask_source_name(source_name)

  if (is.null(x) || nrow(x) == 0) {
    return(x)
  }

  specs <- .data_source_mask_specs()[[source_key]]
  source_rules <- .rule_list_from_config(data_masking[[source_key]])
  global_rules <- .rule_list_from_config(data_masking$all)
  rules <- c(global_rules, source_rules)

  if (length(rules) == 0) {
    return(x)
  }

  date_col <- specs$date_col
  for (rule in rules) {
    .validate_mask_rule(
      rule,
      source_name = source_key,
      date_col = date_col,
      data_columns = names(x)
    )
    mask_idx <- .build_mask_index(x, date_col = date_col, rule = rule)

    if (!any(mask_idx)) {
      next
    }

    x <- x[!mask_idx, , drop = FALSE]
  }

  x
}

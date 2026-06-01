#' Return default data masking configuration
#'
#' `get_data_masking_config()` returns a template configuration that can be
#' extended per scenario. Rules can be defined globally (`all`) and/or per source.
#'
#' Supported source keys include granular pre-blend sources:
#' - `entsoe_power_daily`, `ember_power_monthly`, `ember_power_yearly`
#' - `entsog_flow_raw`, `agsi_storage_daily`, `eurostat_gas_monthly_for_correction`
#' - `eurostat_oil_monthly`, `eurostat_oil_yearly`
#' - `eurostat_solid_monthly`, `eurostat_solid_yearly`
#' - `eurostat_gas_monthly`, `eurostat_gas_yearly`
#' - `eurostat_indprod`, `weather`
#'
#' Compatibility source keys are also supported:
#' - `gas_demand`
#' - `power_generation`
#' - `eurostat_cons`
#'
#' Each rule is a named list. Reserved rule fields:
#' - `date_from` / `available_from`
#' - `date_to` / `available_to`
#' - `value_cols`
#' - `name`
#' - `comment`
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
    gas_demand = list(),
    power_generation = list(),
    eurostat_cons = list(),
    eurostat_indprod = list(),
    weather = list()
  )
}


.data_source_mask_specs <- function() {
  list(
    entsog_flow_raw = list(
      aliases = c("entsog_flow_raw", "entsog_raw", "entsog"),
      date_col = "date",
      value_cols = c("value_m3")
    ),
    agsi_storage_daily = list(
      aliases = c("agsi_storage_daily", "agsi_storage", "agsi"),
      date_col = "date",
      value_cols = c("value_m3")
    ),
    eurostat_gas_monthly_for_correction = list(
      aliases = c("eurostat_gas_monthly_for_correction", "eurostat_gas_for_correction"),
      date_col = "date",
      value_cols = c("value_m3")
    ),
    entsoe_power_daily = list(
      aliases = c("entsoe_power_daily", "entsoe_power", "entsoe_daily"),
      date_col = "date",
      value_cols = c("value_mw", "value_mwh")
    ),
    ember_power_monthly = list(
      aliases = c("ember_power_monthly", "ember_monthly"),
      date_col = "date",
      value_cols = c("value_mw", "value_mwh")
    ),
    ember_power_yearly = list(
      aliases = c("ember_power_yearly", "ember_yearly"),
      date_col = "date",
      value_cols = c("value_mw", "value_mwh")
    ),
    eurostat_oil_monthly = list(
      aliases = c("eurostat_oil_monthly"),
      date_col = "time",
      value_cols = c("values")
    ),
    eurostat_oil_yearly = list(
      aliases = c("eurostat_oil_yearly"),
      date_col = "time",
      value_cols = c("values")
    ),
    eurostat_solid_monthly = list(
      aliases = c("eurostat_solid_monthly", "eurostat_coal_monthly"),
      date_col = "time",
      value_cols = c("values")
    ),
    eurostat_solid_yearly = list(
      aliases = c("eurostat_solid_yearly", "eurostat_coal_yearly"),
      date_col = "time",
      value_cols = c("values")
    ),
    eurostat_gas_monthly = list(
      aliases = c("eurostat_gas_monthly"),
      date_col = "time",
      value_cols = c("values")
    ),
    eurostat_gas_yearly = list(
      aliases = c("eurostat_gas_yearly"),
      date_col = "time",
      value_cols = c("values")
    ),
    gas_demand = list(
      aliases = c("gas_demand", "gas", "entsog_gas_demand"),
      date_col = "date",
      value_cols = c("value")
    ),
    power_generation = list(
      aliases = c("power_generation", "power", "entsoe_ember_power"),
      date_col = "date",
      value_cols = c("value_mw", "value_mwh")
    ),
    eurostat_cons = list(
      aliases = c("eurostat_cons", "eurostat_consumption", "consumption"),
      date_col = "time",
      value_cols = c("values")
    ),
    eurostat_indprod = list(
      aliases = c("eurostat_indprod", "industrial_production", "indprod"),
      date_col = "time",
      value_cols = c("values")
    ),
    weather = list(
      aliases = c("weather", "weather_hdd_cdd"),
      date_col = "date",
      value_cols = c("value")
    )
  )
}


.normalize_source_key <- function(source_name) {
  if (is.null(source_name) || length(source_name) != 1) {
    stop("source_name must be a single character value")
  }

  source_name <- as.character(source_name)
  specs <- .data_source_mask_specs()

  for (key in names(specs)) {
    if (source_name %in% specs[[key]]$aliases) {
      return(key)
    }
  }

  source_name
}


.reserved_mask_rule_fields <- function() {
  c(
    "date_from", "date_to",
    "available_from", "available_to",
    "value_cols", "name", "comment"
  )
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


.resolve_value_cols <- function(x, rule, default_value_cols) {
  rule_value_cols <- rule$value_cols
  if (is.null(rule_value_cols)) {
    return(intersect(default_value_cols, names(x)))
  }

  intersect(as.character(unlist(rule_value_cols)), names(x))
}


.build_mask_index <- function(x, date_col, rule) {
  idx <- rep(TRUE, nrow(x))

  if (!date_col %in% names(x)) {
    return(rep(FALSE, nrow(x)))
  }

  date_from <- if (!is.null(rule$available_from)) rule$available_from else rule$date_from
  date_to <- if (!is.null(rule$available_to)) rule$available_to else rule$date_to

  date_values <- as.Date(x[[date_col]])

  if (!is.null(date_from)) {
    idx <- idx & date_values >= as.Date(date_from)
  }
  if (!is.null(date_to)) {
    idx <- idx & date_values <= as.Date(date_to)
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
#' Applies source-specific masking rules by replacing selected value columns with
#' `NA`. This allows scenario analysis with constrained historical availability.
#'
#' @param x Data frame to mask.
#' @param source_name Source key (for example `"gas_demand"`). Aliases are supported.
#' @param data_masking Named list of masking rules.
#' @return Data frame with masked values.
#' @export
apply_source_data_mask <- function(x, source_name, data_masking = NULL) {
  if (is.null(data_masking) || is.null(x) || nrow(x) == 0) {
    return(x)
  }

  source_key <- .normalize_source_key(source_name)
  specs <- .data_source_mask_specs()[[source_key]]

  if (is.null(specs)) {
    warning(glue::glue("Unknown source '{source_name}' for masking. Returning data unchanged."), call. = FALSE)
    return(x)
  }

  source_rules <- .rule_list_from_config(data_masking[[source_key]])
  global_rules <- .rule_list_from_config(data_masking$all)
  rules <- c(global_rules, source_rules)

  if (length(rules) == 0) {
    return(x)
  }

  date_col <- specs$date_col
  if (!date_col %in% names(x)) {
    warning(glue::glue("Date column '{date_col}' missing for source '{source_key}'. Returning unchanged."), call. = FALSE)
    return(x)
  }

  for (rule in rules) {
    mask_idx <- .build_mask_index(x, date_col = date_col, rule = rule)
    value_cols <- .resolve_value_cols(x, rule = rule, default_value_cols = specs$value_cols)

    if (!any(mask_idx) || length(value_cols) == 0) {
      next
    }

    for (value_col in value_cols) {
      x[[value_col]][mask_idx] <- NA_real_
    }
  }

  x
}
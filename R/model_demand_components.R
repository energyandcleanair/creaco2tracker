#' Split Energy Demand into Heating, Cooling, and Other Components
#'
#' Builds daily time series for gas demand and electricity demand, split into
#' heating, cooling, and other components using HDD/CDD regressions.
#'
#' @param iso2s Character vector of ISO2 country codes. Default is "EU".
#' @param date_from Start date for analysis. Default is "2015-01-01".
#' @param date_to End date for analysis. Default is today.
#' @param use_cache Whether to use cached data. Default is TRUE.
#' @param correct_gas_to_eurostat Whether to correct gas demand to Eurostat. Default is TRUE.
#' @param model_type Model type: "lm" for linear regression (with optional year interaction),
#'   "gam" for generalized additive model with smooth splines (captures non-linear
#'   time-varying temperature sensitivity). Default is "lm".
#' @param include_time_interaction Logical, include a linear year interaction
#'   with HDD/CDD to capture changing temperature sensitivity over time.
#'   Only used when model_type = "lm". Ignored for "gam" (which uses smooth splines).
#' @param diagnostics_folder Folder path for diagnostics outputs. Default is
#'   "diagnostics/demand_components".
#' @param data_masking Optional named list of masking rules. See
#'   `get_data_masking_config()`.
#'
#' @return Tibble with columns:
#'   - iso2: Country code
#'   - date: Date
#'   - fuel: "fossil_gas" or "electricity"
#'   - component: For electricity: "heating", "cooling", "others".
#'       For gas: "heating", "electricity", "others" (electricity = gas-to-power)
#'   - value: Actual demand value for the component
#'   - value_weather_corrected: Demand at climatological-mean HDD/CDD
#'       (day-of-year average). Equals value for non-weather components (others, electricity).
#'   - unit: Unit of the demand values
#'   - frequency: Data frequency (daily when available)
#'   - data_source: Source identifier
#'
#' @details
#' Electricity demand is proxied by ENTSO-E total generation (MW). Gas demand
#' comes from CREA's gas demand series (m3). Weather components are derived
#' via counterfactual predictions: others = predict(HDD=0), heating = observed - others.
#' Weather-corrected values replace actual HDD/CDD with day-of-year climatological means.
#'
#' When model_type = "gam", uses mgcv::gam() with smooth splines to capture
#' non-linear time trends and time-varying temperature sensitivity.
#'
#' @export
get_demand_components <- function(
  iso2s = "EU",
  date_from = "2015-01-01",
  date_to = Sys.Date(),
  use_cache = TRUE,
  correct_gas_to_eurostat = TRUE,
  model_type = c("gam", "lm"),
  include_time_interaction = TRUE,
  diagnostics_folder = "diagnostics/demand_components",
  data_masking = NULL
) {
  model_type <- match.arg(model_type)
  iso2s <- unique(iso2s)
  date_from <- as.Date(date_from)
  date_to <- as.Date(date_to)
  gas_diagnostics_folder <- if (is.null(diagnostics_folder)) {
    NULL
  } else {
    file.path(diagnostics_folder, "gas_demand")
  }
  power_diagnostics_folder <- if (is.null(diagnostics_folder)) {
    NULL
  } else {
    file.path(diagnostics_folder, "power_generation")
  }

  gas_demand <- get_gas_demand(
    iso2 = iso2s,
    use_cache = use_cache,
    correct_to_eurostat = correct_gas_to_eurostat,
    diagnostics_folder = gas_diagnostics_folder,
    data_masking = data_masking
  ) %>%
    filter(date >= date_from, date <= date_to) %>%
    mutate(date = as.Date(date)) %>%
    add_missing_cols(c("data_source")) %>%
    mutate(data_source = coalesce(data_source, "crea"))


  gas_demand <- gas_demand %>%
    filter(iso2 %in% iso2s)

  pwr_generation <- get_power_generation(
    date_from = date_from,
    iso2s = iso2s,
    use_cache = use_cache,
    diagnostics_folder = power_diagnostics_folder,
    data_masking = data_masking
  )

  elec_demand <- pwr_generation %>%
    filter(
      source == "Total",
      date >= date_from,
      date <= date_to
    ) %>%
    mutate(
      date = as.Date(date),
      fuel = "electricity",
      unit = "MW",
      frequency = "daily",
      data_source = "entsoe"
    ) %>%
    select(iso2, date, value = value_mw, fuel, unit, frequency, data_source)

  elec_demand <- elec_demand %>%
    filter(iso2 %in% iso2s)

  # Gas-fired electricity generation (used as predictor in gas demand regression)
  gas_elec <- pwr_generation %>%
    filter(
      source == "Fossil Gas",
      date >= date_from,
      date <= date_to
    ) %>%
    mutate(date = as.Date(date)) %>%
    select(iso2, date, value = value_mw) %>%
    filter(iso2 %in% iso2s)

  weather_raw <- weather_data_access_get_filled(
    variable = "HDD,CDD",
    region_id = iso2s,
    date_from = date_from,
    date_to = date_to,
    use_cache = use_cache,
    data_masking = data_masking
  )
  weather <- weather_raw %>%
    mutate(
      date = as.Date(date),
      region_code = coalesce(region_iso2, region_id)
    ) %>%
    filter(region_code %in% iso2s) %>%
    select(region_code, date, variable, value) %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) %>%
    add_missing_cols(c("hdd", "cdd"))

  gas_components <- .split_demand_gas(
    demand = gas_demand,
    weather = weather,
    gas_elec = gas_elec,
    iso2s = iso2s,
    date_from = date_from,
    date_to = date_to,
    model_type = model_type,
    include_time_interaction = include_time_interaction,
    diagnostics_folder = diagnostics_folder
  )

  elec_components <- .split_demand_elec(
    demand = elec_demand,
    weather = weather,
    iso2s = iso2s,
    date_from = date_from,
    date_to = date_to,
    model_type = model_type,
    include_time_interaction = include_time_interaction,
    diagnostics_folder = diagnostics_folder
  )

  components <- bind_rows(gas_components, elec_components)

  if (!is.null(diagnostics_folder)) {
    create_dir(diagnostics_folder)
  }

  return(components)
}


#' Split electricity demand into heating, cooling, and other components
#' Uses HDD and CDD for regression
#' @keywords internal
.split_demand_elec <- function(
  demand,
  weather,
  iso2s,
  date_from,
  date_to,
  model_type = "gam",
  include_time_interaction = FALSE,
  diagnostics_folder = NULL
) {
  date_seq <- seq.Date(date_from, date_to, by = "day")

  results <- pblapply(iso2s, function(iso2) {
    demand_iso <- demand %>%
      filter(iso2 == !!iso2)

    if (nrow(demand_iso) == 0) {
      return(NULL)
    }

    base <- tibble(date = date_seq) %>%
      left_join(
        demand_iso %>%
          select(date, value, unit, frequency, data_source, fuel),
        by = "date"
      ) %>%
      mutate(
        iso2 = iso2,
        fuel = first(na.omit(demand_iso$fuel)),
        unit = first(na.omit(demand_iso$unit)),
        frequency = first(na.omit(demand_iso$frequency)),
        data_source = first(na.omit(demand_iso$data_source))
      )

    weather_iso <- weather %>%
      filter(region_code == !!iso2)

    model_data <- demand_iso %>%
      left_join(weather_iso, by = "date") %>%
      filter(!is.na(value), !is.na(hdd), !is.na(cdd)) %>%
      mutate(
        wday = lubridate::wday(date),
        yday = lubridate::yday(date),
        date_num = as.numeric(date),
        year_c = lubridate::year(date)
      )

    model_data <- model_data %>%
      mutate(
        year_c = if (include_time_interaction) {
          year_c - mean(year_c, na.rm = TRUE)
        } else {
          0
        }
      )

    # Compute day-of-year mean HDD/CDD for weather correction
    yday_means <- model_data %>%
      group_by(yday) %>%
      summarise(
        hdd_mean = mean(hdd, na.rm = TRUE),
        cdd_mean = mean(cdd, na.rm = TRUE),
        .groups = "drop"
      )

    model_data <- model_data %>%
      left_join(yday_means, by = "yday")

    # Fit model
    if (model_type == "gam") {
      model <- mgcv::gam(
        value ~ s(date_num, k = 10) +
          s(date_num, by = hdd, k = 10) +
          s(date_num, by = cdd, k = 10) +
          as.factor(wday) + hdd:as.factor(wday) + cdd:as.factor(wday),
        data = model_data
      )
    } else {
      formula_terms <- c("hdd", "cdd", "as.factor(wday)")
      if (include_time_interaction) {
        formula_terms <- c(formula_terms, "hdd:year_c", "cdd:year_c")
      }
      model <- lm(as.formula(paste("value ~", paste(formula_terms, collapse = " + "))),
        data = model_data
      )
    }

    # Decomposition using direct subtraction (Lauri's approach)
    # others = predict(HDD=0, CDD=0) — baseline
    # heating = predict(CDD=0) - predict(HDD=0, CDD=0) — isolate HDD effect
    # cooling = observed - predict(CDD=0) — residuals go to cooling
    model_data <- model_data %>%
      mutate(
        pred_no_hdd_cdd = predict(model, mutate(model_data, hdd = 0, cdd = 0)),
        pred_no_cdd = predict(model, mutate(model_data, cdd = 0)),
        others = pred_no_hdd_cdd,
        heating = pred_no_cdd - pred_no_hdd_cdd,
        cooling = value - pred_no_cdd
      )

    # Weather-corrected values: replace HDD/CDD with day-of-year means
    model_data <- model_data %>%
      mutate(
        pred_mean_hdd_no_cdd = predict(model, mutate(model_data, hdd = hdd_mean, cdd = 0)),
        pred_mean_hdd_cdd = predict(model, mutate(model_data, hdd = hdd_mean, cdd = cdd_mean)),
        others_wc = others,
        heating_wc = pred_mean_hdd_no_cdd - pred_no_hdd_cdd,
        cooling_wc = pred_mean_hdd_cdd - pred_mean_hdd_no_cdd
      ) %>%
      select(date, heating, cooling, others, heating_wc, cooling_wc, others_wc)

    if (any(model_data$others < 0, na.rm = TRUE)) {
      warning(glue("Negative 'others' for {iso2}: {sum(model_data$others < 0, na.rm = TRUE)} days"))
    }

    components <- base %>%
      left_join(model_data, by = "date") %>%
      mutate(
        heating = ifelse(is.na(value), NA_real_, heating),
        cooling = ifelse(is.na(value), NA_real_, cooling),
        others = ifelse(is.na(value), NA_real_, others),
        heating_wc = ifelse(is.na(value), NA_real_, heating_wc),
        cooling_wc = ifelse(is.na(value), NA_real_, cooling_wc),
        others_wc = ifelse(is.na(value), NA_real_, others_wc)
      )

    diagnose_demand_components(
      components,
      iso2,
      model,
      model_data,
      model_type,
      include_time_interaction,
      diagnostics_folder
    )

    # Pivot to long format with both value and value_weather_corrected
    comp_value <- components %>%
      select(iso2, date, fuel, unit, frequency, data_source, heating, cooling, others) %>%
      tidyr::pivot_longer(
        cols = c(heating, cooling, others),
        names_to = "component",
        values_to = "value"
      )

    comp_wc <- components %>%
      select(date, heating_wc, cooling_wc, others_wc) %>%
      tidyr::pivot_longer(
        cols = c(heating_wc, cooling_wc, others_wc),
        names_to = "component",
        values_to = "value_weather_corrected"
      ) %>%
      mutate(component = gsub("_wc$", "", component))

    comp_value %>%
      left_join(comp_wc, by = c("date", "component")) %>%
      select(
        iso2, date, fuel, component, value, value_weather_corrected,
        unit, frequency, data_source
      )
  })

  bind_rows(results)
}


#' Split gas demand into heating and other components, with electricity sector separation
#' Uses HDD only (no CDD) for regression, and gas_elec to attribute electricity sector consumption
#' @keywords internal
.split_demand_gas <- function(
  demand,
  weather,
  gas_elec,
  iso2s,
  date_from,
  date_to,
  model_type = "gam",
  include_time_interaction = FALSE,
  diagnostics_folder = NULL
) {
  date_seq <- seq.Date(date_from, date_to, by = "day")

  results <- pblapply(iso2s, function(iso2) {
    demand_iso <- demand %>%
      filter(iso2 == !!iso2)

    if (nrow(demand_iso) == 0) {
      return(NULL)
    }

    gas_elec_iso <- gas_elec %>%
      filter(iso2 == !!iso2) %>%
      select(date, gas_elec_value = value)

    base <- tibble(date = date_seq) %>%
      left_join(
        demand_iso %>%
          select(date, value, unit, frequency, data_source, fuel),
        by = "date"
      ) %>%
      left_join(gas_elec_iso, by = "date") %>%
      mutate(
        iso2 = iso2,
        fuel = first(na.omit(demand_iso$fuel)),
        unit = first(na.omit(demand_iso$unit)),
        frequency = first(na.omit(demand_iso$frequency)),
        data_source = first(na.omit(demand_iso$data_source))
      )

    weather_iso <- weather %>%
      filter(region_code == !!iso2)

    model_data <- demand_iso %>%
      left_join(weather_iso, by = "date") %>%
      left_join(gas_elec_iso, by = "date") %>%
      filter(!is.na(value), !is.na(hdd)) %>%
      mutate(
        wday = lubridate::wday(date),
        yday = lubridate::yday(date),
        date_num = as.numeric(date),
        year_c = lubridate::year(date)
      )

    model_data <- model_data %>%
      mutate(
        year_c = if (include_time_interaction) {
          year_c - mean(year_c, na.rm = TRUE)
        } else {
          0
        }
      )

    # Compute day-of-year mean HDD for weather correction
    yday_means <- model_data %>%
      group_by(yday) %>%
      summarise(
        hdd_mean = mean(hdd, na.rm = TRUE),
        .groups = "drop"
      )

    model_data <- model_data %>%
      left_join(yday_means, by = "yday")

    # Fit model
    if (model_type == "gam") {
      model <- mgcv::gam(
        value ~ s(date_num, k = 10) +
          s(date_num, by = hdd, k = 10) +
          as.factor(wday) + hdd:as.factor(wday) + gas_elec_value,
        data = model_data
      )
    } else {
      formula_terms <- c("hdd", "gas_elec_value", "as.factor(wday)")
      if (include_time_interaction) {
        formula_terms <- c(formula_terms, "hdd:year_c")
      }
      model <- lm(as.formula(paste("value ~", paste(formula_terms, collapse = " + "))),
        data = model_data
      )
    }

    # Decomposition using direct subtraction (Lauri's approach)
    # others = predict(HDD=0, gas_elec=0) — pure baseline
    # elec = predict(HDD=0, actual_gas_elec) - predict(HDD=0, gas_elec=0) — gas-to-power effect
    # heating = observed - predict(HDD=0) — residuals go to heating
    model_data <- model_data %>%
      mutate(
        pred_no_hdd_no_elec = predict(model, mutate(model_data, hdd = 0, gas_elec_value = 0)),
        pred_no_hdd = predict(model, mutate(model_data, hdd = 0)),
        others = pred_no_hdd_no_elec,
        elec = pred_no_hdd - pred_no_hdd_no_elec,
        heating = value - pred_no_hdd
      )

    # Weather-corrected values: replace HDD with day-of-year mean
    model_data <- model_data %>%
      mutate(
        pred_mean_hdd = predict(model, mutate(model_data, hdd = hdd_mean)),
        others_wc = others,
        elec_wc = elec,
        heating_wc = pred_mean_hdd - pred_no_hdd
      ) %>%
      select(date, heating, elec, others, heating_wc, elec_wc, others_wc)

    if (any(model_data$heating < 0, na.rm = TRUE) || any(model_data$others < 0, na.rm = TRUE)) {
      warning(
        glue(
          "Negative values for {iso2}: heating={sum(
            model_data$heating < 0, na.rm =
        TRUE)}, others={sum(model_data$others < 0, na.rm = TRUE)}"
        )
      )
    }

    components <- base %>%
      select(-gas_elec_value) %>%
      left_join(model_data, by = "date") %>%
      mutate(
        heating = ifelse(is.na(value), NA_real_, heating),
        elec = ifelse(is.na(value), NA_real_, elec),
        others = ifelse(is.na(value), NA_real_, others),
        heating_wc = ifelse(is.na(value), NA_real_, heating_wc),
        elec_wc = ifelse(is.na(value), NA_real_, elec_wc),
        others_wc = ifelse(is.na(value), NA_real_, others_wc)
      )

    diagnose_demand_components(
      components,
      iso2,
      model,
      model_data,
      model_type,
      include_time_interaction,
      diagnostics_folder
    )

    # Pivot to long format with both value and value_weather_corrected
    comp_value <- components %>%
      select(iso2, date, fuel, unit, frequency, data_source, heating, elec, others) %>%
      tidyr::pivot_longer(
        cols = c(heating, elec, others),
        names_to = "component",
        values_to = "value"
      ) %>%
      mutate(component = ifelse(component == "elec", "electricity", component))

    comp_wc <- components %>%
      select(date, heating_wc, elec_wc, others_wc) %>%
      tidyr::pivot_longer(
        cols = c(heating_wc, elec_wc, others_wc),
        names_to = "component",
        values_to = "value_weather_corrected"
      ) %>%
      mutate(
        component = gsub("_wc$", "", component),
        component = ifelse(component == "elec", "electricity", component)
      )

    comp_value %>%
      left_join(comp_wc, by = c("date", "component")) %>%
      select(
        iso2, date, fuel, component, value, value_weather_corrected,
        unit, frequency, data_source
      )
  })

  bind_rows(results)
}

#' Split Energy Demand into Heating, Cooling, and Other Components
#'
#' Builds daily time series for gas demand and electricity demand, split into
#' heating, cooling, and other components using HDD/CDD regressions.
#'
#' @param iso2s Character vector of ISO2 country codes. Default is "EU".
#' @param date_from Start date for analysis. Default is "2015-01-01".
#' @param date_to End date for analysis. Default is today.
#' @param use_cache Whether to use cached data. Default is TRUE.
#' @param include_time_interaction Logical, include a linear year interaction
#'   with HDD/CDD to capture changing temperature sensitivity over time.
#' @param diagnostics_folder Folder path for diagnostics outputs. Default is
#'   "diagnostics/demand_split".
#'
#' @return Tibble with columns:
#'   - iso2: Country code
#'   - date: Date
#'   - fuel: "fossil_gas" or "electricity"
#'   - component: "heating", "cooling", or "others"
#'   - value: Demand value for the component
#'   - unit: Unit of the demand values
#'   - frequency: Data frequency (daily when available)
#'   - data_source: Source identifier
#'
#' @details
#' Electricity demand is proxied by ENTSO-E total generation (MW). Gas demand
#' comes from CREA's gas demand series (m3). Weather components are derived
#' from linear models using HDD (and CDD for electricity), with components
#' computed via counterfactual predictions (HDD/CDD set to zero) so the
#' decomposition tracks model-based weather effects.
#'
#' @export
get_demand_components <- function(iso2s = "EU",
                                  date_from = "2015-01-01",
                                  date_to = Sys.Date(),
                                  use_cache = TRUE,
                                  include_time_interaction = TRUE,
                                  diagnostics_folder = "diagnostics/demand_components") {

  iso2s <- unique(iso2s)
  date_from <- as.Date(date_from)
  date_to <- as.Date(date_to)

  gas_demand <- download_gas_demand(
    iso2 = iso2s,
    use_cache = use_cache,
    date_from = date_from
  ) %>%
    filter(date >= date_from, date <= date_to) %>%
    mutate(date = as.Date(date)) %>%
    add_missing_cols(c("data_source")) %>%
    mutate(data_source = coalesce(data_source, "crea"))


  gas_demand <- gas_demand %>%
    filter(iso2 %in% iso2s)

  pwr_generation <- entsoe.get_power_generation(
    date_from = date_from,
    iso2s = iso2s,
    use_cache = use_cache
  )

  elec_demand <- pwr_generation %>%
    filter(source == "Total",
           date >= date_from,
           date <= date_to) %>%
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

  weather_raw <- get_weather(
    variable = "HDD,CDD",
    region_id = iso2s,
    date_from = date_from,
    date_to = date_to,
    use_cache = use_cache
  )
  weather <- fill_weather(weather_raw) %>%
    mutate(
      date = as.Date(date),
      region_code = coalesce(region_iso2, region_id)
    ) %>%
    filter(region_code %in% iso2s) %>%
    select(region_code, date, variable, value) %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) %>%
    add_missing_cols(c("hdd", "cdd"))

  gas_components <- .split_demand_components(
    demand = gas_demand,
    weather = weather,
    iso2s = iso2s,
    date_from = date_from,
    date_to = date_to,
    include_cdd = FALSE,
    include_time_interaction = include_time_interaction,
    diagnostics_folder = diagnostics_folder
  )

  elec_components <- .split_demand_components(
    demand = elec_demand,
    weather = weather,
    iso2s = iso2s,
    date_from = date_from,
    date_to = date_to,
    include_cdd = TRUE,
    include_time_interaction = include_time_interaction,
    diagnostics_folder = diagnostics_folder
  )

  components <- bind_rows(gas_components, elec_components)

  if (!is.null(diagnostics_folder)) {
    create_dir(diagnostics_folder)
  }

  return(components)
}


.split_demand_components <- function(demand,
                                     weather,
                                     iso2s,
                                     date_from,
                                     date_to,
                                     include_cdd = TRUE,
                                     include_time_interaction = FALSE,
                                     diagnostics_folder = NULL) {


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
      mutate(iso2 = iso2, fuel = first(na.omit(demand_iso$fuel)))

    weather_iso <- weather %>%
      filter(region_code == !!iso2)

    model_data <- demand_iso %>%
      left_join(weather_iso, by = "date") %>%
      filter(!is.na(value), !is.na(hdd)) %>%
      mutate(
        cdd = if (include_cdd) cdd else 0,
        wday = lubridate::wday(date),
        year_c = lubridate::year(date)
      )

    if (include_cdd) {
      model_data <- model_data %>%
        filter(!is.na(cdd))
    }


    model_data <- model_data %>%
      mutate(
        year_c = if (include_time_interaction) {
          year_c - mean(year_c, na.rm = TRUE)
        } else {
          0
        }
      )

    formula_terms <- c("hdd", if (include_cdd) "cdd", "as.factor(wday)")
    if (include_time_interaction) {
      formula_terms <- c(
        formula_terms,
        "hdd:year_c",
        if (include_cdd) "cdd:year_c"
      )
    }

    model <- lm(as.formula(paste("value ~", paste(formula_terms, collapse = " + "))),
                data = model_data)

    summary(model)

    model_data <- model_data %>%
      mutate(
        pred_actual = predict(model, model_data),
        pred_no_hdd = predict(model, mutate(model_data, hdd = 0)),
        pred_no_cdd = if (include_cdd) {
          predict(model, mutate(model_data, cdd = 0))
        } else {
          pred_actual
        },
        pred_no_hdd_cdd = predict(model, mutate(model_data, hdd = 0, cdd = 0)),
        heating_raw = pred_actual - pred_no_hdd,
        cooling_raw = if (include_cdd) pred_actual - pred_no_cdd else 0,
        others_raw = pred_no_hdd_cdd,
        scale = case_when(
          !is.na(pred_actual) & pred_actual > 0 ~ value / pred_actual,
          !is.na(pred_actual) & pred_actual == 0 & value == 0 ~ 0,
          TRUE ~ NA_real_
        ),
        heating = heating_raw * scale,
        cooling = cooling_raw * scale,
        others = others_raw * scale
      ) %>%
      select(date, heating, cooling, others)

    if (any(model_data$heating < 0) || any(model_data$cooling < 0) || any(model_data$others < 0)) {
      warning(glue("Negative values for {iso2}: heating={sum(model_data$heating < 0)}, cooling={sum(model_data$cooling < 0)}, others={sum(model_data$others < 0)}"))
    }

    components <- base %>%
      left_join(model_data, by = "date") %>%
      mutate(
        heating = ifelse(is.na(value), NA_real_, heating),
        cooling = ifelse(is.na(value), NA_real_, cooling),
        others = ifelse(is.na(value), NA_real_, others)
      )

    diagnose_demand_components(
      components,
      iso2,
      model,
      model_data,
      include_cdd,
      include_time_interaction,
      diagnostics_folder
    )

    components %>%
      select(-value) %>%
      tidyr::pivot_longer(
        cols = c(heating, cooling, others),
        names_to = "component",
        values_to = "value"
      ) %>%
      select(iso2, date, fuel, component, value, unit, frequency, data_source)
  })

  bind_rows(results)
}

diagnose_demand_components <- function(components,
                                       iso2,
                                       model,
                                       model_data,
                                       include_cdd,
                                       include_time_interaction,
                                       diagnostics_folder) {

  if (!is.null(diagnostics_folder)) {
      fuel_label <- first(na.omit(components$fuel))
      diag_folder <- file.path(diagnostics_folder, fuel_label)
      create_dir(diag_folder)

      summary_path <- file.path(diag_folder, paste0("model_summary_", tolower(iso2), ".txt"))
      if (is.null(model)) {
        writeLines("Insufficient data for model.", summary_path)
      } else {
        writeLines(capture.output(summary(model)), summary_path)
      }

      plot_data <- components %>%
        mutate(
          total_observed = value,
          total = heating + cooling + others
        ) %>%
        filter(!is.na(total)) %>%
        select(-value)

      if (nrow(plot_data) > 0) {
        plot_long <- plot_data %>%
          pivot_longer(cols = c(heating, cooling, others, total_observed),
                       names_to = "series",
                       values_to = "value") %>%
          mutate(series = recode(series, total_observed = "total"))

        monthly_data <- plot_long %>%
          mutate(month = lubridate::floor_date(date, "month")) %>%
          group_by(month, series) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

        yearly_data <- plot_long %>%
          mutate(year = lubridate::year(date)) %>%
          group_by(year, series) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

        color_map <- c(
          total = "black",
          heating = "#D55E00",
          cooling = "#0072B2",
          others = "#999999"
        )

        plt_monthly <- monthly_data %>%
          ggplot(aes(month, value, color = series)) +
          geom_line(linewidth = 0.7) +
          scale_color_manual(values = color_map) +
          labs(
            title = paste0(str_to_title(fuel_label), " demand components (monthly): ", iso2),
            subtitle = paste0(
              "Heating/Cooling from HDD",
              if (include_cdd) " & CDD" else "",
              if (include_time_interaction) " + year interaction" else ""
            ),
            y = paste0("Demand (", first(na.omit(components$unit)), ")"),
            x = NULL,
            color = NULL
          ) +
          rcrea::theme_crea_new()

        quicksave(
          file.path(diag_folder, paste0("demand_components_monthly_", tolower(iso2), ".png")),
          plot = plt_monthly,
          width = 10,
          height = 6,
          preview = FALSE
        )

        plt_yearly <- yearly_data %>%
          ggplot(aes(year, value, color = series)) +
          geom_line(linewidth = 0.7) +
          geom_point(size = 1.5) +
          scale_color_manual(values = color_map) +
          labs(
            title = paste0(str_to_title(fuel_label), " demand components (yearly): ", iso2),
            subtitle = paste0(
              "Heating/Cooling from HDD",
              if (include_cdd) " & CDD" else "",
              if (include_time_interaction) " + year interaction" else ""
            ),
            y = paste0("Demand (", first(na.omit(components$unit)), ")"),
            x = NULL,
            color = NULL
          ) +
          rcrea::theme_crea_new()

        quicksave(
          file.path(diag_folder, paste0("demand_components_yearly_", tolower(iso2), ".png")),
          plot = plt_yearly,
          width = 10,
          height = 6,
          preview = FALSE
        )
      }
    }
}

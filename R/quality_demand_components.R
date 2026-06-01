#' Diagnose demand component decomposition
#'
#' Saves model summaries and generates diagnostic plots for demand components.
#' @keywords internal
diagnose_demand_components <- function(components,
                                       iso2,
                                       model,
                                       model_data,
                                       model_type,
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

      # Detect which secondary component is present (elec or cooling)
      has_elec <- "elec" %in% names(components)
      has_cooling <- "cooling" %in% names(components)
      secondary_col <- if (has_elec) "elec" else if (has_cooling) "cooling" else NULL

      if (is.null(secondary_col)) {
        # Only heating and others
        plot_data <- components %>%
          mutate(
            total_observed = value,
            total = heating + others
          ) %>%
          filter(!is.na(total)) %>%
          select(-value)
        pivot_cols <- c("heating", "others", "total_observed")
        pivot_cols_wc <- c("heating_wc", "others_wc")
      } else {
        plot_data <- components %>%
          mutate(
            total_observed = value,
            total = heating + .data[[secondary_col]] + others
          ) %>%
          filter(!is.na(total)) %>%
          select(-value)
        pivot_cols <- c("heating", secondary_col, "others", "total_observed")
        pivot_cols_wc <- c("heating_wc", paste0(secondary_col, "_wc"), "others_wc")
      }

      if (nrow(plot_data) > 0) {
        # Include weather-corrected series
        all_pivot_cols <- c(pivot_cols, intersect(pivot_cols_wc, names(plot_data)))

        plot_long <- plot_data %>%
          pivot_longer(cols = all_of(all_pivot_cols),
                       names_to = "series",
                       values_to = "value") %>%
          mutate(
            is_wc = grepl("_wc$", series),
            series = gsub("_wc$", "", series),
            series = recode(series, total_observed = "total")
          )

        monthly_data <- plot_long %>%
          mutate(month = lubridate::floor_date(date, "month")) %>%
          group_by(month, series, is_wc) %>%
          summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

        yearly_data <- plot_long %>%
          mutate(year = lubridate::year(date)) %>%
          group_by(year, series, is_wc) %>%
          summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

        color_map <- c(
          total = "black",
          heating = "#D55E00",
          cooling = "#0072B2",
          elec = "#009E73",
          others = "#999999"
        )

        model_label <- if (model_type == "gam") "GAM" else "LM"
        subtitle_text <- paste0(
          model_label, ": Heating from HDD",
          if (has_cooling) ", Cooling from CDD" else "",
          if (has_elec) ", Elec from gas generation" else "",
          if (model_type == "lm" && include_time_interaction) " + year interaction" else ""
        )

        plt_monthly <- monthly_data %>%
          ggplot(aes(month, value, color = series, linetype = ifelse(is_wc, "weather corrected", "actual"))) +
          geom_line(linewidth = 0.7) +
          scale_color_manual(values = color_map) +
          scale_linetype_manual(values = c("actual" = "solid", "weather corrected" = "dashed")) +
          labs(
            title = paste0(str_to_title(fuel_label), " demand components (monthly): ", iso2),
            subtitle = subtitle_text,
            y = paste0("Demand (", first(na.omit(components$unit)), ")"),
            x = NULL,
            color = NULL,
            linetype = NULL
          ) +
          rcrea::theme_crea_new() +
          rcrea::scale_y_zero()

        quicksave(
          file.path(diag_folder, paste0("demand_components_monthly_", tolower(iso2), ".png")),
          plot = plt_monthly,
          width = 10,
          height = 6,
          preview = FALSE
        )

        # Calculate YoY percentage change
        yearly_data <- yearly_data %>%
          group_by(series, is_wc) %>%
          arrange(year) %>%
          mutate(
            yoy_pct = (value / lag(value) - 1) * 100,
            yoy_label = ifelse(
              !is.na(yoy_pct),
              sprintf("%+.1f%%", yoy_pct),
              NA_character_
            )
          ) %>%
          ungroup()

        plt_yearly <- yearly_data %>%
          ggplot(aes(year, value, color = series, linetype = ifelse(is_wc, "weather corrected", "actual"))) +
          geom_line(linewidth = 0.7) +
          geom_point(size = 1.5) +
          ggrepel::geom_text_repel(
            aes(label = yoy_label),
            size = 2.5,
            show.legend = FALSE,
            direction = "y",
            nudge_y = 0.02 * max(yearly_data$value, na.rm = TRUE),
            segment.size = 0.2,
            segment.alpha = 0.5
          ) +
          scale_color_manual(values = color_map) +
          scale_linetype_manual(values = c("actual" = "solid", "weather corrected" = "dashed")) +
          labs(
            title = paste0(str_to_title(fuel_label), " demand components (yearly): ", iso2),
            subtitle = subtitle_text,
            y = paste0("Demand (", first(na.omit(components$unit)), ")"),
            x = NULL,
            color = NULL,
            linetype = NULL
          ) +
          rcrea::theme_crea_new() +
          rcrea::scale_y_zero()

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


#' Compare LM and GAM Demand Decomposition Models
#'
#' Runs get_demand_components() with both model_type="lm" and model_type="gam",
#' and produces side-by-side diagnostic plots.
#'
#' @inheritParams get_demand_components
#' @return Tibble with all components from both models, tagged with a model_type column.
#' @export
compare_demand_models <- function(iso2s = "EU",
                                  date_from = "2015-01-01",
                                  date_to = Sys.Date(),
                                  use_cache = TRUE,
                                  correct_gas_to_eurostat = TRUE,
                                  include_time_interaction = TRUE,
                                  diagnostics_folder = "diagnostics/demand_components/comparison") {

  lm_result <- get_demand_components(
    iso2s = iso2s,
    date_from = date_from,
    date_to = date_to,
    use_cache = use_cache,
    correct_gas_to_eurostat = correct_gas_to_eurostat,
    model_type = "lm",
    include_time_interaction = include_time_interaction,
    diagnostics_folder = file.path(diagnostics_folder, "lm")
  ) %>%
    mutate(model_type = "lm")

  gam_result <- get_demand_components(
    iso2s = iso2s,
    date_from = date_from,
    date_to = date_to,
    use_cache = use_cache,
    correct_gas_to_eurostat = correct_gas_to_eurostat,
    model_type = "gam",
    diagnostics_folder = file.path(diagnostics_folder, "gam")
  ) %>%
    mutate(model_type = "gam")

  combined <- bind_rows(lm_result, gam_result)

  # Comparison plots
  if (!is.null(diagnostics_folder)) {
    create_dir(diagnostics_folder)

    for (fuel_val in unique(combined$fuel)) {
      fuel_data <- combined %>%
        filter(fuel == fuel_val, !is.na(value))

      # Monthly comparison
      monthly <- fuel_data %>%
        mutate(month = lubridate::floor_date(date, "month")) %>%
        group_by(month, component, model_type) %>%
        summarise(value = sum(value, na.rm = TRUE),
                  value_weather_corrected = sum(value_weather_corrected, na.rm = TRUE),
                  .groups = "drop")

      plt <- monthly %>%
        ggplot(aes(month, value, color = component, linetype = model_type)) +
        geom_line(linewidth = 0.7) +
        labs(
          title = paste0(str_to_title(fuel_val), " demand: LM vs GAM (monthly)"),
          y = paste0("Demand (", first(na.omit(fuel_data$unit)), ")"),
          x = NULL, color = NULL, linetype = NULL
        ) +
        rcrea::theme_crea_new() +
        rcrea::scale_y_zero()

      quicksave(
        file.path(diagnostics_folder, paste0("comparison_monthly_", fuel_val, ".png")),
        plot = plt, width = 12, height = 6, preview = FALSE
      )

      # Weather-corrected comparison
      plt_wc <- monthly %>%
        ggplot(aes(month, value_weather_corrected, color = component, linetype = model_type)) +
        geom_line(linewidth = 0.7) +
        labs(
          title = paste0(str_to_title(fuel_val), " demand weather-corrected: LM vs GAM (monthly)"),
          y = paste0("Demand (", first(na.omit(fuel_data$unit)), ")"),
          x = NULL, color = NULL, linetype = NULL
        ) +
        rcrea::theme_crea_new() +
        rcrea::scale_y_zero()

      quicksave(
        file.path(diagnostics_folder, paste0("comparison_monthly_wc_", fuel_val, ".png")),
        plot = plt_wc, width = 12, height = 6, preview = FALSE
      )

      # Yearly comparison
      yearly <- fuel_data %>%
        mutate(year = lubridate::year(date)) %>%
        group_by(year, component, model_type) %>%
        summarise(value = sum(value, na.rm = TRUE),
                  value_weather_corrected = sum(value_weather_corrected, na.rm = TRUE),
                  .groups = "drop") %>%
        group_by(component, model_type) %>%
        arrange(year) %>%
        mutate(yoy_pct = (value / lag(value) - 1) * 100,
               yoy_label = ifelse(!is.na(yoy_pct), sprintf("%+.1f%%", yoy_pct), NA_character_)) %>%
        ungroup()

      plt_yr <- yearly %>%
        ggplot(aes(year, value, color = component, linetype = model_type)) +
        geom_line(linewidth = 0.7) +
        geom_point(size = 1.5) +
        ggrepel::geom_text_repel(
          aes(label = yoy_label), size = 2.5, show.legend = FALSE,
          direction = "y",
          nudge_y = 0.02 * max(yearly$value, na.rm = TRUE),
          segment.size = 0.2, segment.alpha = 0.5
        ) +
        labs(
          title = paste0(str_to_title(fuel_val), " demand: LM vs GAM (yearly)"),
          y = paste0("Demand (", first(na.omit(fuel_data$unit)), ")"),
          x = NULL, color = NULL, linetype = NULL
        ) +
        rcrea::theme_crea_new() +
        rcrea::scale_y_zero()

      quicksave(
        file.path(diagnostics_folder, paste0("comparison_yearly_", fuel_val, ".png")),
        plot = plt_yr, width = 12, height = 6, preview = FALSE
      )

      # Yearly weather-corrected comparison
      yearly_wc <- yearly %>%
        group_by(component, model_type) %>%
        arrange(year) %>%
        mutate(yoy_wc_pct = (value_weather_corrected / lag(value_weather_corrected) - 1) * 100,
               yoy_wc_label = ifelse(!is.na(yoy_wc_pct), sprintf("%+.1f%%", yoy_wc_pct), NA_character_)) %>%
        ungroup()

      plt_yr_wc <- yearly_wc %>%
        ggplot(aes(year, value_weather_corrected, color = component, linetype = model_type)) +
        geom_line(linewidth = 0.7) +
        geom_point(size = 1.5) +
        ggrepel::geom_text_repel(
          aes(label = yoy_wc_label), size = 2.5, show.legend = FALSE,
          direction = "y",
          nudge_y = 0.02 * max(yearly_wc$value_weather_corrected, na.rm = TRUE),
          segment.size = 0.2, segment.alpha = 0.5
        ) +
        labs(
          title = paste0(str_to_title(fuel_val), " demand weather-corrected: LM vs GAM (yearly)"),
          y = paste0("Demand (", first(na.omit(fuel_data$unit)), ")"),
          x = NULL, color = NULL, linetype = NULL
        ) +
        rcrea::theme_crea_new() +
        rcrea::scale_y_zero()

      quicksave(
        file.path(diagnostics_folder, paste0("comparison_yearly_wc_", fuel_val, ".png")),
        plot = plt_yr_wc, width = 12, height = 6, preview = FALSE
      )
    }
  }

  return(combined)
}

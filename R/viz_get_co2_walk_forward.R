plot_get_co2_walk_forward_validation <- function(
  comparison,
  output_folder,
  final_as_of,
  width = 10,
  height = 6,
  dpi = 300
) {
  create_dir(output_folder)

  plot_paths <- c(
    total = file.path(output_folder, "get_co2_walk_forward_total.png"),
    total_error = file.path(output_folder, "get_co2_walk_forward_total_error.png"),
    revision_heatmap = file.path(output_folder, "get_co2_walk_forward_revision_heatmap.png"),
    component_error = file.path(output_folder, "get_co2_walk_forward_component_error.png"),
    lag_small_multiples = file.path(output_folder, "get_co2_walk_forward_lag_small_multiples.png"),
    actual_vs_expected_scatter = file.path(output_folder, "get_co2_walk_forward_actual_vs_expected_scatter.png")
  )

  plot_get_co2_walk_forward_total(
    comparison = comparison,
    filepath = plot_paths[["total"]],
    final_as_of = final_as_of,
    width = width,
    height = height,
    dpi = dpi
  )
  plot_get_co2_walk_forward_total_error(
    comparison = comparison,
    filepath = plot_paths[["total_error"]],
    width = width,
    height = height,
    dpi = dpi
  )
  plot_get_co2_walk_forward_revision_heatmap(
    comparison = comparison,
    filepath = plot_paths[["revision_heatmap"]],
    width = width,
    height = height,
    dpi = dpi
  )
  plot_get_co2_walk_forward_component_error(
    comparison = comparison,
    filepath = plot_paths[["component_error"]],
    width = width,
    height = max(height, 7),
    dpi = dpi
  )
  plot_get_co2_walk_forward_lag_small_multiples(
    comparison = comparison,
    filepath = plot_paths[["lag_small_multiples"]],
    width = width,
    height = max(height, 7),
    dpi = dpi
  )
  plot_get_co2_walk_forward_actual_vs_expected_scatter(
    comparison = comparison,
    filepath = plot_paths[["actual_vs_expected_scatter"]],
    width = width,
    height = height,
    dpi = dpi
  )

  plot_paths
}


plot_get_co2_walk_forward_total <- function(
  comparison,
  filepath,
  final_as_of,
  width = 10,
  height = 6,
  dpi = 300
) {
  latest_comparison <- .get_co2_walk_forward_latest_months(comparison)
  final_label <- as.character(glue("Final as of {as.Date(final_as_of)}"))

  plot_data <- latest_comparison %>%
    filter(
      estimate == "central",
      fuel == FUEL_TOTAL,
      sector == SECTOR_ALL
    ) %>%
    select(iso2, target_month, walk_forward_value, final_value) %>%
    pivot_longer(
      cols = c(walk_forward_value, final_value),
      names_to = "series",
      values_to = "value"
    ) %>%
    mutate(
      series = recode(
        series,
        walk_forward_value = "Walk-forward estimate",
        final_value = final_label
      )
    )

  colors <- c(rcrea::pal_crea[["Blue"]], rcrea::pal_crea[["Dark.red"]])
  names(colors) <- c("Walk-forward estimate", final_label)

  plt <- ggplot(plot_data, aes(target_month, value / 1e6, color = series, linetype = series)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    (if (n_distinct(plot_data$iso2) > 1) {
      facet_wrap(~iso2, scales = "free_y")
    } else {
      NULL
    }) +
    scale_color_manual(values = colors) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    rcrea::theme_crea_new() +
    rcrea::scale_y_crea_zero() +
    labs(
      title = "Walk-forward vs final get_co2 monthly totals",
      subtitle = "Monthly total CO2: walk-forward run at each month-end compared with the final run",
      x = NULL,
      y = NULL,
      color = NULL,
      linetype = NULL,
      caption = "Source: CREA analysis."
    )

  rcrea::quicksave(filepath, plot = plt, width = width, height = height, dpi = dpi)

  plt
}


plot_get_co2_walk_forward_total_error <- function(
  comparison,
  filepath,
  width = 10,
  height = 6,
  dpi = 300
) {
  plot_data <- .get_co2_walk_forward_latest_months(comparison) %>%
    filter(
      estimate == "central",
      fuel == FUEL_TOTAL,
      sector == SECTOR_ALL
    ) %>%
    mutate(
      error_mt = diff / 1e6,
      error_label = scales::percent(pct_diff, accuracy = 0.1),
      direction = if_else(diff >= 0, "Above final", "Below final")
    )

  plt <- ggplot(plot_data, aes(target_month, error_mt, fill = direction)) +
    geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3) +
    geom_col(width = 25) +
    geom_text(
      aes(label = error_label),
      vjust = if_else(plot_data$error_mt >= 0, -0.4, 1.2),
      size = 3,
      color = "gray25"
    ) +
    (if (n_distinct(plot_data$iso2) > 1) {
      facet_wrap(~iso2, scales = "free_y")
    } else {
      NULL
    }) +
    scale_fill_manual(
      values = c(
        "Above final" = rcrea::pal_crea[["Dark.red"]],
        "Below final" = rcrea::pal_crea[["Blue"]]
      )
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    rcrea::theme_crea_new() +
    labs(
      title = "Walk-forward minus final get_co2 monthly totals",
      subtitle = "Bars show monthly total CO2 error; labels show percent difference versus the final run",
      x = NULL,
      y = NULL,
      fill = NULL,
      caption = "Source: CREA analysis."
    )

  rcrea::quicksave(filepath, plot = plt, width = width, height = height, dpi = dpi)

  plt
}


plot_get_co2_walk_forward_revision_heatmap <- function(
  comparison,
  filepath,
  width = 10,
  height = 6,
  dpi = 300
) {
  plot_data <- comparison %>%
    filter(
      estimate == "central",
      fuel == FUEL_TOTAL,
      sector == SECTOR_ALL
    ) %>%
    mutate(as_of_month = lubridate::floor_date(as.Date(as_of_date), "month"))

  plt <- ggplot(plot_data, aes(target_month, as_of_month, fill = pct_diff)) +
    geom_tile(color = "white", linewidth = 0.2) +
    (if (n_distinct(plot_data$iso2) > 1) {
      facet_wrap(~iso2)
    } else {
      NULL
    }) +
    scale_fill_gradient2(
      low = rcrea::pal_crea[["Blue"]],
      mid = "white",
      high = rcrea::pal_crea[["Dark.red"]],
      labels = scales::percent_format(accuracy = 1),
      na.value = "gray90"
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_date(date_breaks = "1 month", date_labels = "%b") +
    rcrea::theme_crea_new() +
    labs(
      title = "How monthly get_co2 estimates revise over time",
      subtitle = "Color shows percent difference between each walk-forward run and the final run",
      x = "Target month",
      y = "Run month",
      fill = "Walk-forward minus final",
      caption = "Source: CREA analysis."
    )

  rcrea::quicksave(filepath, plot = plt, width = width, height = height, dpi = dpi)

  plt
}


plot_get_co2_walk_forward_component_error <- function(
  comparison,
  filepath,
  width = 10,
  height = 7,
  dpi = 300
) {
  plot_data <- .get_co2_walk_forward_latest_months(comparison) %>%
    filter(
      estimate == "central",
      fuel != FUEL_TOTAL
    ) %>%
    mutate(
      component = paste(
        stringr::str_to_title(fuel),
        stringr::str_to_title(gsub("_", " ", sector)),
        sep = " - "
      ),
      abs_pct_diff = if_else(is.na(pct_diff), 0, abs(pct_diff))
    )

  component_levels <- plot_data %>%
    group_by(component) %>%
    summarise(max_abs_pct_diff = max(abs_pct_diff, na.rm = TRUE), .groups = "drop") %>%
    arrange(max_abs_pct_diff) %>%
    pull(component)

  plot_data <- plot_data %>%
    mutate(component = factor(component, levels = component_levels))

  plt <- ggplot(plot_data, aes(target_month, component, fill = pct_diff)) +
    geom_tile(color = "white", linewidth = 0.2) +
    (if (n_distinct(plot_data$iso2) > 1) {
      facet_wrap(~iso2)
    } else {
      NULL
    }) +
    scale_fill_gradient2(
      low = rcrea::pal_crea[["Blue"]],
      mid = "white",
      high = rcrea::pal_crea[["Dark.red"]],
      labels = scales::percent_format(accuracy = 1),
      na.value = "gray90"
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    rcrea::theme_crea_new() +
    labs(
      title = "Walk-forward component differences vs final get_co2 data",
      subtitle = "Color shows percent difference for each fuel-sector component in the first month it appears",
      x = NULL,
      y = NULL,
      fill = "Walk-forward minus final",
      caption = "Source: CREA analysis."
    )

  rcrea::quicksave(filepath, plot = plt, width = width, height = height, dpi = dpi)

  plt
}


plot_get_co2_walk_forward_lag_small_multiples <- function(
  comparison,
  filepath,
  width = 10,
  height = 7,
  dpi = 300
) {
  plot_data <- comparison %>%
    mutate(
      as_of_month = lubridate::floor_date(as.Date(as_of_date), "month"),
      lag_months = .get_co2_walk_forward_month_lag(target_month, as_of_month),
      lag_label = factor(
        lag_months,
        levels = 0:2,
        labels = c("Lag 0 months", "Lag 1 month", "Lag 2 months")
      )
    ) %>%
    filter(
      estimate == "central",
      fuel == FUEL_TOTAL,
      sector == SECTOR_ALL,
      lag_months %in% 0:2
    ) %>%
    select(
      iso2,
      target_month,
      lag_months,
      lag_label,
      walk_forward_value,
      final_value
    ) %>%
    pivot_longer(
      cols = c(walk_forward_value, final_value),
      names_to = "series",
      values_to = "value"
    ) %>%
    mutate(
      series = recode(
        series,
        walk_forward_value = "Walk-forward estimate",
        final_value = "Final value"
      )
    )

  plt <- ggplot(plot_data, aes(target_month, value / 1e6, color = series, linetype = series)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    (if (n_distinct(plot_data$iso2) > 1) {
      facet_grid(lag_label ~ iso2, scales = "free_y")
    } else {
      facet_wrap(~lag_label, ncol = 1)
    }) +
    scale_color_manual(values = c("Walk-forward estimate" = rcrea::pal_crea[["Blue"]], "Final value" = rcrea::pal_crea[["Dark.red"]])) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    rcrea::theme_crea_new() +
    rcrea::scale_y_crea_zero() +
    labs(
      title = "Walk-forward estimate vs final value by lag",
      subtitle = "Monthly total CO2; the same months shown at 0, 1, and 2 month lag",
      x = NULL,
      y = NULL,
      color = NULL,
      linetype = NULL,
      caption = "Source: CREA analysis."
    )

  rcrea::quicksave(filepath, plot = plt, width = width, height = height, dpi = dpi)

  plt
}


plot_get_co2_walk_forward_actual_vs_expected_scatter <- function(
  comparison,
  filepath,
  width = 10,
  height = 6,
  dpi = 300
) {
  plot_data <- comparison %>%
    mutate(
      as_of_month = lubridate::floor_date(as.Date(as_of_date), "month"),
      lag_months = .get_co2_walk_forward_month_lag(target_month, as_of_month)
    ) %>%
    filter(
      estimate == "central",
      fuel == FUEL_TOTAL,
      sector == SECTOR_ALL
    )

  plt <- ggplot(plot_data, aes(final_value / 1e6, walk_forward_value / 1e6, color = lag_months)) +
    geom_abline(intercept = 0, slope = 1, color = "gray70", linewidth = 0.4, linetype = "dashed") +
    geom_point(alpha = 0.8, size = 2) +
    (if (n_distinct(plot_data$iso2) > 1) {
      facet_wrap(~iso2)
    } else {
      NULL
    }) +
    scale_color_gradient(
      low = rcrea::pal_crea[["Blue"]],
      high = rcrea::pal_crea[["Dark.red"]],
      name = "Lag (months)"
    ) +
    rcrea::theme_crea_new() +
    labs(
      title = "Expected vs actual monthly total CO2",
      subtitle = "Each point is a month-end walk-forward estimate; color shows lag in months",
      x = "Final / actual value (Mt CO2)",
      y = "Walk-forward estimate (Mt CO2)",
      caption = "Source: CREA analysis."
    )

  rcrea::quicksave(filepath, plot = plt, width = width, height = height, dpi = dpi)

  plt
}


.get_co2_walk_forward_month_lag <- function(target_month, as_of_month) {
  (lubridate::year(as_of_month) - lubridate::year(target_month)) * 12 +
    (lubridate::month(as_of_month) - lubridate::month(target_month))
}


.get_co2_walk_forward_latest_months <- function(comparison) {
  comparison %>%
    mutate(as_of_month = lubridate::floor_date(as.Date(as_of_date), "month")) %>%
    filter(target_month == as_of_month) %>%
    select(-as_of_month)
}

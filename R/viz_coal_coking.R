.coal_coking_changed <- function(original, resolved, method, conflict, duplicate) {
  value_changed <- xor(is.finite(original), is.finite(resolved)) |
    (is.finite(original) & is.finite(resolved) & abs(original - resolved) > 1e-6)
  value_changed | method != "reported" | conflict | duplicate
}


#' Prepare original and resolved coking series for diagnostics
#'
#' @param diagnostics Coking provenance returned by `.resolve_coal_coking()`.
#' @param frequency One of `"monthly"` or `"annual"`.
#'
#' @return Long-form data for countries with a correction or source conflict.
#' @keywords internal
coal_coking_diagnostic_data <- function(
  diagnostics,
  frequency = c("monthly", "annual")
) {
  frequency <- match.arg(frequency)
  required <- c(
    "iso2", "time", "frequency", "original_value", "resolved_value",
    "method", "conflict", "duplicate"
  )
  if (!all(required %in% names(diagnostics))) {
    stop("Coking diagnostics are missing required provenance columns.")
  }

  series <- diagnostics %>%
    filter(.data$frequency == .env$frequency) %>%
    mutate(changed = .coal_coking_changed(
      original_value,
      resolved_value,
      method,
      conflict,
      duplicate
    ))
  affected <- series %>%
    group_by(iso2) %>%
    summarise(
      affected = any(changed),
      has_nonzero_value = any(
        abs(c(original_value, resolved_value)) > 1e-6,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    filter(affected, has_nonzero_value) %>%
    pull(iso2)

  series %>%
    filter(iso2 %in% affected) %>%
    select(
      iso2,
      time,
      method,
      conflict,
      duplicate,
      changed,
      original_value,
      resolved_value
    ) %>%
    tidyr::pivot_longer(
      cols = c(original_value, resolved_value),
      names_to = "series",
      values_to = "value"
    ) %>%
    mutate(series = recode(
      series,
      original_value = "Original",
      resolved_value = "Resolved"
    ))
}


#' Plot original and resolved coking series
#'
#' @param diagnostics Coking provenance returned by `.resolve_coal_coking()`.
#' @param frequency One of `"monthly"` or `"annual"`.
#'
#' @return A ggplot object, or `NULL` when no series was changed or flagged.
#' @keywords internal
plot_coal_coking_diagnostics <- function(
  diagnostics,
  frequency = c("monthly", "annual")
) {
  frequency <- match.arg(frequency)
  plot_data <- coal_coking_diagnostic_data(diagnostics, frequency)
  if (!nrow(plot_data)) return(NULL)

  period <- if (frequency == "monthly") "Monthly" else "Annual"
  plot <- ggplot(plot_data, aes(time, value, colour = series)) +
    geom_line(linewidth = 0.7, na.rm = TRUE) +
    geom_point(size = 1.2, alpha = 0.8, na.rm = TRUE) +
    geom_point(
      data = plot_data %>% filter(series == "Resolved", changed),
      colour = "#C5283D",
      fill = "#F4B942",
      shape = 21,
      size = 2.5,
      stroke = 0.7,
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    facet_wrap(~iso2, scales = "free_y", ncol = 3) +
    scale_colour_manual(values = c(Original = "#777777", Resolved = "#C5283D"))
  if (frequency == "annual") {
    plot <- plot + scale_x_date(date_breaks = "5 years", date_labels = "%Y")
  }

  plot +
    rcrea::theme_crea_new() +
    labs(
      title = paste(period, "coking corrections"),
      subtitle = paste(
        "Hard coal input to coke ovens | affected countries |",
        "original vs resolved | thousand tonnes"
      ),
      caption = paste(
        "Gold markers identify corrected observations; zero-only series are omitted.",
        "Facets use independent y-scales."
      ),
      x = NULL,
      y = NULL,
      colour = NULL
    )
}


#' Write coking correction diagnostic plots
#'
#' @param diagnostics Coking provenance returned by `.resolve_coal_coking()`.
#' @param diagnostics_folder Output directory.
#'
#' @return Invisibly, the paths of plots that were written.
#' @keywords internal
write_coal_coking_diagnostic_plots <- function(diagnostics, diagnostics_folder) {
  if (is_null_or_empty(diagnostics_folder)) return(invisible(character()))
  create_dir(diagnostics_folder)

  paths <- character()
  for (frequency in c("monthly", "annual")) {
    plot <- plot_coal_coking_diagnostics(diagnostics, frequency)
    if (is.null(plot)) next
    path <- file.path(
      diagnostics_folder,
      paste0("coal_coking_", frequency, "_original_vs_resolved.png")
    )
    facet_count <- n_distinct(plot$data$iso2)
    height <- max(6, min(24, 2.5 + 2.5 * ceiling(facet_count / 3)))
    rcrea::quicksave(
      path,
      plot = plot,
      width = 12,
      height = height,
      bg = "white",
      scale = 1.25,
      preview = FALSE
    )
    paths <- c(paths, path)
  }
  invisible(paths)
}

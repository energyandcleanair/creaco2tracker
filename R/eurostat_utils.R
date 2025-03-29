#' Fill gaps in time series with zeros when appropriate
#'
#' This function fills NA values with zeros when there are enough consecutive small values
#' before the NA, suggesting that the NA should be treated as zero.
#'
#' @param x A numeric vector with possible NA values
#' @param consecutive_required Number of consecutive small values required before filling NAs with zeros
#' @param prob Probability threshold for determining "small" values (default: 0.05)
#' @param beyond_last_na Whether to fill NAs that occur after the last non-NA value (default: FALSE)
#' @return A numeric vector with appropriate NA values filled with zeros
#' @export
fill_gaps_with_zero_when_suitable <- function(x,
                                             consecutive_required = 3,
                                             prob = 0.05,
                                             beyond_last_na = FALSE) {
  # 1. Determine threshold
  threshold <- quantile(x, probs = prob, na.rm = TRUE)

  # 2. Find the index of the last non-NA
  last_non_na_idx <- suppressWarnings(max(which(!is.na(x))))
  # If everything is NA, just return x
  if (!is.finite(last_non_na_idx)) return(x)

  out <- x
  # We'll track how many consecutive "small or zero" values we've seen so far.
  streak <- 0

  for (i in seq_along(out)) {
    if (i > last_non_na_idx & !beyond_last_na) {
      # Once we pass the last non-NA, do nothing
      break
    }
    if (!is.na(out[i])) {
      # Non-NA. If it's below/equal to threshold, it extends our streak; otherwise, streak resets
      if (out[i] <= threshold) {
        streak <- streak + 1
      } else {
        streak <- 0
      }
    } else {
      # We've hit an NA
      if (streak >= consecutive_required) {
        # Fill it with zero
        out[i] <- 0
        # 0 might be counted as "small" if 0 <= threshold
        if (0 <= threshold) {
          streak <- streak + 1
        } else {
          streak <- 0
        }
      } else {
        streak <- 0
      }
    }
  }

  out
}

#' Fill gaps in time series with interpolation when appropriate
#'
#' This function fills NA values with interpolated values when the coefficient of variation
#' is low enough to suggest that interpolation is appropriate.
#'
#' @param x A numeric vector with possible NA values
#' @param cv_threshold Coefficient of variation threshold below which interpolation is used (default: 0.1)
#' @param maxgap Maximum gap size to interpolate (default: 3)
#' @return A numeric vector with appropriate NA values filled with interpolated values
#' @export
fill_gaps_with_interp_when_suitable <- function(x, cv_threshold = 0.1, maxgap = 3) {
  cv <- sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
  if (!is.na(cv) & cv < cv_threshold) {
    return(zoo::na.approx(x, maxgap = maxgap, na.rm = FALSE))
  } else {
    return(x)
  }
}

#' Fill missing EU values using the sum of EU member countries
#'
#' This function calculates the sum of values from EU member countries and uses it
#' to fill missing values for the EU aggregate. It also validates that when both
#' EU aggregate and sum of countries exist, they are similar.
#'
#' @param data A dataframe containing the data with an iso2 column identifying countries
#' @param group_cols Column names to group by when summing country values
#' @param value_col Name of the column containing values to sum
#' @param min_countries Minimum number of countries required for a valid sum (default: 25)
#' @param tolerance Maximum allowed relative difference between EU value and sum of countries (default: 0.05)
#' @param eu_pattern Pattern to identify EU rows in the data (default: "European")
#' @return A dataframe with filled EU values
#' @export
fill_eu_from_countries_sum <- function(data,
                                      group_cols,
                                      min_countries = 25,
                                      tolerance = 0.05) {


  # Get EU member states
  eu_iso2s <- get_eu_iso2s(include_eu = FALSE)

  # Calculate sum of countries
  countries_sum <- data %>%
    add_iso2() %>%
    filter(iso2 %in% eu_iso2s) %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      values_sum_countries = sum(values, na.rm = TRUE),
      n = sum(!is.na(values)),
      .groups = "drop"
    ) %>%
    filter(n >= min_countries)

  # Identify EU rows
  eu_data <- data %>%
    filter(iso2=="EU")

  # Join with country sums
  eu_from_countries <- eu_data %>%
    ungroup() %>%
    # We do full join in case eu is missing some time x categories
    full_join(countries_sum, by = group_cols) %>%
    mutate(iso2="EU")


  # Validate when both values exist
  comparison <- eu_from_countries %>%
    filter(!is.na(values), !is.na(values_sum_countries),
           (values != 0  | values_sum_countries != 0)) %>%
    mutate(diff = abs(values - values_sum_countries) / values)

  if (nrow(comparison) > 0) {
    max_diff <- max(abs(comparison$values - comparison$values_sum_countries) /
                   comparison$values, na.rm = TRUE)

    if (max_diff > tolerance) {
      warning(sprintf("Maximum relative difference between EU value and sum of countries is %.2f, which exceeds tolerance of %.2f",
                     max_diff, tolerance))
    }
  }

  # Fill missing values
  eu_filled <- eu_from_countries %>%
    mutate(values = coalesce(values, values_sum_countries)) %>%
    select(-values_sum_countries, -n)

  # Only keep contiguous values (optional)
  # eu_filled <- eu_filled %>%
  #   group_by(across(setdiff(group_cols, "time"))) %>%
  #   arrange(time) %>%
  #   mutate(invalid = cumsum(diff(c(time[1], time)) > 31)) %>%
  #   filter(invalid == 0) %>%
  #   select(-invalid)

  # Replace in original data
  result <- bind_rows(
    # Keep all non-EU rows and EU rows that don't match with filled data
    data %>% anti_join(eu_filled, by = names(eu_filled)),
    # Add filled EU rows
    eu_filled
  )

  return(result)
}

#' Fill gaps in time series data with appropriate methods
#'
#' This function applies a series of gap filling methods to time series data:
#' 1. Completes missing time periods
#' 2. Fills gaps with zeros when appropriate
#' 3. Fills gaps with interpolation when appropriate
#'
#' @param data A dataframe containing time series data
#' @param group_cols Column names to group by when filling gaps
#' @param time_col Name of the column containing time values (default: "time")
#' @param frequency Time frequency for completing missing periods ("year", "month", etc.)
#' @param zero_consecutive_required Number of consecutive small values required before filling NAs with zeros
#' @param zero_consecutive_required_beyond_last Number of consecutive small values required for filling beyond last non-NA
#' @param zero_prob Probability threshold for determining "small" values (default: 0.05)
#' @param interp_cv_threshold Coefficient of variation threshold for interpolation (default: 0.1)
#' @param interp_maxgap Maximum gap size to interpolate (default: 3)
#' @param exclude_iso2s Vector of ISO2 codes to exclude from gap filling (default: "EU")
#' @return A dataframe with filled gaps
#' @export
fill_gaps_in_time_series <- function(data,
                                    group_cols,
                                    zero_consecutive_required = 3,
                                    zero_consecutive_required_beyond_last = 12,
                                    zero_prob = 0.05,
                                    interp_cv_threshold = 0.1,
                                    interp_maxgap = 3,
                                    exclude_iso2s = "EU") {

  # Ensure data has iso2 column
  data <- add_iso2(data)

  # Get time frequency from data
  if(all(lubridate::month(data$time) == 1)) {
    frequency <- "year"
  } else {
    frequency <- "month"
  }

  # Process data by group, excluding specified ISO2 codes
  result <- data %>%
    group_by(across(all_of(group_cols))) %>%
    group_modify(function(group_data, group_keys) {
      # Skip gap filling for excluded ISO2 codes
      if ("iso2" %in% names(group_data) &&
          any(group_data$iso2 %in% exclude_iso2s)) {
        return(group_data)
      }

      if ("iso2" %in% names(group_keys) &&
          any(group_keys$iso2 %in% exclude_iso2s)) {
        return(group_data)
      }

      # Get min and max time for this group
      min_time <- min(group_data$time, na.rm = TRUE)
      max_time <- max(group_data$time, na.rm = TRUE)

      # Complete missing time periods
      complete_data <- group_data %>%
        complete(
          time = seq.Date(min_time, max_time, by = frequency)
        ) %>%
        arrange(time)

      # Apply gap filling methods
      complete_data %>%
        mutate(
          # Fill gaps but stop at last NA
          values = fill_gaps_with_zero_when_suitable(
            values,
            consecutive_required = zero_consecutive_required,
            prob = zero_prob,
            beyond_last_na = FALSE
          ),
          # Fill gaps beyond last NA with more stringent requirements
          values = fill_gaps_with_zero_when_suitable(
            values,
            consecutive_required = zero_consecutive_required_beyond_last,
            prob = zero_prob,
            beyond_last_na = TRUE
          ),
          # Fill remaining gaps with interpolation when appropriate
          values = fill_gaps_with_interp_when_suitable(
            values,
            cv_threshold = interp_cv_threshold,
            maxgap = interp_maxgap
          )
        )
    }) %>%
    ungroup()

  return(result)
}

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


#' Fill missing EU values using the sum of EU member countries when the sum is historically close enough
#'
#' This function calculates the sum of values from EU member countries and uses it
#' to fill missing values for the EU aggregate, but only when the relative difference
#' between the sum and the EU values is small enough. It tries different combinations
#' of countries to fill as many rows as possible
#'
#' @param data A dataframe containing the data with an iso2 column identifying countries
#' @param group_cols Column names to group by when summing country values
#' @param min_countries Minimum number of countries required for a valid sum (default: 20)
#' @param max_rel_diff Maximum allowed relative difference between EU value and sum (default: 0.05)
#' @param min_points Minimum number of non-NA points required for correlation check (default: 12)
#' @return A dataframe with filled EU values
#' @export
fill_eu_from_countries_sum <- function(data,
                                       group_cols,
                                       min_countries = 20,
                                       max_rel_diff = 0.05,
                                       min_points = 12) {


  # Add iso2 and time to group_cols if in data cols and not in group_cols
  group_cols <- unique(c(group_cols, "iso2", "time"))

  # Get EU member states
  eu_iso2s <- get_eu_iso2s(include_eu = FALSE)

  # Split EU and country data
  eu_data <- data %>% filter(iso2 == "EU")
  country_data <- data %>% filter(iso2 %in% eu_iso2s)

  # Get latest date with EU data for each group
  latest_eu_dates <- eu_data %>%
    filter(!is.na(values)) %>%
    group_by(across(all_of(setdiff(group_cols, c("time", "iso2"))))) %>%
    summarise(latest_eu_date = max(time), .groups = "drop")

  # For each month after latest EU data, get country availability info
  countries_availability <- country_data %>%
    left_join(latest_eu_dates, by = setdiff(group_cols, c("time", "iso2"))) %>%
    filter(time > latest_eu_date) %>%
    group_by(time, across(all_of(setdiff(group_cols, c("time", "iso2"))))) %>%
    summarise(
      available_countries = list(iso2[!is.na(values)]),
      n_countries = length(available_countries[[1]]),
      .groups = "drop"
    ) %>%
    filter(n_countries >= min_countries)

  # Get distinct country sets
  country_sets <- countries_availability %>%
    distinct(available_countries, n_countries) %>%
    arrange(desc(n_countries))

  if(nrow(country_sets) == 0) {
    return(data)
  }

  # Initialize results list
  projected_results <- list()

  # Loop through each distinct set of countries
  for (i in 1:nrow(country_sets)) {
    current_countries <- country_sets$available_countries[[i]]

    # Calculate sum for this set of countries
    country_sums <- country_data %>%
      filter(iso2 %in% current_countries) %>%
      group_by(across(all_of(setdiff(group_cols, "iso2")))) %>%
      summarise(
        values_sum = sum(values, na.rm = TRUE),
        n = sum(!is.na(values)),
        .groups = "drop"
      ) %>%
      filter(n == length(current_countries))  # Only keep rows where all countries have data

    # Check correlation with existing EU data
    correlation_check <- eu_data %>%
      left_join(country_sums, by = setdiff(group_cols, "iso2")) %>%
      filter(!is.na(values), !is.na(values_sum)) %>%
      group_by(across(all_of(setdiff(group_cols, "time")))) %>%
      summarise(
        corr = list(check_proxy_correlation(values, values_sum, max_rel_diff = max_rel_diff, min_points = min_points)),
        .groups = "drop"
      ) %>%
      # Expand list
      unnest_wider(corr, names_sep = "_")


    # If correlation is good enough, use this set for filling
    if(any(correlation_check$corr_is_good_enough)) {
      # Get the groups where correlation is good enough
      good_groups <- correlation_check %>%
        filter(corr_is_good_enough) %>%
        select(-corr_rel_diff, -corr_n_points, -corr_is_good_enough)

      # Fill missing values for these groups
      current_projection <- eu_data %>%
        left_join(country_sums, by = setdiff(group_cols, "iso2")) %>%
        inner_join(good_groups, by = setdiff(group_cols, "time")) %>%
        mutate(
          values = coalesce(values, values_sum),
          order = i  # Add order based on country set size
        ) %>%
        select(-values_sum, -n)

      projected_results[[i]] <- current_projection
    }
  }

  # Combine all projections and take first available value for each combination
  final_projection <- bind_rows(projected_results) %>%
    group_by(across(all_of(group_cols))) %>%
    filter(!is.na(values)) %>%
    slice_min(order, n = 1) %>%  # Take row with smallest order number
    ungroup() %>%
    select(-order)  # Remove the order column

  # Combine projected EU data with original data, ensuring no duplicates
  bind_rows(
    data %>% anti_join(final_projection, by = group_cols),
    final_projection %>% distinct(across(all_of(group_cols)), .keep_all = TRUE)
  )
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

#' Check if proxy variable has good enough correlation with target variable
#'
#' @param target A numeric vector of target values
#' @param proxy A numeric vector of proxy values
#' @param max_rel_diff Maximum allowed relative difference between target and proxy (default: 0.05)
#' @param min_points Minimum number of non-NA points required (default: 12)
#' @return A list with correlation metrics and whether it's good enough
#' @export
check_proxy_correlation <- function(target, proxy, max_rel_diff=0.05, min_points=12) {
  # Remove rows where either target or proxy is NA
  valid_data <- data.frame(target=target, proxy=proxy) %>%
    filter(!is.na(target), !is.na(proxy))

  if(nrow(valid_data) < min_points) {
    return(list(
      rel_diff = 1,
      n_points = nrow(valid_data),
      is_good_enough = FALSE
    ))
  }

  # Calculate relative difference
  rel_diff <- abs(valid_data$target - valid_data$proxy) / valid_data$target
  max_rel_diff_observed <- max(rel_diff)

  list(
    rel_diff = max_rel_diff_observed,
    n_points = nrow(valid_data),
    is_good_enough = max_rel_diff_observed <= max_rel_diff
  )
}

#' Split Eurostat data into electricity and other sectors
#'
#' This function takes Eurostat fossil fuel consumption data and splits it into
#' 'electricity' and 'others' sectors, ensuring that the sum is preserved and
#' that the sectoral split is consistent. It is used for both solid and gas fuels.
#'
#' @param x A data frame with columns iso2, time, unit, siec_code, fuel, sector, values
#' @return A data frame with sectors split into 'electricity' and 'others'
#' @export
eurostat_split_elec_others <- function(x) {
  # Quick return if no work needed
  sectors <- unique(x$sector)
  if(setequal(sectors, c(SECTOR_ELEC, SECTOR_OTHERS))) {
    return(x)
  }

  group_cols <- intersect(names(x), c("iso2", "time", "unit", "siec_code", "fuel"))

  # Process data using pivot operations
  result <- x %>%
    pivot_wider(
      id_cols = all_of(group_cols),
      names_from = sector,
      values_from = values,
      values_fill = NA
    ) %>%
    add_missing_cols(c("all", "others", "electricity")) %>%
    mutate(
      # Handle case where only "all" data is available
      electricity = coalesce(electricity, 0),  # Default to 0 if no electricity data
      others = coalesce(others, all - electricity),  # calculate others value
      # If others is still NA (which can happen if all is NA), set it to 0
      others = coalesce(others, 0),
      # Recalculate electricity if it was set to 0 but we have all data
      electricity = case_when(
        !is.na(all) & electricity == 0 ~ all - others,
        TRUE ~ electricity
      )
    ) %>%
    select(-all) %>%
    pivot_longer(
      cols = c(electricity, others),
      names_to = "sector",
      values_to = "values"
    )

  result
}

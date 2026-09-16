COAL_MONTHLY_GAP_FUELS <- c(
  SIEC_HARD_COAL,
  SIEC_BROWN_COAL,
  SIEC_BROWN_COAL_BRIQUETTES,
  SIEC_OIL_SHALE
)

COAL_MONTHLY_GAP_BALANCES <- c("GID_CAL", "TI_EHG_MAP", "TI_CO")
COAL_MONTHLY_SUPPLY_BALANCES <- c("IPRD", "IMP", "EXP", "STK_CHG")

.coal_monthly_interpolation_enabled <- function(siec, nrg_bal, gap_type) {
  siec == SIEC_HARD_COAL & nrg_bal == "GID_CAL" & gap_type == "internal"
}

.coal_monthly_gap_keys <- function() c("iso2", "siec", "nrg_bal", "unit")

.coal_monthly_date_add <- function(date, months) {
  lubridate::`%m+%`(date, lubridate::period(months, "month"))
}

.coal_monthly_key <- function(data, include_time = TRUE) {
  columns <- .coal_monthly_gap_keys()
  if (include_time) columns <- c(columns, "time")
  do.call(paste, c(data[columns], sep = "\r"))
}

.coal_monthly_accounting <- function(
  target,
  monthly,
  annual,
  relative_tolerance = 0.05
) {
  if (target$nrg_bal[[1]] != "GID_CAL") {
    return(list(value = NA_real_, reason = "not_applicable"))
  }
  annual_columns <- c("iso2", "siec", "unit", "nrg_bal", "time", "values")
  if (!all(annual_columns %in% names(annual))) {
    return(list(value = NA_real_, reason = "annual_history_unavailable"))
  }

  target_inputs <- monthly %>%
    filter(
      iso2 == target$iso2[[1]],
      siec == target$siec[[1]],
      unit == target$unit[[1]],
      time == target$time[[1]],
      nrg_bal %in% COAL_MONTHLY_SUPPLY_BALANCES
    )

  if (
    nrow(target_inputs) != length(COAL_MONTHLY_SUPPLY_BALANCES) ||
      anyDuplicated(target_inputs$nrg_bal) ||
      any(is.na(target_inputs$values)) ||
      !setequal(target_inputs$nrg_bal, COAL_MONTHLY_SUPPLY_BALANCES)
  ) {
    return(list(value = NA_real_, reason = "target_supply_inputs_incomplete"))
  }

  eligible_years <- annual %>%
    filter(
      iso2 == target$iso2[[1]],
      siec == target$siec[[1]],
      unit == target$unit[[1]],
      nrg_bal == "IC_CAL",
      lubridate::year(time) < lubridate::year(target$time[[1]]),
      !is.na(values)
    ) %>%
    transmute(year = lubridate::year(time), annual_value = values) %>%
    group_by(year) %>%
    summarise(
      annual_rows = n(),
      annual_value = if (annual_rows == 1) first(annual_value) else NA_real_,
      .groups = "drop"
    ) %>%
    filter(annual_rows == 1) %>%
    arrange(desc(year))

  historical_supply <- monthly %>%
    filter(
      iso2 == target$iso2[[1]],
      siec == target$siec[[1]],
      unit == target$unit[[1]],
      nrg_bal %in% COAL_MONTHLY_SUPPLY_BALANCES,
      lubridate::year(time) < lubridate::year(target$time[[1]])
    ) %>%
    mutate(year = lubridate::year(time)) %>%
    group_by(year) %>%
    summarise(
      rows = n(),
      months = n_distinct(time),
      balances = n_distinct(nrg_bal),
      complete = rows == 48 && months == 12 && balances == 4 && all(!is.na(values)),
      supply = if (complete) {
        sum(case_when(
          nrg_bal == "IPRD" ~ values,
          nrg_bal == "IMP" ~ values,
          nrg_bal == "EXP" ~ -values,
          nrg_bal == "STK_CHG" ~ values
        ))
      } else {
        NA_real_
      },
      .groups = "drop"
    )

  reconciliation <- eligible_years %>%
    inner_join(historical_supply, by = "year") %>%
    filter(complete) %>%
    slice_head(n = 3) %>%
    mutate(
      within_bounds = case_when(
        annual_value == 0 ~ abs(supply) <= 1e-6,
        TRUE ~ abs(supply - annual_value) / abs(annual_value) <= relative_tolerance
      )
    )

  if (nrow(reconciliation) < 3 || any(!reconciliation$within_bounds)) {
    return(list(value = NA_real_, reason = "three_reconciled_years_unavailable"))
  }

  value <- sum(case_when(
    target_inputs$nrg_bal == "IPRD" ~ target_inputs$values,
    target_inputs$nrg_bal == "IMP" ~ target_inputs$values,
    target_inputs$nrg_bal == "EXP" ~ -target_inputs$values,
    target_inputs$nrg_bal == "STK_CHG" ~ target_inputs$values
  ))

  if (value < 0) {
    return(list(value = NA_real_, reason = "negative_prediction"))
  }

  list(value = value, reason = "eligible")
}

.coal_monthly_fill_group <- function(group, monthly, annual, country_cutoff, max_gap) {
  group <- arrange(group, time)
  first_reported <- min(group$time[!is.na(group$values)])
  observations <- group %>%
    transmute(
      time,
      original_value = values,
      source_flag = if ("flags" %in% names(group)) flags else NA_character_
    )
  calendar <- tibble::tibble(time = seq(first_reported, country_cutoff, by = "month")) %>%
    left_join(observations, by = "time") %>%
    mutate(reported = !is.na(original_value))

  missing <- rle(!calendar$reported)
  calendar$gap_id <- rep(seq_along(missing$lengths), missing$lengths)
  missing_calendar <- calendar %>% filter(!reported)
  gap_info <- if (nrow(missing_calendar) == 0) {
    tibble::tibble(
      gap_id = integer(), gap_start = as.Date(character()),
      gap_end = as.Date(character()), gap_length = integer(), gap_type = character()
    )
  } else {
    missing_calendar %>%
      group_by(gap_id) %>%
      summarise(
        gap_start = min(time),
        gap_end = max(time),
        gap_length = n(),
        gap_type = if_else(gap_end == country_cutoff, "trailing", "internal"),
        .groups = "drop"
      )
  }

  calendar <- calendar %>%
    left_join(gap_info, by = "gap_id") %>%
    mutate(
      value = original_value,
      method = if_else(reported, "reported", "unresolved"),
      accounting_reason = if_else(reported, "reported", NA_character_),
      interpolation_reason = if_else(reported, "reported", NA_character_),
      previous_year_reason = if_else(reported, "reported", NA_character_),
      accounting_input_date = as.Date(NA),
      interpolation_before_date = as.Date(NA),
      interpolation_after_date = as.Date(NA),
      previous_year_input_date = as.Date(NA)
    )

  missing_rows <- which(!calendar$reported)
  for (index in missing_rows) {
    row <- calendar[index, ] %>%
      mutate(
        iso2 = group$iso2[[1]],
        siec = group$siec[[1]],
        nrg_bal = group$nrg_bal[[1]],
        unit = group$unit[[1]]
      )

    if (row$gap_length[[1]] > max_gap) {
      calendar$accounting_reason[[index]] <- "gap_too_long"
      calendar$interpolation_reason[[index]] <- "gap_too_long"
      calendar$previous_year_reason[[index]] <- "gap_too_long"
      next
    }

    accounting <- .coal_monthly_accounting(row, monthly, annual)
    calendar$accounting_reason[[index]] <- accounting$reason
    if (!is.na(accounting$value)) {
      calendar$accounting_input_date[[index]] <- row$time[[1]]
      calendar$value[[index]] <- accounting$value
      calendar$method[[index]] <- "accounting"
      calendar$interpolation_reason[[index]] <- "not_attempted"
      calendar$previous_year_reason[[index]] <- "not_attempted"
      next
    }

    interpolation_enabled <- .coal_monthly_interpolation_enabled(
      row$siec[[1]], row$nrg_bal[[1]], row$gap_type[[1]]
    )
    if (interpolation_enabled) {
      before <- max(which(calendar$reported & seq_len(nrow(calendar)) < index))
      after <- min(which(calendar$reported & seq_len(nrow(calendar)) > index))
      if (is.finite(before) && is.finite(after)) {
        calendar$interpolation_before_date[[index]] <- calendar$time[[before]]
        calendar$interpolation_after_date[[index]] <- calendar$time[[after]]
        prediction <- stats::approx(
          x = c(before, after),
          y = calendar$original_value[c(before, after)],
          xout = index
        )$y
        if (!is.na(prediction) && prediction >= 0) {
          calendar$value[[index]] <- prediction
          calendar$method[[index]] <- "interpolation"
          calendar$interpolation_reason[[index]] <- "eligible"
          calendar$previous_year_reason[[index]] <- "not_attempted"
          next
        }
        calendar$interpolation_reason[[index]] <- "negative_prediction"
      } else {
        calendar$interpolation_reason[[index]] <- "reported_brackets_unavailable"
      }
    } else if (row$gap_type[[1]] != "internal") {
      calendar$interpolation_reason[[index]] <- "not_internal"
    } else {
      calendar$interpolation_reason[[index]] <- "validation_policy_not_enabled"
    }

    previous_date <- .coal_monthly_date_add(row$time[[1]], -12)
    calendar$previous_year_input_date[[index]] <- previous_date
    previous_value <- group$values[match(previous_date, group$time)]
    if (length(previous_value) == 1 && !is.na(previous_value) && previous_value >= 0) {
      calendar$value[[index]] <- previous_value
      calendar$method[[index]] <- "previous_year"
      calendar$previous_year_reason[[index]] <- "eligible"
    } else {
      calendar$previous_year_reason[[index]] <- "reported_previous_year_unavailable"
    }
  }

  calendar
}

.coal_monthly_add_rows <- function(monthly, completed) {
  keys <- c(.coal_monthly_gap_keys(), "time")
  prediction <- completed %>% select(all_of(keys), values = value)
  original_keys <- .coal_monthly_key(monthly)
  prediction_keys <- .coal_monthly_key(prediction)

  existing_prediction <- prediction$values[match(original_keys, prediction_keys)]
  replace <- is.na(monthly$values) & !is.na(existing_prediction)
  monthly$values[replace] <- existing_prediction[replace]

  new_prediction <- prediction[!prediction_keys %in% original_keys, ]
  if (nrow(new_prediction) == 0) return(monthly)

  new_rows <- monthly[rep(NA_integer_, nrow(new_prediction)), , drop = FALSE]
  for (column in keys) new_rows[[column]] <- new_prediction[[column]]
  new_rows$values <- new_prediction$values
  if ("freq" %in% names(new_rows)) new_rows$freq <- "M"
  if ("geo" %in% names(new_rows)) {
    geo_map <- monthly %>% filter(!is.na(geo)) %>% distinct(iso2, geo)
    new_rows$geo <- geo_map$geo[match(new_rows$iso2, geo_map$iso2)]
  }

  bind_rows(monthly, new_rows) %>% arrange(iso2, siec, nrg_bal, unit, time)
}

.coal_monthly_raw_reconciliation <- function(monthly, annual, provenance) {
  annual_values <- if (all(c("nrg_bal", "siec", "iso2", "unit", "time", "values") %in%
        names(annual))) {
    annual %>%
      filter(nrg_bal == "IC_CAL", siec %in% COAL_MONTHLY_GAP_FUELS) %>%
      transmute(iso2, siec, unit, year = lubridate::year(time), annual_value = values)
  } else {
    tibble::tibble(
      iso2 = character(), siec = character(), unit = character(), year = integer(),
      annual_value = numeric()
    )
  }

  monthly %>%
    filter(nrg_bal == "GID_CAL", siec %in% COAL_MONTHLY_GAP_FUELS) %>%
    select(iso2, siec, unit, time, values) %>%
    inner_join(
      provenance %>% select(iso2, siec, unit, time, method),
      by = c("iso2", "siec", "unit", "time")
    ) %>%
    mutate(year = lubridate::year(time)) %>%
    group_by(iso2, siec, unit, year) %>%
    summarise(
      month_count = n_distinct(time),
      monthly_value = if (month_count == 12 && all(!is.na(values))) sum(values) else NA_real_,
      source = case_when(
        all(method == "reported") ~ "reported",
        all(method != "reported") ~ "imputed",
        TRUE ~ "mixed"
      ),
      .groups = "drop"
    ) %>%
    left_join(annual_values, by = c("iso2", "siec", "unit", "year")) %>%
    mutate(
      difference = monthly_value - annual_value,
      zero_denominator = !is.na(annual_value) & annual_value == 0,
      relative_difference = if_else(
        is.na(annual_value) | zero_denominator,
        NA_real_,
        difference / abs(annual_value)
      ),
      within_bounds = case_when(
        is.na(monthly_value) | is.na(annual_value) ~ NA,
        zero_denominator ~ abs(difference) <= 1e-6,
        TRUE ~ abs(relative_difference) <= 0.05
      )
    )
}

#' Fill established raw monthly coal series
#'
#' @keywords internal
fill_raw_coal_monthly <- function(monthly, annual, max_gap = 6) {
  if (!"iso2" %in% names(monthly) || anyNA(monthly$iso2)) monthly <- monthly %>% add_iso2()
  if (nrow(annual) > 0 && (!"iso2" %in% names(annual) || anyNA(annual$iso2))) {
    annual <- annual %>% add_iso2()
  }
  eu_iso2s <- get_eu_iso2s(include_eu = FALSE)
  keys <- .coal_monthly_gap_keys()

  targets <- monthly %>%
    filter(
      iso2 %in% eu_iso2s,
      siec %in% COAL_MONTHLY_GAP_FUELS,
      nrg_bal %in% COAL_MONTHLY_GAP_BALANCES
    )
  established <- targets %>%
    group_by(across(all_of(keys))) %>%
    filter(any(!is.na(values))) %>%
    ungroup()
  units <- unique(targets$unit)
  if (length(units) == 0) units <- "THS_T"
  exclusions <- tidyr::crossing(
    iso2 = eu_iso2s,
    siec = COAL_MONTHLY_GAP_FUELS,
    nrg_bal = COAL_MONTHLY_GAP_BALANCES,
    unit = units
  ) %>%
    anti_join(
      established %>% distinct(across(all_of(keys))),
      by = keys
    ) %>%
    mutate(reason = "unavailable_series")

  if (nrow(established) == 0) {
    attr(monthly, "coal_gap_provenance") <- tibble::tibble()
    attr(monthly, "coal_gap_reconciliation") <- tibble::tibble()
    attr(monthly, "coal_gap_completeness") <- tibble::tibble()
    attr(monthly, "coal_gap_exclusions") <- exclusions
    return(monthly)
  }
  country_cutoffs <- monthly %>%
    filter(iso2 %in% eu_iso2s, siec %in% COAL_MONTHLY_GAP_FUELS, !is.na(values)) %>%
    group_by(iso2) %>%
    summarise(country_cutoff = max(time), .groups = "drop")

  series_keys <- established %>% distinct(across(all_of(keys)))
  completed <- bind_rows(lapply(seq_len(nrow(series_keys)), function(i) {
    key <- series_keys %>% slice(i)
    group <- established %>% semi_join(key, by = keys)
    cutoff <- country_cutoffs$country_cutoff[match(key$iso2[[1]], country_cutoffs$iso2)]
    .coal_monthly_fill_group(group, monthly, annual, cutoff, max_gap) %>%
      mutate(
        iso2 = key$iso2[[1]], siec = key$siec[[1]],
        nrg_bal = key$nrg_bal[[1]], unit = key$unit[[1]]
      )
  }))

  result <- .coal_monthly_add_rows(monthly, completed)
  provenance <- completed %>%
    transmute(
      iso2, siec, nrg_bal, unit, time,
      original_value, source_flag, reported, gap_start, gap_end, gap_length, gap_type,
      method, filled_value = value,
      accounting_reason, interpolation_reason, previous_year_reason,
      accounting_input_date, interpolation_before_date, interpolation_after_date,
      previous_year_input_date
    )
  reconciliation <- .coal_monthly_raw_reconciliation(result, annual, provenance)
  completeness <- provenance %>%
    group_by(time, siec, nrg_bal, unit) %>%
    summarise(
      expected_countries = length(eu_iso2s),
      available_countries = n_distinct(iso2[!is.na(filled_value)]),
      unresolved_countries = expected_countries - available_countries,
      complete = unresolved_countries == 0,
      .groups = "drop"
    )

  attr(result, "coal_gap_provenance") <- provenance
  attr(result, "coal_gap_reconciliation") <- reconciliation
  attr(result, "coal_gap_completeness") <- completeness
  attr(result, "coal_gap_exclusions") <- exclusions
  result
}

write_coal_gap_diagnostics <- function(monthly, diagnostics_folder) {
  if (is_null_or_empty(diagnostics_folder)) return(invisible(NULL))
  diagnostics <- list(
    coal_gap_provenance = attr(monthly, "coal_gap_provenance"),
    coal_gap_reconciliation = attr(monthly, "coal_gap_reconciliation"),
    coal_gap_completeness = attr(monthly, "coal_gap_completeness"),
    coal_gap_exclusions = attr(monthly, "coal_gap_exclusions")
  )
  dir.create(diagnostics_folder, recursive = TRUE, showWarnings = FALSE)
  raw_out_of_bounds <- diagnostics$coal_gap_reconciliation
  if (is.null(raw_out_of_bounds) || !"within_bounds" %in% names(raw_out_of_bounds)) {
    raw_out_of_bounds <- tibble::tibble()
  } else {
    raw_out_of_bounds <- raw_out_of_bounds %>% filter(!is.na(within_bounds), !within_bounds)
  }
  if (nrow(raw_out_of_bounds) > 0) {
    log_warn(paste0(
      "Raw coal monthly totals differ from annual balances by more than 5% for ",
      nrow(raw_out_of_bounds), " complete country-fuel series."
    ))
  }
  for (name in names(diagnostics)) {
    if (!is.null(diagnostics[[name]])) {
      readr::write_csv(diagnostics[[name]], file.path(diagnostics_folder, paste0(name, ".csv")))
    }
  }
  invisible(diagnostics)
}

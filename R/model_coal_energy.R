#' Annual coal energy use and its required balance components
#'
#' Inland consumption includes non-energy flows and statistical differences.
#' Combustion uses final energy plus transformation input, less the existing
#' coking deduction. An absent required balance is not a reported zero.
#' @keywords internal
coal_annual_energy <- function(x) {
  resolved <- isTRUE(attr(x, "coal_coking_resolved"))
  keys <- c("iso2", "time", "siec", "unit")
  balances <- c("FC_E", "TI_E", "TI_CO_E", COAL_ANNUAL_POWER_BALANCES)
  x %>%
    filter(siec %in% COAL_MONTHLY_GAP_FUELS, nrg_bal %in% balances) %>%
    group_by(across(all_of(c(keys, "nrg_bal")))) %>%
    summarise(values = if (n() == 1L) first(values) else NA_real_, .groups = "drop") %>%
    pivot_wider(names_from = nrg_bal, values_from = values) %>%
    add_missing_cols(balances) %>%
    mutate(
      TI_CO_E = if_else(!resolved & .coking_conflict(iso2, time, TI_CO_E),
        NA_real_, TI_CO_E),
      coking_required = siec == SIEC_HARD_COAL &
        (is.na(TI_E) | TI_E != 0 | (!is.na(TI_CO_E) & TI_CO_E != 0)),
      coking = if_else(coking_required, TI_CO_E, 0),
      energy = FC_E + TI_E - (1 - HARDCOAL_COKING_RATE_FACTOR) * coking,
      electricity = if_else(TI_E == 0 & !is.na(TI_E), 0,
        TI_EHG_MAPE_E + TI_EHG_MAPCHP_E),
      energy = if_else(FC_E >= 0 & TI_E >= 0 & coking >= 0 & coking <= TI_E & energy >= 0,
        energy, NA_real_),
      electricity = if_else(electricity >= 0 & (is.na(energy) | electricity <= energy),
        electricity, NA_real_),
      status = case_when(
        is.na(energy) ~ "unresolved",
        energy == 0 ~ "reported_zero_energy_use",
        TRUE ~ "reported_annual_energy"
      ),
      fuel = FUEL_COAL
    )
}

#' Classify energy systems without requiring an unavailable monthly series
#' @keywords internal
coal_energy_coverage <- function(annual, reported) {
  monthly <- reported %>%
    filter(siec %in% COAL_MONTHLY_GAP_FUELS, nrg_bal == "GID_CAL") %>%
    mutate(year = lubridate::year(time)) %>%
    group_by(iso2, siec, unit, year) %>%
    summarise(reported_months = n_distinct(time[!is.na(values)]), .groups = "drop")
  coal_annual_energy(annual) %>%
    mutate(year = lubridate::year(time)) %>%
    left_join(monthly, by = c("iso2", "siec", "unit", "year")) %>%
    mutate(
      reported_months = coalesce(reported_months, 0L),
      coverage = case_when(
        !is.na(energy) & energy == 0 ~ "reported_zero_energy_use",
        reported_months == 12 ~ "monthly_reported",
        reported_months > 0 ~ "monthly_with_gaps",
        !is.na(energy) ~ "annual_only",
        TRUE ~ "unresolved"
      )
    )
}

#' Allocate an annual coal quantity while preserving reported monthly values
#' @keywords internal
coal_allocate_annual <- function(annual, monthly) {
  keys <- c("iso2", "siec", "unit", "fuel", "sector")
  annual <- annual %>% filter(iso2 %in% get_eu_iso2s(include_eu = TRUE),
    siec %in% COAL_MONTHLY_GAP_FUELS)
  if (nrow(annual) == 0) return(monthly[0, ])
  reported <- attr(monthly, "coal_reported_monthly")
  monthly <- monthly %>% filter(siec %in% COAL_MONTHLY_GAP_FUELS)
  if (is.null(reported)) reported <- monthly
  # A reported total profile is also usable when its historical split was known.
  totals <- reported %>%
    group_by(iso2, siec, unit, fuel, time) %>%
    summarise(
      values = if (any(sector == SECTOR_UNKNOWN)) {
        sum(values)
      } else if (all(c(SECTOR_ELEC, SECTOR_OTHERS) %in% sector)) {
        sum(values)
      } else NA_real_, .groups = "drop"
    ) %>% mutate(sector = SECTOR_UNKNOWN)
  history <- bind_rows(reported %>% filter(sector != SECTOR_UNKNOWN), totals)
  groups <- split(history, do.call(paste, c(history[keys], sep = "\r")))
  current_groups <- split(monthly, do.call(paste, c(monthly[keys], sep = "\r")))
  profile_cache <- new.env(parent = emptyenv())
  annual_ids <- do.call(paste, c(annual[keys], sep = "\r"))
  annual_years <- lubridate::year(annual$time)
  rows <- lapply(seq_len(nrow(annual)), function(i) {
    row <- annual[i, ]
    yr <- annual_years[i]
    id <- annual_ids[i]
    dates <- seq(as.Date(paste0(yr, "-01-01")), by = "month", length.out = 12)
    group <- groups[[id]]
    if (is.null(group)) group <- history[0, ]
    current <- current_groups[[id]]
    if (is.null(current)) current <- monthly[0, ]
    current <- current[lubridate::year(current$time) == yr, ]
    observed <- current$values[match(dates, current$time)]
    reported_values <- group$values[match(dates, group$time)]
    # An explicitly zero annual energy balance is a stronger definition check
    # than a contradictory complete monthly series. This catches overlapping
    # monthly fuel classifications without introducing country-specific rules.
    annual_zero_conflict <- is.finite(row$values) && abs(row$values) <= 1e-6 &&
      all(is.finite(observed)) && any(abs(observed) > 1e-6)
    if (annual_zero_conflict) observed[] <- NA_real_
    shares_for <- function(year_value) {
      cache_key <- paste(id, year_value)
      if (exists(cache_key, profile_cache, inherits = FALSE)) {
        return(get(cache_key, profile_cache))
      }
      past <- group[lubridate::year(group$time) == year_value, ]
      past <- past[order(past$time), ]
      if (nrow(past) != 12 || anyDuplicated(past$time) || anyNA(past$values) ||
        any(past$values < 0) || sum(past$values) <= 0) {
        assign(cache_key, NULL, profile_cache)
        return(NULL)
      }
      shares <- past$values / sum(past$values)
      assign(cache_key, shares, profile_cache)
      shares
    }
    shares <- shares_for(yr - 1L)
    method <- "previous_year_profile"
    profile_years <- as.character(yr - 1L)
    if (is.null(shares)) {
      # Average the latest three complete years within a bounded five-year window.
      candidates <- seq.int(yr - 1L, yr - 5L)
      past <- setNames(lapply(candidates, shares_for), candidates)
      past <- Filter(Negate(is.null), past)
      if (length(past) >= 3L) {
        shares <- Reduce(`+`, past[1:3]) / 3
        method <- "three_year_profile"
        profile_years <- paste(names(past)[1:3], collapse = ",")
      } else {
        # Monthly solid-fuel observations are only used in production from
        # 2020 onwards. Earlier annual history has always been backfilled with
        # an observed seasonal climatology. Retain that profile for those
        # historical rows, rather than flattening them to equal twelfths. This
        # is explicitly a retrospective reconstruction: projections and
        # current gaps still use only preceding observations.
        profile_candidates <- sort(unique(lubridate::year(group$time)))
        climatology <- setNames(
          lapply(profile_candidates, shares_for),
          profile_candidates
        )
        climatology <- Filter(Negate(is.null), climatology)
        if (yr < 2020L && length(climatology) > 0L) {
          shares <- Reduce(`+`, climatology) / length(climatology)
          method <- "historical_reported_climatology"
          profile_years <- paste(names(climatology), collapse = ",")
        } else {
          shares <- rep(1 / 12, 12)
          method <- "equal_months"
          profile_years <- NA_character_
        }
      }
    }
    missing <- is.na(observed)
    remaining <- row$values - sum(observed[!missing])
    valid <- is.finite(remaining) && remaining >= -1e-6
    prediction <- rep(NA_real_, 12)
    if (valid && any(missing)) {
      if (abs(remaining) <= 1e-6) {
        prediction[missing] <- 0
      } else if (sum(shares[missing]) > 0) {
        prediction[missing] <- remaining * shares[missing] / sum(shares[missing])
      }
    }
    prediction[!missing] <- observed[!missing]
    output <- row[rep(1, 12), ]
    output$time <- dates
    output$values <- prediction
    is_reported <- !is.na(reported_values) & !missing &
      abs(reported_values - observed) <= 1e-6
    output$allocation_method <- if (annual_zero_conflict) {
      "reported_zero_annual_bound"
    } else ifelse(!missing,
      ifelse(is_reported, "reported", "preserved_monthly_estimate"), method)
    output$allocation_status <- if (!valid) "annual_remainder_inconsistent" else "estimated"
    output$annual_value <- row$values
    output$profile_years <- profile_years
    output$original_value <- reported_values
    output
  })
  result <- bind_rows(rows)
  diagnostics <- result
  result <- result %>% select(all_of(names(monthly)))
  attr(result, "coal_allocation") <- diagnostics
  result
}

#' Project separately accounted coal fuels before aggregating their emissions
#'
#' Complete totals are forecast from original history only. Allocated annual
#' observations retain their provenance and are never regression training rows.
#' Forecast totals below known sector consumption are raised to the known sum.
#' Diagnostics retain the original forecast and shortfall and flag the conflict.
#' @keywords internal
coal_prepare_total_forecasts <- function(
  x, date_to, diagnostics_folder = NULL,
  forecast_method = c("previous_year", "three_year_average")
) {
  forecast_method <- match.arg(forecast_method)
  if (is.null(date_to)) date_to <- Sys.Date()
  end <- lubridate::floor_date(as.Date(date_to), "month")
  keys <- c("iso2", "siec", "unit", "fuel")
  allocation <- attr(x, "coal_allocation")
  coal <- x %>% filter(iso2 %in% get_eu_iso2s(include_eu = TRUE),
    siec %in% COAL_MONTHLY_GAP_FUELS, time <= end)
  selected <- coal %>%
    group_by(across(all_of(keys))) %>%
    filter(lubridate::year(time) >= max(lubridate::year(time)) - 1L) %>%
    filter((sector == SECTOR_UNKNOWN & !is.na(values)) |
      (is.finite(values) & values < 0)) %>%
    ungroup() %>% distinct(across(all_of(keys)))
  if (!is.null(allocation) && nrow(allocation) > 0) {
    annual_only <- allocation %>%
      group_by(across(all_of(keys))) %>%
      summarise(annual_only = all(is.na(original_value)) && any(is.finite(values)),
        .groups = "drop") %>%
      filter(annual_only) %>% select(all_of(keys))
    selected <- bind_rows(selected, annual_only) %>% distinct()
  }
  original <- coal %>% semi_join(selected, by = keys)
  if (nrow(original) == 0) return(x)
  groups <- original %>% group_by(across(all_of(keys))) %>% group_split()
  provenance <- list()
  projected <- lapply(seq_along(groups), function(index) {
    group <- groups[[index]]
    # A negative derived sector is an inconsistent split, not negative fuel use.
    # Bound a conflicting split to its finite, non-negative total. An invalid total
    # remains unresolved while usable non-negative sector observations survive.
    invalid_dates <- as.Date(character())
    conflicting_dates <- unique(group$time[is.finite(group$values) & group$values < 0])
    for (date_index in seq_along(conflicting_dates)) {
      conflict_date <- conflicting_dates[date_index]
      rows <- group %>% filter(time == conflict_date)
      complete <- !anyNA(rows$values) &&
        (any(rows$sector == SECTOR_UNKNOWN) ||
          all(c(SECTOR_ELEC, SECTOR_OTHERS) %in% rows$sector))
      total <- if (complete) sum(rows$values) else NA_real_
      valid_total <- is.finite(total) && total >= 0
      provenance[[length(provenance) + 1L]] <<- group[1, keys] %>% mutate(
        time = conflict_date, method = if (valid_total)
          "conflicting_split_bounded_to_total" else "unresolved_invalid_split",
        input_date = conflict_date, total = if (valid_total) total else NA_real_,
        known_sectors = sum(rows$values[rows$values >= 0], na.rm = TRUE),
        residual = sum(rows$values[rows$values < 0], na.rm = TRUE),
        conflict = TRUE, three_year_total = NA_real_,
        forecast_total = NA_real_, forecast_residual = NA_real_
      )
      if (valid_total) {
        replacement <- rows %>% mutate(values = pmax(0, values))
        positive_total <- sum(replacement$values)
        replacement$values <- if (positive_total > 0) {
          replacement$values * total / positive_total
        } else rep(0, nrow(replacement))
      } else {
        invalid_dates <- c(invalid_dates, conflict_date)
        replacement <- rows %>% mutate(values = if_else(values < 0, NA_real_, values))
      }
      group <- bind_rows(group %>% filter(time != conflict_date), replacement)
    }
    # Allocate known totals from recent complete splits of this country and fuel.
    # Freeze the reference rows so inferred splits cannot propagate through history.
    split_history <- group %>%
      group_by(time) %>%
      filter(n() == 2L, all(c(SECTOR_ELEC, SECTOR_OTHERS) %in% sector),
        all(is.finite(values) & values >= 0), sum(values) > 0) %>% ungroup()
    unknown_dates <- unique(group$time[group$sector == SECTOR_UNKNOWN &
      is.finite(group$values) & group$values >= 0])
    for (date_index in seq_along(unknown_dates)) {
      date <- unknown_dates[date_index]
      rows <- group %>% filter(time == date)
      if (nrow(rows) != 1L) next
      reference <- split_history %>% filter(time < date,
        time >= date %m-% lubridate::years(5)) %>%
        mutate(same_month = lubridate::month(time) == lubridate::month(date)) %>%
        arrange(desc(same_month), desc(time))
      if (nrow(reference) == 0) next
      reference <- reference %>% filter(time == first(time))
      replacement <- rows[rep(1, 2), ]
      replacement$sector <- reference$sector
      replacement$values <- rows$values * reference$values / sum(reference$values)
      provenance[[length(provenance) + 1L]] <<- group[1, keys] %>% mutate(
        time = date, method = "recent_historical_split", input_date = reference$time[1],
        total = rows$values, known_sectors = 0, residual = rows$values,
        conflict = FALSE, three_year_total = NA_real_,
        forecast_total = NA_real_, forecast_residual = NA_real_
      )
      group <- bind_rows(group %>% filter(time != date), replacement)
    }
    totals <- group %>% group_by(time) %>% summarise(
      total = if (anyNA(values) ||
        (!any(sector == SECTOR_UNKNOWN) &&
          !all(c(SECTOR_ELEC, SECTOR_OTHERS) %in% sector))) NA_real_ else sum(values),
      .groups = "drop"
    )
    latest <- totals %>% filter(!is.na(total))
    if (nrow(latest) == 0) {
      dates <- seq(min(group$time), end, by = "month")
      dates <- dates[!dates %in% group$time]
      missing <- group[rep(1, length(dates)), ] %>% mutate(
        time = dates, sector = SECTOR_UNKNOWN, values = NA_real_
      )
      provenance[[length(provenance) + 1L]] <<- missing %>% select(all_of(keys), time) %>%
        mutate(method = "unresolved_no_total_history", input_date = as.Date(NA),
          total = NA_real_, known_sectors = 0, residual = NA_real_, conflict = FALSE,
          three_year_total = NA_real_, forecast_total = NA_real_, forecast_residual = NA_real_)
      return(bind_rows(group, missing))
    }
    limit <- as.Date(paste0(max(lubridate::year(latest$time)) + 1L, "-12-01"))
    dates <- seq(min(group$time), end, by = "month")
    # Keep all complete source months together; only incomplete months need a
    # forecast decision. This also avoids materialising forecasts as history.
    complete_dates <- latest$time
    dates <- dates[!dates %in% complete_dates]
    out <- lapply(seq_along(dates), function(j) {
      date <- dates[j]
      rows <- group %>% filter(time == date)
      if (date %in% invalid_dates) return(rows)
      actual <- totals$total[match(date, totals$time)]
      previous_date <- date %m-% lubridate::years(1)
      previous <- totals$total[match(previous_date, totals$time)]
      trailing <- totals$total[match(date %m-% lubridate::years(1:3), totals$time)]
      candidate <- if (forecast_method == "previous_year") previous else {
        if (all(is.finite(trailing))) mean(trailing) else NA_real_
      }
      method <- "reported_or_annual_allocated"
      total <- actual
      if (is.na(total)) {
        total <- if (date > max(latest$time) && date <= limit) candidate else NA_real_
        method <- if (!is.na(total)) paste0(forecast_method, "_total") else "unresolved"
      }
      known <- rows %>% filter(sector != SECTOR_UNKNOWN, !is.na(values))
      residual <- total - sum(known$values)
      conflict <- !is.na(residual) && residual < -1e-6
      provenance[[length(provenance) + 1L]] <<- group[1, keys] %>% mutate(
        time = date, method = method, input_date = previous_date,
        total = total, known_sectors = sum(known$values),
        residual = residual, conflict = conflict,
        forecast_total = if (is.na(actual)) total else NA_real_,
        forecast_residual = if (is.na(actual)) residual else NA_real_,
        three_year_total = if (all(is.finite(trailing))) mean(trailing) else NA_real_
      )
      if (!is.na(actual)) return(rows)
      if (conflict && is.finite(total) && total >= 0) {
        missing_sectors <- setdiff(c(SECTOR_ELEC, SECTOR_OTHERS), known$sector)
        zeros <- group[rep(1, length(missing_sectors)), ] %>%
          mutate(time = date, sector = missing_sectors, values = 0)
        provenance[[length(provenance)]]$method <<- paste0(method, "_raised_to_known_sectors")
        provenance[[length(provenance)]]$total <<- sum(known$values)
        provenance[[length(provenance)]]$residual <<- 0
        return(bind_rows(known, zeros))
      }
      if (conflict) residual <- NA_real_
      if (is.finite(residual) && residual >= 0 && nrow(known) == 1L &&
        known$sector %in% c(SECTOR_ELEC, SECTOR_OTHERS)) {
        remainder <- group[1, ] %>% mutate(time = date,
          sector = setdiff(c(SECTOR_ELEC, SECTOR_OTHERS), known$sector), values = residual)
        return(bind_rows(known, remainder))
      }
      # A valid original split from the same month of the preceding year can
      # allocate a forecast total. Estimated rows never become training history.
      split <- group %>% filter(time == previous_date,
        sector %in% c(SECTOR_ELEC, SECTOR_OTHERS))
      valid_split <- nrow(split) == 2L &&
        all(c(SECTOR_ELEC, SECTOR_OTHERS) %in% split$sector) &&
        all(is.finite(split$values) & split$values >= 0) && sum(split$values) > 0
      if (is.finite(total) && total >= 0 && !conflict && valid_split) {
        missing_sectors <- split %>% filter(!sector %in% known$sector)
        if (nrow(missing_sectors) > 0 && sum(missing_sectors$values) > 0) {
          missing_sectors <- missing_sectors %>% mutate(
            time = date, values = residual * values / sum(values)
          )
          provenance[[length(provenance)]]$method <<- paste0(method, "_previous_year_split")
          return(bind_rows(known, missing_sectors))
        }
      }
      unknown <- group[1, ] %>% mutate(
        time = date, sector = SECTOR_UNKNOWN, values = residual
      )
      bind_rows(known, unknown)
    })
    bind_rows(group %>% filter(time %in% complete_dates), bind_rows(out))
  }) %>% bind_rows()
  result <- bind_rows(x %>% anti_join(selected, by = keys), projected)
  for (name in c("coal_eu_repair_candidates", "coal_allocation", "coal_coking_provenance")) {
    attr(result, name) <- attr(x, name)
  }
  attr(result, "coal_separate_projection") <- selected
  diagnostics <- bind_rows(provenance)
  attr(result, "coal_total_forecasts") <- diagnostics
  if (!is_null_or_empty(diagnostics_folder)) {
    create_dir(diagnostics_folder)
    readr::write_csv(diagnostics, file.path(diagnostics_folder, "coal_total_forecasts.csv"))
  }
  result
}

#' Identify projections that resolve, overwrite or leave missing coal components
#' @keywords internal
coal_projection_diagnostics <- function(before, after) {
  separate <- attr(before, "coal_separate_projection")
  original <- bind_rows(before, separate) %>% filter(fuel == FUEL_COAL) %>%
    group_by(iso2, date, fuel, sector) %>%
    summarise(original = if (anyNA(value)) NA_real_ else sum(value), .groups = "drop")
  after %>% filter(fuel == FUEL_COAL, estimate == "central") %>%
    select(iso2, date, fuel, sector, projected = value) %>%
    full_join(original, by = c("iso2", "date", "fuel", "sector")) %>% mutate(
      status = case_when(
        is.na(projected) ~ "unresolved",
        is.na(original) ~ "existing_downstream_estimate",
        abs(projected - original) > 1e-6 ~ "overwritten",
        TRUE ~ "preserved"
      )
    )
}

#' Rebuild absent EU coal observations only from a complete member-state set
#' @keywords internal
coal_fill_complete_eu <- function(x) {
  keys <- intersect(names(x), c("time", "siec", "nrg_bal", "sector", "unit"))
  members <- get_eu_iso2s()
  countries <- x %>% filter(iso2 %in% members) %>%
    group_by(across(all_of(keys))) %>% summarise(
      countries = n_distinct(iso2[!is.na(values)]),
      unique_rows = n() == n_distinct(iso2),
      missing_countries = paste(setdiff(members, iso2[!is.na(values)]), collapse = ","),
      country_sum = if (countries == length(members) && unique_rows) sum(values) else NA_real_,
      .groups = "drop"
    )
  eu <- x %>% filter(iso2 == "EU") %>% select(all_of(keys), eu_value = values)
  diagnostics <- countries %>% full_join(eu, by = keys) %>% mutate(
    status = case_when(
      !is.na(eu_value) ~ "reported_eu_preserved",
      !is.na(country_sum) ~ "rebuilt_complete_members",
      TRUE ~ "unresolved_member_coverage"
    )
  )
  result <- x %>% left_join(countries %>% select(all_of(keys), country_sum), by = keys) %>%
    mutate(values = if_else(iso2 == "EU", coalesce(values, country_sum), values)) %>%
    select(-country_sum)
  absent <- countries %>% filter(!is.na(country_sum)) %>% anti_join(eu, by = keys)
  if (nrow(absent) > 0) {
    new_rows <- x[rep(NA_integer_, nrow(absent)), ]
    for (key in keys) new_rows[[key]] <- absent[[key]]
    new_rows$iso2 <- "EU"
    new_rows$values <- absent$country_sum
    result <- bind_rows(result, new_rows)
  }
  attr(result, "coal_eu_completeness") <- diagnostics
  result
}

COAL_ANNUAL_POWER_BALANCES <- c("TI_EHG_MAPE_E", "TI_EHG_MAPCHP_E")

.coal_annual_policy_table <- function() {
  # These inherited switches describe raw-balance evidence. They are not
  # claims that a policy passes the separate downstream H1 comparison.
  tibble::tribble(
    ~siec, ~nrg_bal, ~gap_scenario, ~enabled, ~reason,
    SIEC_HARD_COAL, "GID_CAL", "partial", TRUE, "raw_balance_validation_only",
    SIEC_HARD_COAL, "GID_CAL", "long", TRUE, "raw_balance_validation_only",
    SIEC_HARD_COAL, "TI_EHG_MAP", "any", TRUE, "raw_balance_validation_only",
    SIEC_HARD_COAL, "TI_CO", "any", FALSE, "validation_failed",
    SIEC_BROWN_COAL, "GID_CAL", "partial", TRUE, "raw_balance_validation_only",
    SIEC_BROWN_COAL, "GID_CAL", "long", TRUE, "raw_balance_validation_only",
    SIEC_BROWN_COAL, "TI_EHG_MAP", "any", TRUE, "raw_balance_validation_only",
    SIEC_OIL_SHALE, "GID_CAL", "partial", TRUE, "raw_balance_validation_only",
    SIEC_OIL_SHALE, "GID_CAL", "long", FALSE, "validation_failed",
    SIEC_OIL_SHALE, "TI_EHG_MAP", "any", FALSE, "validation_failed",
    SIEC_BROWN_COAL_BRIQUETTES, "GID_CAL", "partial", FALSE,
    "insufficient_validation_coverage",
    SIEC_BROWN_COAL_BRIQUETTES, "GID_CAL", "long", FALSE,
    "insufficient_validation_coverage",
    SIEC_BROWN_COAL_BRIQUETTES, "TI_EHG_MAP", "any", FALSE,
    "insufficient_validation_coverage"
  )
}

.coal_annual_policy_enabled <- function(siec, nrg_bal, missing_months) {
  if (length(nrg_bal) != 1L || length(siec) != 1L || length(missing_months) != 1L) {
    return(FALSE)
  }
  scenario <- if (nrg_bal == "GID_CAL") {
    if (missing_months <= 6) "partial" else "long"
  } else {
    "any"
  }
  decision <- .coal_annual_policy_table() %>%
    filter(
      .data$siec == .env$siec,
      .data$nrg_bal == .env$nrg_bal,
      .data$gap_scenario == .env$scenario
    ) %>%
    pull(enabled)
  length(decision) == 1 && decision
}

.coal_annual_target_code <- function(nrg_bal) {
  dplyr::case_when(
    nrg_bal == "GID_CAL" ~ "IC_CAL",
    nrg_bal == "TI_CO" ~ "TI_CO_E",
    TRUE ~ NA_character_
  )
}

.coal_unique_value <- function(data) {
  values <- data$values[!is.na(data$values)]
  if (length(values) != 1) NA_real_ else values[[1]]
}

.coal_annual_direct_value <- function(annual, iso2, siec, unit, year, nrg_bal) {
  annual_year <- if (".coal_year" %in% names(annual)) {
    annual$.coal_year
  } else {
    lubridate::year(annual$time)
  }
  rows <- annual[
    annual$iso2 == iso2 & annual$siec == siec & annual$unit == unit &
      annual_year == year,
    ,
    drop = FALSE
  ]

  if (nrg_bal == "TI_EHG_MAP") {
    power <- rows %>% filter(nrg_bal %in% COAL_ANNUAL_POWER_BALANCES)
    if (!setequal(power$nrg_bal[!is.na(power$values)], COAL_ANNUAL_POWER_BALANCES) ||
      anyDuplicated(power$nrg_bal)) {
      return(NA_real_)
    }
    return(sum(power$values))
  }

  annual_code <- .coal_annual_target_code(nrg_bal)
  .coal_unique_value(rows %>% filter(nrg_bal == annual_code))
}

.coal_annual_history <- function(annual, iso2, siec, unit, target_year, nrg_bal) {
  annual_year <- if (".coal_year" %in% names(annual)) {
    annual$.coal_year
  } else {
    lubridate::year(annual$time)
  }
  first_year <- min(annual_year, na.rm = TRUE)
  years <- seq.int(first_year, target_year - 1L)
  tibble::tibble(year = years) %>%
    mutate(
      target_value = vapply(
        year,
        function(history_year) {
          .coal_annual_direct_value(
            annual, iso2, siec, unit, history_year, nrg_bal
          )
        },
        numeric(1)
      ),
      consumption_value = vapply(
        year,
        function(history_year) {
          .coal_annual_direct_value(
            annual, iso2, siec, unit, history_year, "GID_CAL"
          )
        },
        numeric(1)
      ),
      transformation_value = vapply(
        year,
        function(history_year) {
          .coal_unique_value(annual[
            annual$iso2 == iso2 & annual$siec == siec & annual$unit == unit &
              annual$nrg_bal == "TI_E" & annual_year == history_year,
            ,
            drop = FALSE
          ])
        },
        numeric(1)
      )
    ) %>%
    filter(!is.na(target_value), !is.na(consumption_value)) %>%
    slice_tail(n = 3)
}

.coal_stable_ratio <- function(numerator, denominator, tolerance = 0.10) {
  ratio <- numerator / denominator
  valid <- is.finite(ratio) & denominator > 0 & ratio >= 0 & ratio <= 1.10
  if (length(ratio) != 3 || !all(valid) || diff(range(ratio)) > tolerance) {
    return(NULL)
  }
  ratio
}

.coal_annual_anchor <- function(annual, iso2, siec, unit, year, nrg_bal) {
  direct <- .coal_annual_direct_value(annual, iso2, siec, unit, year, nrg_bal)
  if (!is.na(direct)) {
    return(list(value = direct, method = "reported_annual", input_years = year))
  }

  history <- .coal_annual_history(annual, iso2, siec, unit, year, nrg_bal)
  current_consumption <- .coal_annual_direct_value(
    annual, iso2, siec, unit, year, "GID_CAL"
  )

  if (nrg_bal == "TI_EHG_MAP") {
    annual_year <- if (".coal_year" %in% names(annual)) {
      annual$.coal_year
    } else {
      lubridate::year(annual$time)
    }
    current_transformation <- .coal_unique_value(annual[
      annual$iso2 == iso2 & annual$siec == siec & annual$unit == unit &
        annual$nrg_bal == "TI_E" & annual_year == year,
      ,
      drop = FALSE
    ])
    if (!is.na(current_transformation) && current_transformation == 0) {
      return(list(
        value = 0,
        method = "reported_zero_transformation_bound",
        input_years = year
      ))
    }
    accounting_error <- abs(history$target_value - history$transformation_value) /
      pmax(abs(history$transformation_value), 1)
    if (
      nrow(history) == 3 &&
        !is.na(current_transformation) &&
        all(is.finite(accounting_error)) &&
        all(accounting_error <= 0.05)
    ) {
      return(list(
        value = current_transformation,
        method = "transformation_identity",
        input_years = history$year
      ))
    }
  }

  ratio <- .coal_stable_ratio(history$target_value, history$consumption_value)
  if (!is.null(ratio) && !is.na(current_consumption)) {
    return(list(
      value = current_consumption * tail(ratio, 1),
      method = "previous_year_share",
      input_years = history$year
    ))
  }

  list(value = NA_real_, method = "unresolved", input_years = integer())
}

.coal_reported_values <- function(monthly) {
  provenance <- attr(monthly, "coal_gap_provenance")
  keys <- c(.coal_monthly_gap_keys(), "time")
  if (is.null(provenance) || nrow(provenance) == 0) {
    return(monthly %>%
      filter(nrg_bal %in% COAL_MONTHLY_GAP_BALANCES) %>%
      transmute(across(all_of(keys)), reported_value = values))
  }

  provenance %>%
    filter(method == "reported") %>%
    transmute(across(all_of(keys)), reported_value = original_value)
}

.coal_monthly_profile <- function(
  reported,
  iso2,
  siec,
  unit,
  year,
  nrg_bal,
  use_consumption_profile = FALSE
) {
  profile_balance <- if (use_consumption_profile) "GID_CAL" else nrg_bal
  reported_year <- if (".coal_year" %in% names(reported)) {
    reported$.coal_year
  } else {
    lubridate::year(reported$time)
  }
  get_share <- function(profile_year) {
    values <- reported[
      reported$iso2 == iso2 & reported$siec == siec & reported$unit == unit &
        reported$nrg_bal == profile_balance & reported_year == profile_year,
      ,
      drop = FALSE
    ]
    values <- values[order(values$time), , drop = FALSE]
    if (
      nrow(values) != 12 || any(is.na(values$reported_value)) ||
        sum(values$reported_value) <= 0
    ) {
      return(NULL)
    }
    values$reported_value / sum(values$reported_value)
  }

  previous <- get_share(year - 1L)
  if (!is.null(previous)) {
    return(list(
      shares = previous,
      method = if (use_consumption_profile) {
        "previous_year_consumption_profile"
      } else {
        "previous_year_profile"
      },
      input_years = year - 1L
    ))
  }

  trailing <- lapply(seq.int(year - 3L, year - 1L), get_share)
  if (length(trailing) == 3 && all(!vapply(trailing, is.null, logical(1)))) {
    return(list(
      shares = rowMeans(do.call(cbind, trailing)),
      method = if (use_consumption_profile) {
        "three_year_consumption_profile"
      } else {
        "three_year_profile"
      },
      input_years = seq.int(year - 3L, year - 1L)
    ))
  }

  NULL
}

.coal_power_uses_consumption_profile <- function(
  annual,
  iso2,
  siec,
  unit,
  year
) {
  history <- .coal_annual_history(annual, iso2, siec, unit, year, "TI_EHG_MAP")
  power_to_transformation <- abs(history$target_value - history$transformation_value) /
    pmax(abs(history$transformation_value), 1)
  power_to_consumption <- .coal_stable_ratio(
    history$target_value,
    history$consumption_value
  )
  all(is.finite(power_to_transformation)) &&
    all(power_to_transformation <= 0.05) &&
    !is.null(power_to_consumption) &&
    min(power_to_consumption) >= 0.80
}

.coal_add_or_replace_predictions <- function(monthly, predictions) {
  if (nrow(predictions) == 0) return(monthly)
  keys <- c(.coal_monthly_gap_keys(), "time")
  monthly_key <- .coal_monthly_key(monthly)
  prediction_key <- .coal_monthly_key(predictions)
  matched <- match(monthly_key, prediction_key)
  replace <- !is.na(matched)
  monthly$values[replace] <- predictions$filled_value[matched[replace]]

  new_predictions <- predictions[!prediction_key %in% monthly_key, , drop = FALSE]
  if (nrow(new_predictions) == 0) return(monthly)
  new_rows <- monthly[rep(NA_integer_, nrow(new_predictions)), , drop = FALSE]
  for (column in keys) new_rows[[column]] <- new_predictions[[column]]
  new_rows$values <- new_predictions$filled_value
  if ("freq" %in% names(new_rows)) new_rows$freq <- "M"
  if ("geo" %in% names(new_rows)) {
    geo_map <- monthly %>% filter(!is.na(geo)) %>% distinct(iso2, geo)
    new_rows$geo <- geo_map$geo[match(new_rows$iso2, geo_map$iso2)]
  }
  bind_rows(monthly, new_rows)
}

.coal_annual_fill_series <- function(
  monthly,
  annual,
  reported,
  iso2,
  siec,
  unit,
  year,
  nrg_bal,
  country_cutoff
) {
  year_dates <- seq(as.Date(paste0(year, "-01-01")), by = "month", length.out = 12)
  # Annual residuals always cover twelve months, including months beyond a
  # simulated extraction cutoff. Restrict returned predictions only afterwards.
  if (length(year_dates) == 0) return(tibble::tibble())

  existing <- monthly[
    monthly$iso2 == iso2 & monthly$siec == siec & monthly$unit == unit &
      monthly$nrg_bal == nrg_bal & monthly$time %in% year_dates,
    ,
    drop = FALSE
  ]
  values <- existing$values[match(year_dates, existing$time)]
  original <- reported$reported_value[match(
    do.call(paste, c(tibble::tibble(
      iso2 = iso2, siec = siec, nrg_bal = nrg_bal, unit = unit, time = year_dates
    ), sep = "\r")),
    .coal_monthly_key(reported)
  )]

  use_consumption_profile <- nrg_bal == "TI_EHG_MAP" &&
    .coal_power_uses_consumption_profile(annual, iso2, siec, unit, year)
  current_consumption <- .coal_annual_direct_value(
    annual, iso2, siec, unit, year, "GID_CAL"
  )
  consumption_positive <- any(monthly$values[
    monthly$iso2 == iso2 & monthly$siec == siec & monthly$nrg_bal == "GID_CAL" &
      monthly$unit == unit & monthly$time %in% year_dates
  ] > 0, na.rm = TRUE) || (!is.na(current_consumption) && current_consumption > 0)
  inconsistent_zero <- use_consumption_profile &&
    any(!is.na(original)) &&
    all(original[!is.na(original)] == 0) &&
    consumption_positive
  replaceable <- is.na(values) | (inconsistent_zero & !is.na(original) & original == 0)
  if (!any(replaceable)) return(tibble::tibble())
  anchor <- .coal_annual_anchor(annual, iso2, siec, unit, year, nrg_bal)
  exact_power_anchor <- nrg_bal == "TI_EHG_MAP" &&
    anchor$method %in% c("reported_annual", "reported_zero_transformation_bound")
  if (
    !.coal_annual_policy_enabled(siec, nrg_bal, sum(replaceable)) &&
      !exact_power_anchor
  ) {
    return(tibble::tibble())
  }

  if (is.na(anchor$value) && nrg_bal %in% c("TI_EHG_MAP", "TI_CO")) {
    history <- .coal_annual_history(annual, iso2, siec, unit, year, nrg_bal)
    ratio <- .coal_stable_ratio(history$target_value, history$consumption_value)
    reported_consumption <- reported[
      reported$iso2 == iso2 & reported$siec == siec & reported$unit == unit &
        reported$nrg_bal == "GID_CAL" & reported$time %in% year_dates,
      ,
      drop = FALSE
    ]
    consumption <- reported_consumption$reported_value[
      match(year_dates, reported_consumption$time)
    ]
    if (!is.null(ratio) && all(!is.na(consumption[replaceable]))) {
      return(tibble::tibble(
        iso2 = iso2,
        siec = siec,
        nrg_bal = nrg_bal,
        unit = unit,
        time = year_dates[replaceable],
        original_value = original[replaceable],
        filled_value = pmax(0, consumption[replaceable] * tail(ratio, 1)),
        replace_existing = !is.na(values[replaceable]),
        component_status = if_else(
          !is.na(original[replaceable]), "reported_inconsistent", "missing"
        ),
        annual_method = "previous_year_share_unconstrained",
        annual_input_years = paste(history$year, collapse = ","),
        profile_method = "same_month_reported_consumption",
        profile_input_years = as.character(year)
      ))
    }
  }
  if (is.na(anchor$value)) return(tibble::tibble())
  profile <- .coal_monthly_profile(
    reported, iso2, siec, unit, year, nrg_bal, use_consumption_profile
  )
  if (anchor$value == 0) {
    profile <- list(shares = rep(1 / 12, 12), method = "zero_anchor", input_years = year)
  }
  if (is.null(profile)) return(tibble::tibble())

  fixed_total <- sum(values[!replaceable], na.rm = TRUE)
  remainder <- anchor$value - fixed_total
  if (!is.finite(remainder) || remainder < -1e-6) return(tibble::tibble())
  month_numbers <- lubridate::month(year_dates)
  missing_weights <- profile$shares[month_numbers][replaceable]
  if (sum(missing_weights) <= 0) return(tibble::tibble())
  prediction <- pmax(0, remainder) * missing_weights / sum(missing_weights)

  tibble::tibble(
    iso2 = iso2,
    siec = siec,
    nrg_bal = nrg_bal,
    unit = unit,
    time = year_dates[replaceable],
    original_value = original[replaceable],
    filled_value = prediction,
    replace_existing = !is.na(values[replaceable]),
    component_status = if_else(
      !is.na(original[replaceable]), "reported_inconsistent", "missing"
    ),
    annual_method = anchor$method,
    annual_input_years = paste(anchor$input_years, collapse = ","),
    profile_method = profile$method,
    profile_input_years = paste(profile$input_years, collapse = ",")
  ) %>% filter(time <= country_cutoff)
}

.coal_complete_required_rows <- function(monthly, annual, country_coverage) {
  systems <- annual %>%
    filter(
      iso2 %in% get_eu_iso2s(include_eu = FALSE),
      siec %in% COAL_MONTHLY_GAP_FUELS,
      unit == "THS_T",
      nrg_bal == "IC_CAL",
      !is.na(values),
      values > 0
    ) %>%
    transmute(iso2, siec, unit, year = lubridate::year(time)) %>%
    bind_rows(
      monthly %>%
        filter(
          iso2 %in% get_eu_iso2s(include_eu = FALSE),
          siec %in% COAL_MONTHLY_GAP_FUELS,
          unit == "THS_T",
          nrg_bal == "GID_CAL",
          !is.na(values),
          values > 0
        ) %>%
        transmute(iso2, siec, unit, year = lubridate::year(time))
    ) %>%
    distinct() %>%
    inner_join(country_coverage, by = "iso2") %>%
    filter(
      year >= 2014,
      as.Date(paste0(year, "-01-01")) >= lubridate::floor_date(country_start, "year")
    )

  # No eligible calendar year remains when all monthly history is masked.
  # Annual energy allocation handles this system after raw reconstruction.
  if (nrow(systems) == 0L) return(monthly)
  required <- systems %>%
    rowwise() %>%
    reframe(
      iso2 = iso2,
      siec = siec,
      unit = unit,
      time = seq(as.Date(paste0(year, "-01-01")), by = "month", length.out = 12),
      country_cutoff = country_cutoff
    ) %>%
    ungroup() %>%
    filter(time <= country_cutoff) %>%
    tidyr::crossing(nrg_bal = COAL_MONTHLY_GAP_BALANCES) %>%
    filter(nrg_bal != "TI_CO" | siec == SIEC_HARD_COAL)
  keys <- c(.coal_monthly_gap_keys(), "time")
  missing_rows <- required %>%
    anti_join(monthly %>% distinct(across(all_of(keys))), by = keys) %>%
    mutate(values = NA_real_) %>%
    select(all_of(keys), values)
  if (nrow(missing_rows) == 0) return(monthly)

  new_rows <- monthly[rep(NA_integer_, nrow(missing_rows)), , drop = FALSE]
  for (column in keys) new_rows[[column]] <- missing_rows[[column]]
  new_rows$values <- NA_real_
  if ("freq" %in% names(new_rows)) new_rows$freq <- "M"
  if ("geo" %in% names(new_rows)) {
    geo_map <- monthly %>% filter(!is.na(geo)) %>% distinct(iso2, geo)
    new_rows$geo <- geo_map$geo[match(new_rows$iso2, geo_map$iso2)]
  }
  bind_rows(monthly, new_rows)
}

.coal_component_diagnostics <- function(monthly, provenance) {
  values <- monthly %>%
    filter(
      iso2 %in% get_eu_iso2s(include_eu = FALSE),
      siec %in% COAL_MONTHLY_GAP_FUELS,
      nrg_bal %in% COAL_MONTHLY_GAP_BALANCES
    ) %>%
    select(iso2, siec, nrg_bal, unit, time, values) %>%
    distinct()
  wide <- values %>%
    pivot_wider(names_from = nrg_bal, values_from = values) %>%
    add_missing_cols(COAL_MONTHLY_GAP_BALANCES)
  completeness <- wide %>%
    transmute(
      iso2, siec, unit, time,
      total_complete = !is.na(GID_CAL) & (siec != SIEC_HARD_COAL | !is.na(TI_CO)),
      electricity_complete = !is.na(TI_EHG_MAP),
      others_complete = total_complete & electricity_complete,
      adjusted_total = if_else(
        total_complete,
        GID_CAL - if_else(siec == SIEC_HARD_COAL, 1 - HARDCOAL_COKING_RATE_FACTOR, 0) *
          coalesce(TI_CO, 0),
        NA_real_
      )
    )
  unallocated <- completeness %>%
    filter(total_complete, !electricity_complete) %>%
    transmute(
      iso2, siec, unit, time, adjusted_total,
      reason = "electricity_component_unresolved"
    )
  list(completeness = completeness, unallocated = unallocated, provenance = provenance)
}

.coal_detect_eu_omissions <- function(original, filled, annual_provenance) {
  if (nrow(annual_provenance) == 0) return(tibble::tibble())
  keys <- c("siec", "nrg_bal", "unit", "time")
  contributors <- annual_provenance %>%
    filter(nrg_bal == "GID_CAL", is.na(original_value), !is.na(filled_value)) %>%
    select(contributor_iso2 = iso2, all_of(keys), contribution = filled_value)
  if (nrow(contributors) == 0) return(tibble::tibble())

  provenance <- attr(original, "coal_gap_provenance")
  if (!is.null(provenance) && nrow(provenance) > 0) {
    original <- original %>% left_join(
      provenance %>% select(all_of(c(.coal_monthly_gap_keys(), "time")), original_value),
      by = c(.coal_monthly_gap_keys(), "time")
    ) %>% mutate(values = if_else(iso2 %in% get_eu_iso2s(), original_value, values)) %>%
      select(-original_value)
  }
  eu <- original %>%
    filter(iso2 == "EU") %>%
    select(all_of(keys), eu_value = values)
  country_sum <- original %>%
    filter(iso2 %in% get_eu_iso2s(include_eu = FALSE)) %>%
    group_by(across(all_of(keys))) %>%
    summarise(reported_country_sum = sum(values, na.rm = TRUE), .groups = "drop")

  contributors %>%
    left_join(eu, by = keys) %>%
    left_join(country_sum, by = keys) %>%
    mutate(
      source_difference = eu_value - reported_country_sum,
      tolerance = pmax(0.001, abs(eu_value) * 1e-6),
      verified = !is.na(eu_value) & abs(source_difference) <= tolerance
    )
}

#' Fill long monthly coal gaps from reported annual balances
#'
#' @keywords internal
fill_raw_coal_annual_backed <- function(monthly, annual) {
  short_diagnostics <- lapply(
    c(
      "coal_gap_provenance", "coal_gap_reconciliation",
      "coal_gap_completeness", "coal_gap_exclusions"
    ),
    function(name) attr(monthly, name)
  )
  names(short_diagnostics) <- c(
    "coal_gap_provenance", "coal_gap_reconciliation",
    "coal_gap_completeness", "coal_gap_exclusions"
  )
  original <- monthly
  coal_monthly <- monthly %>%
    filter(
      iso2 %in% get_eu_iso2s(include_eu = TRUE),
      siec %in% COAL_MONTHLY_GAP_FUELS,
      unit == "THS_T"
    )
  annual <- annual %>%
    filter(
      iso2 %in% get_eu_iso2s(include_eu = FALSE),
      siec %in% COAL_MONTHLY_GAP_FUELS,
      unit == "THS_T",
      nrg_bal %in% c(
        "IC_CAL", "TI_CO_E", "TI_E", COAL_ANNUAL_POWER_BALANCES
      )
    ) %>%
    mutate(.coal_year = lubridate::year(time))
  reported <- .coal_reported_values(coal_monthly)
  reported$.coal_year <- lubridate::year(reported$time)
  covered <- coal_monthly %>%
    filter(
      iso2 %in% get_eu_iso2s(include_eu = FALSE),
      siec %in% COAL_MONTHLY_GAP_FUELS,
      !is.na(values)
    )
  if (nrow(covered) == 0) return(monthly)
  country_coverage <- covered %>%
    group_by(iso2) %>%
    summarise(
      country_start = min(time),
      country_cutoff = max(time),
      .groups = "drop"
    )
  # An annual-only extract has no monthly cutoff. Its energy totals are
  # allocated by the annual energy path, rather than invented as monthly GID.
  if (nrow(country_coverage) == 0) return(monthly)
  systems <- annual %>%
    filter(
      iso2 %in% get_eu_iso2s(include_eu = FALSE),
      siec %in% COAL_MONTHLY_GAP_FUELS,
      unit == "THS_T",
      nrg_bal == "IC_CAL",
      !is.na(values),
      values > 0
    ) %>%
    transmute(iso2, siec, unit, year = lubridate::year(time)) %>%
    bind_rows(
      coal_monthly %>%
        filter(
          iso2 %in% get_eu_iso2s(include_eu = FALSE),
          siec %in% COAL_MONTHLY_GAP_FUELS,
          unit == "THS_T",
          nrg_bal == "GID_CAL",
          !is.na(values),
          values > 0
        ) %>%
        transmute(iso2, siec, unit, year = lubridate::year(time))
    ) %>%
    distinct() %>%
    inner_join(country_coverage, by = "iso2") %>%
    filter(
      year >= 2014,
      as.Date(paste0(year, "-01-01")) <= country_cutoff,
      as.Date(paste0(year, "-12-01")) >= country_start
    )

  if (nrow(systems) == 0L) return(monthly)
  expected <- systems %>%
    rowwise() %>%
    reframe(
      iso2 = iso2,
      siec = siec,
      unit = unit,
      year = year,
      country_cutoff = country_cutoff,
      time = seq(as.Date(paste0(year, "-01-01")), by = "month", length.out = 12)
    ) %>%
    ungroup() %>%
    filter(time <= country_cutoff) %>%
    tidyr::crossing(nrg_bal = COAL_MONTHLY_GAP_BALANCES) %>%
    filter(nrg_bal != "TI_CO" | siec == SIEC_HARD_COAL)
  observed <- coal_monthly %>%
    filter(nrg_bal %in% COAL_MONTHLY_GAP_BALANCES) %>%
    select(iso2, siec, unit, time, nrg_bal, values) %>%
    distinct()
  tasks <- expected %>%
    left_join(observed, by = c("iso2", "siec", "unit", "time", "nrg_bal")) %>%
    group_by(iso2, siec, unit, year, country_cutoff, nrg_bal) %>%
    summarise(
      has_missing = any(is.na(values)),
      missing_months = sum(is.na(values)),
      all_reported_zero = all(!is.na(values)) && all(values == 0),
      .groups = "drop"
    ) %>%
    filter(
      has_missing | (nrg_bal == "TI_EHG_MAP" & all_reported_zero)
    ) %>%
    rowwise() %>%
    filter(
      .coal_annual_policy_enabled(siec, nrg_bal, missing_months) |
        nrg_bal == "TI_EHG_MAP"
    ) %>%
    ungroup()

  predictions <- bind_rows(lapply(seq_len(nrow(tasks)), function(index) {
    task <- tasks[index, ]
    .coal_annual_fill_series(
      coal_monthly, annual, reported,
      task$iso2, task$siec, task$unit, task$year, task$nrg_bal,
      task$country_cutoff
    )
  }))
  monthly <- .coal_add_or_replace_predictions(monthly, predictions)
  monthly <- .coal_complete_required_rows(monthly, annual, country_coverage)
  diagnostics <- .coal_component_diagnostics(monthly, predictions)
  repairs <- .coal_detect_eu_omissions(original, monthly, predictions)

  attr(monthly, "coal_annual_provenance") <- diagnostics$provenance
  attr(monthly, "coal_component_completeness") <- diagnostics$completeness
  attr(monthly, "coal_unallocated_sector") <- diagnostics$unallocated
  attr(monthly, "coal_eu_repair_candidates") <- repairs
  attr(monthly, "coal_annual_policy") <- .coal_annual_policy_table()
  for (name in names(short_diagnostics)) {
    attr(monthly, name) <- short_diagnostics[[name]]
  }
  monthly
}

apply_verified_coal_eu_repairs <- function(data, candidates) {
  prior_repairs <- attr(data, "coal_eu_emissions_repairs")
  data <- ungroup(data)
  if (is.null(candidates) || nrow(candidates) == 0 || !any(candidates$verified)) {
    attr(data, "coal_eu_emissions_repairs") <- tibble::tibble()
    return(data)
  }
  keys <- c("siec", "time", "unit")
  verified <- candidates %>%
    filter(verified) %>%
    distinct(contributor_iso2, across(all_of(keys)))
  if (!is.null(prior_repairs) && nrow(prior_repairs) > 0) {
    # A replay of the same verified contribution must be idempotent.
    already_applied <- prior_repairs %>% filter(applied) %>%
      select(all_of(keys), contributors) %>%
      tidyr::separate_rows(contributors, sep = ",") %>%
      rename(contributor_iso2 = contributors) %>% distinct()
    verified <- verified %>% anti_join(already_applied, by = c("contributor_iso2", keys))
  }
  if (nrow(verified) == 0) return(data)
  contributions <- data %>%
    inner_join(
      verified,
      by = c("iso2" = "contributor_iso2", "siec", "time", "unit")
    ) %>%
    group_by(siec, time, unit, fuel, sector) %>%
    summarise(
      contributor_count = n_distinct(iso2),
      contribution_co2_tonne = sum_or_na(value_co2_tonne),
      contributors = paste(sort(unique(iso2)), collapse = ","),
      .groups = "drop"
    )
  if (nrow(contributions) == 0) {
    attr(data, "coal_eu_emissions_repairs") <- tibble::tibble()
    return(data)
  }

  repair_keys <- c("siec", "time", "unit", "fuel", "sector")
  eu_rows <- data %>%
    filter(iso2 == "EU") %>%
    select(all_of(repair_keys), eu_value_before = value_co2_tonne) %>%
    inner_join(contributions, by = repair_keys) %>%
    mutate(
      applied = !is.na(eu_value_before) & !is.na(contribution_co2_tonne),
      eu_value_after = if_else(
        applied,
        eu_value_before + contribution_co2_tonne,
        eu_value_before
      )
    )
  applied <- eu_rows %>% filter(applied)
  if (nrow(applied) > 0) {
    data <- data %>%
      left_join(
        applied %>% select(all_of(repair_keys), eu_value_after),
        by = repair_keys
      ) %>%
      mutate(
        value_co2_tonne = if_else(
          iso2 == "EU" & !is.na(eu_value_after),
          eu_value_after,
          value_co2_tonne
        )
      ) %>%
      select(-eu_value_after)
  }
  attr(data, "coal_eu_emissions_repairs") <- bind_rows(prior_repairs, eu_rows)
  data
}

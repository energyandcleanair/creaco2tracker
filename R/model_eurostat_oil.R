#' Process oil data from EUROSTAT
#'
#' @param x Raw EUROSTAT data
#' @return Processed oil consumption data
#' @export
process_oil <- function(x) {
  # Define attribution matrices for different operations

  # 1. Initial filtering matrix - which combinations to keep
  filter_matrix <- tribble(
    ~siec, ~nrg_bal, ~keep,
    # Oil products: SECTOR_TOTAL
    SIEC_OIL_PRODUCTS, "GID_OBS", TRUE,
    SIEC_OIL_PRODUCTS, "GID_NE", TRUE,
    SIEC_FUEL_OIL, "GID_OBS", TRUE,
    SIEC_FUEL_OIL, "GID_NE", TRUE,
    SIEC_HEATING_GASOIL, "GID_OBS", TRUE,
    SIEC_HEATING_GASOIL, "GID_NE", TRUE,
    SIEC_BIOGASOLINE, "GID_OBS", TRUE,
    SIEC_BIOGASOLINE, "GID_NE", TRUE,
    SIEC_BIODIESEL, "GID_OBS", TRUE,
    SIEC_BIODIESEL, "GID_NE", TRUE,

    # SECTOR_TRANSPORT: Road diesel and gasoline
    SIEC_ROAD_DIESEL, "FC_TRA_E", TRUE,
    SIEC_MOTOR_GASOLINE_XBIO, "FC_TRA_E", TRUE,
    SIEC_BIODIESEL, "FC_TRA_E", TRUE,

    # SECTOR_TRANSPORT: Aviation
    SIEC_KEROSENE_XBIO, "FC_TRA_DAVI_E", TRUE,
    SIEC_KEROSENE_XBIO, "INTAVI_E", TRUE,
    SIEC_AVIATION_GASOLINE, "FC_TRA_DAVI_E", TRUE,
    SIEC_AVIATION_GASOLINE, "INTAVI_E", TRUE,

    # SECTOR_ELEC
    # Monthly has TI_EHG_MAP
    SIEC_OIL_PRODUCTS, "TI_EHG_MAP", TRUE,
    SIEC_FUEL_OIL, "TI_EHG_MAP", TRUE,
    SIEC_HEATING_GASOIL, "TI_EHG_MAP", TRUE,

    # Yealry has TI_EHG_MAPE and TI_EHG_MAPCHP
    SIEC_OIL_PRODUCTS, "TI_EHG_MAPE", TRUE,
    SIEC_FUEL_OIL, "TI_EHG_MAPE", TRUE,
    SIEC_HEATING_GASOIL, "TI_EHG_MAPE", TRUE,
    SIEC_OIL_PRODUCTS, "TI_EHG_MAPCHP", TRUE,
    SIEC_FUEL_OIL, "TI_EHG_MAPCHP", TRUE,
    SIEC_HEATING_GASOIL, "TI_EHG_MAPCHP", TRUE,
  )

  # 2. Deduction rules - which fuels to subtract from which base fuels
  deduction_rules <- tribble(
    ~base_siec_code, ~nrg_bal, ~deduct_siec_codes,
    SIEC_OIL_PRODUCTS, "GID_OBS", list(
      c(
        SIEC_FUEL_OIL, SIEC_HEATING_GASOIL, SIEC_BIOGASOLINE,
        SIEC_BIODIESEL
      )
    ),
    SIEC_OIL_PRODUCTS, "GID_NE", list(
      c(
        SIEC_FUEL_OIL, SIEC_HEATING_GASOIL, SIEC_BIOGASOLINE,
        SIEC_BIODIESEL
      )
    ),
    SIEC_ROAD_DIESEL, "FC_TRA_E", list(c(SIEC_BIODIESEL)),
    SIEC_OIL_PRODUCTS, "TI_EHG_MAP", list(c(SIEC_FUEL_OIL, SIEC_HEATING_GASOIL)),
    SIEC_OIL_PRODUCTS, "TI_EHG_MAPE", list(c(SIEC_FUEL_OIL, SIEC_HEATING_GASOIL)),
    SIEC_OIL_PRODUCTS, "TI_EHG_MAPCHP", list(c(SIEC_FUEL_OIL, SIEC_HEATING_GASOIL))
  )

  # Fuels to exclude after deduction
  SIEC_CODES_TO_EXCLUDE <- c(SIEC_BIOGASOLINE, SIEC_BIODIESEL)

  # 3. Factor matrix - which factors to apply based on nrg_bal
  factor_matrix <- tribble(
    ~nrg_bal, ~factor, ~sector,
    "GID_NE", -1, SECTOR_ALL,
    "GID_OBS", 1, SECTOR_ALL,
    "FC_TRA_E", 1, SECTOR_TRANSPORT_DOMESTIC,
    "FC_TRA_DAVI_E", 1, SECTOR_TRANSPORT_DOMESTIC,
    "INTAVI_E", 1, SECTOR_TRANSPORT_INTERNATIONAL_AVIATION,
    "TI_EHG_MAP", 1, SECTOR_ELEC,
    "TI_EHG_MAPE", 1, SECTOR_ELEC,
    "TI_EHG_MAPCHP", 1, SECTOR_ELEC
  )


  # Only keep meaningful columns to avoid confusion
  siecs <- x %>%
    ungroup() %>%
    distinct(siec)

  x <- x %>%
    add_iso2() %>%
    select(siec, nrg_bal, iso2, time, values, unit)

  complete_oil_all_keys <- x %>%
    filter(
      siec == SIEC_OIL_PRODUCTS,
      nrg_bal == "GID_OBS",
      !is.na(values)
    ) %>%
    distinct(iso2, time, unit)

  # Step 1: Filter the data based on the filter matrix
  x_filtered <- x %>%
    inner_join(filter_matrix %>% filter(keep), by = c("siec", "nrg_bal"))

  # Step 2: Apply deductions to avoid double counting using a cleaner approach
  x_deducted <- x_filtered %>%
    # Create a temporary ID for each group
    group_by(nrg_bal, unit, iso2, time) %>%
    mutate(group_id = dplyr::cur_group_id()) %>%
    ungroup()

  # For each deduction rule, calculate the deduction amount
  deductions <- map_df(seq_len(nrow(deduction_rules)), function(i) {
    rule <- deduction_rules[i, ]

    # Get the data for this base fuel
    base_data <- x_deducted %>%
      filter(
        siec == rule$base_siec_code,
        nrg_bal == rule$nrg_bal
      )

    # Calculate deductions for each group
    if (nrow(base_data) > 0) {
      deduct_amounts <- x_deducted %>%
        filter(
          siec %in% unlist(rule$deduct_siec_codes),
          nrg_bal == rule$nrg_bal,
          group_id %in% base_data$group_id
        ) %>%
        group_by(group_id) %>%
        summarise(deduction = sum_or_na(values), .groups = "drop")

      # Join with base data
      base_data %>%
        left_join(deduct_amounts, by = "group_id") %>%
        mutate(
          deduction = replace_na(deduction, 0),
          new_value = values - deduction
        ) %>%
        select(group_id, siec, new_value)
    } else {
      tibble(group_id = integer(0), siec = character(0), new_value = numeric(0))
    }
  })

  # Apply the deductions
  x_deducted <- x_deducted %>%
    left_join(deductions, by = c("group_id", "siec")) %>%
    mutate(values = coalesce(new_value, values)) %>%
    select(-c(group_id, new_value)) %>%
    # Remove biofuels after deduction using the constant defined at the top
    filter(!siec %in% SIEC_CODES_TO_EXCLUDE)

  # Step 3: Apply factors and assign sectors
  x_processed <- x_deducted %>%
    left_join(factor_matrix, by = "nrg_bal") %>%
    mutate(
      values = values * factor
    ) %>%
    select(-factor) %>%
    arrange(desc(time)) %>%
    {
      bind_rows(
        filter(., sector != SECTOR_ALL),
        filter(., sector == SECTOR_ALL) %>%
          dplyr::semi_join(complete_oil_all_keys, by = c("iso2", "time", "unit"))
      )
    }

  # Apply gap filling
  x_processed <- fill_gaps_in_time_series(
    data = x_processed,
    group_cols = c("iso2", "siec", "nrg_bal", "sector", "unit"),
    zero_consecutive_required = 3,
    zero_consecutive_required_beyond_last = 12,
    exclude_iso2s = "EU"
  )

  # Manual gap filling for Estonia with more aggressive parameters
  # It is missing one month data in 2024-10-01
  x_processed <- x_processed %>%
    fill_gaps_in_time_series(
      group_cols = c("iso2", "siec", "nrg_bal", "sector", "unit"),
      zero_consecutive_required = 2,
      zero_consecutive_required_beyond_last = 6,
      interp_cv_threshold = 3,
      interp_maxgap = 6,
      exclude_iso2s = setdiff(unique(x_processed$iso2), "EE")
    )

  # Fill EU values from country sums
  x_processed <- fill_eu_from_countries_sum(
    data = x_processed,
    group_cols = c("sector", "siec", "time", "nrg_bal", "unit"),
    min_countries = 25,
    max_rel_diff = 0.05
  ) %>%
    filter(!is.na(values)) %>%
    # After validation, we find that transport data is only correct from ~2010
    filter(
      sector == SECTOR_ALL | time >= "2010-01-01"
    )

  # Remove EU rows that are based on incomplete data
  x_processed <- x_processed %>%
    fix_eu_when_important_countries_missing()

  # Check that we have all siecs per sectors
  check_eurostat_oil_completeness(x_processed)

  # Aggregate
  result <- x_processed %>%
    mutate(fuel = FUEL_OIL) %>%
    group_by(iso2, time, fuel, unit, siec, sector) %>%
    summarise(values = sum_or_na(values), .groups = "drop") %>%
    ungroup()

  return(result)
}


check_eurostat_oil_completeness <- function(x_processed) {
  sector_counts <- x_processed %>%
    distinct(siec, nrg_bal, sector) %>%
    count(sector) %>%
    spread(sector, n, fill = 0)

  stopifnot(
    "Fix oil" = sector_counts[SECTOR_ALL] == 6,
    "Fix oil" = sector_counts[SECTOR_TRANSPORT_DOMESTIC] == 4,
    "Fix oil" = sector_counts[SECTOR_TRANSPORT_INTERNATIONAL_AVIATION] == 2,
    # 3 for monthly, 6 for yearly
    "Fix oil" = sector_counts[SECTOR_ELEC] %in% c(3, 6)
  )
}


#' Process monthly oil data
#' @param x Raw monthly oil data
#' @return Processed monthly oil consumption
#' @export
process_oil_monthly <- function(x) {
  process_oil(x)
}

#' Process yearly oil data
#' @param x Raw yearly oil data
#' @return Processed yearly oil consumption
#' @export
process_oil_yearly <- function(x) {
  process_oil(x)
}


#' Monthly data is not as detailed as yearly one.
#' We make and test some assumptions to fill the gaps (see in code)
#'
#'
#'
#' @param cons_yearly_raw
#' @param cons_monthly_raw
#'
#' @return
#' @export
#'
#' @examples
add_oil_transport <- function(monthly, yearly, plot_validation = FALSE) {
  eu_iso2s <- get_eu_iso2s(include_eu = TRUE)
  filter_plot <- function(x) {
    x %>%
      add_iso2() %>%
      filter(iso2 %in% eu_iso2s) %>%
      filter(time >= "2010-01-01")
  }
  stopifnot(all(yearly$unit == EUROSTAT_UNIT_THOUSAND_TONNES))
  stopifnot(all(monthly$unit == EUROSTAT_UNIT_THOUSAND_TONNES))

  project_shares <- function(x) {
    x %>%
      ungroup() %>%
      tidyr::complete(time = unique(monthly$time), geo, siec) %>%
      group_by(geo, siec) %>%
      arrange(time) %>%
      tidyr::fill(share, .direction = c("updown")) %>%
      ungroup()
  }

  # Visually validate computation
  validate_computation <- function(computed_monthly) {
    computed_monthly %>%
      group_by(time = floor_date(time, "year"), geo, nrg_bal, siec) %>%
      summarise(
        values_monthly = sum(values, na.rm = TRUE),
        n = n()
      ) %>%
      filter(n == 12) %>%
      # split and unnest nrg_bal
      mutate(nrg_bal = strsplit(nrg_bal, "\\+")) %>%
      unnest(nrg_bal) %>%
      # Prevent double counting
      group_by(time, geo, siec) %>%
      mutate(values_monthly = values_monthly / n()) %>%
      left_join(
        yearly %>%
          rename(values_yearly = values)
      ) %>%
      ungroup() %>%
      group_by(time, geo) %>%
      summarise(
        values_monthly = sum(values_monthly, na.rm = FALSE),
        values_yearly = sum(values_yearly, na.rm = FALSE)
      ) %>%
      gather(key, values, -time, -geo) %>%
      ungroup() %>%
      filter_plot() %>%
      ggplot() +
      geom_line(aes(time, values, col = key)) +
      facet_wrap(~geo, scales = "free_y") +
      rcrea::scale_y_zero()
  }


  # Assumption 1: Constant share of motor gasoline (that doesn't go to petro industry or
  # electricity) goes to road energy use
  # Compute share
  share_motor_gasoline_road <- yearly %>%
    filter(
      siec == SIEC_MOTOR_GASOLINE_XBIO,
      nrg_bal %in% c("GID_OBS", "FC_TRA_ROAD_E", "GID_NE")
    ) %>%
    group_by(geo, time, siec, nrg_bal) %>%
    summarise(values = sum(values, na.rm = TRUE)) %>%
    spread(nrg_bal, values) %>%
    group_by(geo, time, siec) %>%
    filter(
      FC_TRA_ROAD_E > 0
    ) %>%
    summarise(share = FC_TRA_ROAD_E / (GID_OBS - GID_NE))

  # Visually validate assumption
  if (plot_validation) {
    ggplot(share_motor_gasoline_road %>% project_shares() %>% filter_plot()) +
      geom_line(aes(time, share)) +
      facet_wrap(~geo) +
      rcrea::scale_y_zero()
  }

  # Compute new data
  monthly_tra_road_e_gasoline <- share_motor_gasoline_road %>%
    project_shares() %>%
    right_join(
      monthly %>%
        filter(
          siec == SIEC_MOTOR_GASOLINE_XBIO,
          nrg_bal %in% c("GID_OBS", "GID_NE")
        ) %>%
        mutate(
          factor = case_when(
            nrg_bal == "GID_NE" ~ -1,
            T ~ 1
          )
        ) %>%
        group_by(geo, time, siec) %>%
        summarise(values = sum(values * factor, na.rm = TRUE)),
      by = c("geo", "time", "siec")
    ) %>%
    mutate(
      values = values * share,
      nrg_bal = "FC_TRA_ROAD_E"
    ) %>%
    select(-c(share)) %>%
    ungroup()

  if (plot_validation) {
    validate_computation(monthly_tra_road_e_gasoline)
  }


  # Assumption 2: Constant share of road diesel goes to road energy use
  share_road_diesel_road <- yearly %>%
    filter(
      siec == SIEC_ROAD_DIESEL,
      nrg_bal %in% c("GID_OBS", "FC_TRA_ROAD_E")
    ) %>%
    group_by(geo, time, nrg_bal, siec) %>%
    summarise(values = sum(values, na.rm = TRUE)) %>%
    spread(nrg_bal, values) %>%
    filter(
      GID_OBS > 0,
      FC_TRA_ROAD_E > 0
    ) %>%
    group_by(geo, time, siec) %>%
    summarise(share = FC_TRA_ROAD_E / (GID_OBS))

  if (plot_validation) {
    ggplot(share_road_diesel_road %>% filter_plot()) +
      geom_line(aes(time, share)) +
      facet_wrap(~geo) +
      rcrea::scale_y_zero()
  }

  # Compute new data
  monthly_tra_road_e_diesel <- share_road_diesel_road %>%
    project_shares() %>%
    right_join(
      monthly %>%
        filter(
          siec == SIEC_ROAD_DIESEL,
          nrg_bal %in% c("GID_OBS")
        ) %>%
        group_by(geo, time, siec) %>%
        summarise(values = sum(values, na.rm = TRUE)),
      by = c("geo", "time", "siec")
    ) %>%
    mutate(
      values = values * share,
      nrg_bal = "FC_TRA_ROAD_E"
    ) %>%
    select(-c(share)) %>%
    ungroup()

  # Visually validate computation
  if (plot_validation) {
    validate_computation(monthly_tra_road_e_diesel)
  }

  # Assumption 2bis: Constant share of road diesel goes to transportation energy use
  share_road_diesel_transport <- yearly %>%
    filter(
      siec == SIEC_ROAD_DIESEL,
      nrg_bal %in% c("GID_OBS", "FC_TRA_E")
    ) %>%
    group_by(geo, time, nrg_bal, siec) %>%
    summarise(values = sum(values, na.rm = TRUE)) %>%
    spread(nrg_bal, values) %>%
    filter(
      GID_OBS > 0,
      FC_TRA_E > 0
    ) %>%
    group_by(geo, time, siec) %>%
    summarise(share = FC_TRA_E / (GID_OBS))

  if (plot_validation) {
    ggplot(share_road_diesel_transport %>% filter_plot()) +
      geom_line(aes(time, share)) +
      facet_wrap(~geo) +
      rcrea::scale_y_zero()
  }

  # Compute new data
  monthly_tra_e_diesel <- share_road_diesel_transport %>%
    project_shares() %>%
    right_join(
      monthly %>%
        filter(
          siec == SIEC_ROAD_DIESEL,
          nrg_bal %in% c("GID_OBS")
        ) %>%
        select(geo, time, siec, values),
      by = c("geo", "time", "siec")
    ) %>%
    mutate(
      values = values * share,
      nrg_bal = "FC_TRA_E"
    ) %>%
    select(-c(share)) %>%
    ungroup()

  # Visually validate computation
  if (plot_validation) {
    validate_computation(monthly_tra_e_diesel)
  }

  # Assumption 2ter: Constant share of motor gasoline goes to transport energy use
  share_motor_gasoline_tra <- yearly %>%
    filter(
      siec == SIEC_MOTOR_GASOLINE_XBIO,
      nrg_bal %in% c("GID_OBS", "FC_TRA_E", "GID_NE")
    ) %>%
    group_by(geo, time, siec, nrg_bal) %>%
    summarise(values = sum(values, na.rm = TRUE)) %>%
    spread(nrg_bal, values) %>%
    filter(
      GID_OBS > 0,
      FC_TRA_E > 0
    ) %>%
    group_by(geo, time, siec) %>%
    summarise(share = FC_TRA_E / (GID_OBS - GID_NE))

  # Visually validate assumption
  if (plot_validation) {
    ggplot(share_motor_gasoline_tra %>% filter_plot()) +
      geom_line(aes(time, share)) +
      facet_wrap(~geo) +
      rcrea::scale_y_zero()
  }

  # Compute new data
  monthly_tra_e_gasoline <- share_motor_gasoline_tra %>%
    project_shares() %>%
    right_join(
      monthly %>%
        filter(
          siec == SIEC_MOTOR_GASOLINE_XBIO,
          nrg_bal %in% c("GID_OBS", "GID_NE")
        ) %>%
        mutate(
          factor = case_when(
            nrg_bal == "GID_NE" ~ -1,
            T ~ 1
          )
        ) %>%
        group_by(geo, time, siec) %>%
        summarise(values = sum(values * factor, na.rm = TRUE)),
      by = c("geo", "time", "siec")
    ) %>%
    mutate(
      values = values * share,
      nrg_bal = "FC_TRA_E"
    ) %>%
    select(-c(share)) %>%
    ungroup()


  # Visually validate computation
  if (plot_validation) {
    validate_computation(monthly_tra_e_gasoline)
  }

  # Assumption 3: Road diesel and motor gasoline are the only fuels used in road transportation
  share_gasoline_diesel_road <- yearly %>%
    filter(
      siec %in% c(
        SIEC_OIL_PRODUCTS, SIEC_ROAD_DIESEL, SIEC_MOTOR_GASOLINE_XBIO,
        SIEC_BIOGASOLINE
      ),
      nrg_bal == "FC_TRA_ROAD_E"
    ) %>%
    group_by(geo, time, siec) %>%
    summarise(values = sum(values, na.rm = TRUE)) %>%
    spread(siec, values) %>%
    group_by(geo, time) %>%
    summarise(
      share_road = (
        !!sym(SIEC_ROAD_DIESEL) + !!sym(SIEC_MOTOR_GASOLINE_XBIO) +
          !!sym(SIEC_BIOGASOLINE)
      ) / !!sym(SIEC_OIL_PRODUCTS)
    )

  # TODO INVESTIGATE AND FIX BULGARIA
  if (plot_validation) {
    ggplot(share_gasoline_diesel_road %>% filter_plot()) +
      geom_line(aes(time, share_road)) +
      facet_wrap(~geo) +
      rcrea::scale_y_zero()
  }


  # This one doesn't need any computation
  # This is more to validate the assumption for computation later on

  # Assumption 4: Share of kerosene going to domestic aviation vs international aviation
  # is constant
  # Note: Monthly data has international aviation but not domestic
  share_kerosene_aviation <- yearly %>%
    filter(
      siec == SIEC_KEROSENE_XBIO,
      nrg_bal %in% c("INTAVI_E", "FC_TRA_DAVI_E")
    ) %>%
    group_by(geo, time, nrg_bal, siec) %>%
    summarise(values = sum(values, na.rm = TRUE)) %>%
    spread(nrg_bal, values) %>%
    filter(
      INTAVI_E > 0,
      FC_TRA_DAVI_E > 0
    ) %>%
    group_by(geo, time, siec) %>%
    summarise(share = FC_TRA_DAVI_E / INTAVI_E) %>%
    ungroup()

  if (plot_validation) {
    ggplot(share_kerosene_aviation %>% filter_plot()) +
      geom_line(aes(time, share)) +
      facet_wrap(~geo) +
      rcrea::scale_y_zero()
  }

  # Compute new data
  monthly_tra_aviation_kerosene <- share_kerosene_aviation %>%
    project_shares() %>%
    right_join(
      monthly %>%
        filter(
          siec == SIEC_KEROSENE_XBIO,
          nrg_bal %in% c("INTAVI_E")
        ) %>%
        select(geo, time, siec, values),
      by = c("geo", "time", "siec")
    ) %>%
    mutate(
      values = share * values,
      nrg_bal = "FC_TRA_DAVI_E"
    ) %>%
    select(-c(share)) %>%
    ungroup()


  # Visually validate computation
  if (plot_validation) {
    validate_computation(monthly_tra_aviation_kerosene)
  }

  # Assumption 5: Share of aviation gasoline going to aviation constant (and close to 1)
  share_aviation_aviation_gasoline <- yearly %>%
    filter(
      siec == SIEC_AVIATION_GASOLINE,
      nrg_bal %in% c("GID_OBS", "FC_TRA_DAVI_E")
    ) %>%
    group_by(geo, time, nrg_bal, siec) %>%
    summarise(values = sum(values, na.rm = TRUE)) %>%
    spread(nrg_bal, values) %>%
    filter(
      GID_OBS > 0,
      FC_TRA_DAVI_E > 0
    ) %>%
    group_by(geo, time, siec) %>%
    summarise(
      share = (FC_TRA_DAVI_E) / GID_OBS,
      .groups = "drop"
    )

  if (plot_validation) {
    ggplot(share_aviation_aviation_gasoline %>% filter_plot()) +
      geom_line(aes(time, share)) +
      facet_wrap(~geo) +
      rcrea::scale_y_zero()
  }

  # Compute new data
  monthly_tra_aviation_gasoline <- share_aviation_aviation_gasoline %>%
    project_shares() %>%
    right_join(
      monthly %>%
        filter(
          siec == SIEC_AVIATION_GASOLINE,
          nrg_bal %in% c("GID_OBS")
        ) %>%
        select(geo, time, siec, values),
      by = c("geo", "time", "siec")
    ) %>%
    mutate(
      values = values * share,
      nrg_bal = "FC_TRA_DAVI_E"
    ) %>%
    select(-c(share)) %>%
    ungroup()

  # Visually validate computation
  if (plot_validation) {
    validate_computation(monthly_tra_aviation_gasoline)
  }

  # Assumption 6: Motor gasoline, road diesel and kerosene represent most of the transport sector
  share_transport <- yearly %>%
    filter(
      siec %in% c(
        SIEC_MOTOR_GASOLINE_XBIO, SIEC_ROAD_DIESEL, SIEC_KEROSENE_XBIO,
        SIEC_BIOGASOLINE, SIEC_OIL_PRODUCTS
      ),
      nrg_bal == "FC_TRA_E"
    ) %>%
    group_by(geo, time, siec) %>%
    summarise(values = sum(values, na.rm = TRUE)) %>%
    spread(siec, values) %>%
    group_by(geo, time) %>%
    summarise(
      share_transport = (
        !!sym(SIEC_MOTOR_GASOLINE_XBIO) + !!sym(SIEC_ROAD_DIESEL) +
          !!sym(SIEC_KEROSENE_XBIO) + !!sym(SIEC_BIOGASOLINE)
      ) / !!sym(SIEC_OIL_PRODUCTS)
    )

  if (plot_validation) {
    ggplot(share_transport %>% filter_plot()) +
      geom_line(aes(time, share_transport)) +
      facet_wrap(~geo) +
      rcrea::scale_y_zero()
  }

  # Conclusion:
  # Transport will be split in Road, Aviation (domestic + international),
  # International Maritime and Others (domestic navigation, rail, pipeline etc.)
  # We will use the following shares:
  # - Road: Motor gasoline + Road diesel
  # - Aviation: all Kerosene
  # - International Maritime: using International Bunkers which is also available in monthly data
  # - Others: the rest based on FC_TRA_E - Road - Aviation - International Maritime
  #
  # If we don't want to divide inso sub sectors
  # We can do TRANSPORT = FC_TRA_E (Gasoline + Diesel) + INTAVI_E+FC_TRA_DAVI_E (Kerosene) + All
  # bunkers
  added <- bind_rows(
    monthly_tra_road_e_gasoline,
    monthly_tra_road_e_diesel,
    monthly_tra_aviation_kerosene,
    monthly_tra_aviation_gasoline,
    monthly_tra_e_diesel,
    monthly_tra_e_gasoline
  ) %>%
    select(time, geo, siec, values, nrg_bal) %>%
    mutate(unit = EUROSTAT_UNIT_THOUSAND_TONNES)

  # Re-add siec for NCV/emission factor matching
  # Though ultimately we should switch everything to siec constants only.

  monthly_filled <- bind_rows(
    monthly %>% anti_join(added %>% distinct(geo, time, siec, nrg_bal)),
    added
  )

  return(monthly_filled)
}


fill_oil_non_energy_use_yearly <- function(yearly) {
  # Austria, and maybe other countries have GD_PI but not GID_NE in yearly data.
  # We'll assume GID_NE=GD_PI for those
  yearly %>%
    group_by(siec, geo) %>%
    mutate(
      should_use = case_when(
        any(values[nrg_bal == "GID_NE"] > 0) ~ "GID_NE",
        any(values[nrg_bal == "GD_PI"] > 0) ~ "GD_PI",
        T ~ "GID_NE"
      )
    ) %>%
    filter(
      (nrg_bal %in% c("GID_NE", "GD_PI") & should_use == nrg_bal) |
        (!nrg_bal %in% c("GID_NE", "GD_PI"))
    ) %>%
    mutate(
      nrg_bal = recode(nrg_bal, "GD_PI" = "GID_NE")
    ) %>%
    ungroup()
}


#' In some rare instances, GID_OBS is 0/NA while it clearly shouldn't be
#' e.g. NL, Blended biodiesel, 2020, monthly data
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
fill_gid_obs_with_gid_cal <- function(x) {
  filler <- x %>%
    ungroup() %>%
    filter(nrg_bal == "GID_CAL") %>%
    mutate(
      nrg_bal = "GID_OBS"
    ) %>%
    rename(values_cal = values)

  # Join with all but values
  x %>%
    full_join(filler) %>%
    mutate(
      values =
        case_when(
          !is.na(values_cal) & is.na(values) ~ values_cal,
          !is.na(values_cal) & values == 0 ~ values_cal,
          T ~ values
        )
    )
}

#' Non energy use is not available in monthly data. We derive it from industrial GD_PI
#' i.e. gross delieveries to petrochemical industry
#'
#' @return
#' @export
#'
#' @examples
fill_oil_non_energy_use_monthly <- function(yearly, monthly) {
  gd_pi_to_gid_ne <- bind_rows(
    yearly,
    monthly
  ) %>%
    inner_join(
      tibble(
        freq = c(EUROSTAT_FREQ_ANNUAL, EUROSTAT_FREQ_MONTHLY),
        nrg_bal = c("GID_NE", "GD_PI")
      )
    ) %>%
    group_by(geo, siec, year = year(time), nrg_bal, freq) %>%
    summarise(
      values = sum(values, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    filter(n == 12 | freq == EUROSTAT_FREQ_ANNUAL) %>%
    filter(!is.na(values)) %>%
    select(-c(freq, n)) %>%
    spread(nrg_bal, values) %>%
    mutate(ratio = GID_NE / GD_PI) %>%
    filter(!is.na(ratio), !is.infinite(ratio), ratio > 0) %>%
    complete(
      nesting(geo, siec),
      year = seq(min(year(yearly$time)), max(year(monthly$time))),
      fill = list(ratio = NA)
    ) %>%
    # Fill up and down
    arrange(year) %>%
    group_by(geo, siec) %>%
    fill(ratio, .direction = "updown") %>%
    ungroup() %>%
    select(geo, siec, year, ratio)


  # Add GID_NE where not existing
  monthly_gid_ne <- monthly %>%
    filter(nrg_bal == "GD_PI") %>%
    mutate(year = year(time)) %>%
    left_join(
      gd_pi_to_gid_ne,
      by = c("geo", "siec", "year")
    ) %>%
    mutate(ratio = replace_na(ratio, 1)) %>%
    mutate(
      values = values * ratio,
      nrg_bal = "GID_NE"
    ) %>%
    select(nrg_bal, siec, freq, unit, geo, time, values)

  # Just ensure there were no GID_NE in monthly
  stopifnot(0 == nrow(monthly %>% filter(nrg_bal == "GID_NE")))

  bind_rows(
    monthly,
    monthly_gid_ne
  ) %>%
    add_iso2()
}

#' Remove EU aggregate values for months where important countries are missing (including absent)
#'
#' Builds a reference dataframe of valid EU time points where all important countries are present
#' and non-NA,
#' then only keeps EU values for those combinations.
#'
#' @param data A dataframe with columns iso2, time, values (and possibly others)
#' @param important_iso2s A character vector of important country ISO2 codes (default: c("DE",
#' "FR", "IT", "ES"))
#' @return The dataframe with EU values set to NA where any important country is missing
#' @export
fix_eu_when_important_countries_missing <- function(
  data,
  important_iso2s = c("DE", "FR", "IT", "ES")
) {
  # Ensure 'iso2' and 'time' columns exist in the data
  stopifnot("iso2" %in% names(data), "time" %in% names(data))

  # Identify unique EU time points with non-NA values
  eu_times <- data %>%
    filter(iso2 == "EU", !is.na(values)) %>%
    distinct(nrg_bal, time)

  # Determine if all important countries are present for each time point
  important_countries_times <- data %>%
    filter(iso2 %in% important_iso2s, !is.na(values)) %>%
    group_by(nrg_bal, time) %>%
    summarise(
      has_important_countries = n_distinct(iso2) == length(important_iso2s),
      .groups = "drop"
    )

  # Identify EU time points where not all important countries are present
  to_remove <- eu_times %>%
    left_join(important_countries_times, by = c("nrg_bal", "time")) %>%
    filter(has_important_countries == FALSE) %>%
    mutate(iso2 = "EU")

  # Remove EU rows where not all important countries are present
  data <- data %>%
    anti_join(to_remove, by = c("iso2", "time", "nrg_bal"))

  # Print information if rows were removed
  if (nrow(to_remove) > 0) {
    log_warn(glue::glue(
      "Rows removed for EU where not all important countries are present: {nrow(to_remove)}"
    ))
  }

  return(data)
}

#' This function fills SECTOR_OTHER from SECTOR_TOTAL, SECTOR_TRANSPORT, SECTOR_ELEC etc.
#' when SECTOR_OTHER is missing.
#'
#' This is different from the split total elec:
#' - it manages cases where the four "sectors" are available (used for oil and coal at the moment)
#' - it works by date: i.e. the result could be a time period with only total sector over a certain
#' period,
#' and sector-disaggregated values over the next period (e.g. for oil, where transport data is only
#' available from 2011)
#'
#'
#' @param co2
#'
#' @return
#' @export
#'
#' @examples
detotalise_co2 <- function(x) {
  # sector should NOT be in group cols
  group_by_cols <- intersect(names(x), c("iso2", "geo", "date", "fuel", "estimate", "unit"))

  # Deduct known sectors from totals. When all and sector rows coexist, the
  # sector total is the anchor and others is the residual.
  filler <- x %>%
    filter(sector != SECTOR_OTHERS) %>%
    mutate(factor = ifelse(sector == SECTOR_ALL, 1, -1)) %>%
    group_by_at(group_by_cols) %>%
    filter(
      any(sector == SECTOR_ALL),
      any(sector != SECTOR_ALL & !is.na(value))
    ) %>%
    summarise(
      value_deducted = sum(value * factor),
      sector = SECTOR_OTHERS,
      .groups = "drop"
    ) %>%
    arrange(desc(date))

  y <- x %>%
    full_join(filler) %>%
    mutate(
      value = if_else(
        sector == SECTOR_OTHERS & !is.na(value_deducted),
        value_deducted,
        value
      )
    ) %>%
    select(-value_deducted) %>%
    group_by(iso2, fuel, date) %>%
    # Remove total if there is another sector
    filter(!(sector == SECTOR_ALL & any(sector == SECTOR_OTHERS & !is.na(value)))) %>%
    ungroup()

  return(y)
}

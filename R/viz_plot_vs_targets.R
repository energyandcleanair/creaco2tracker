plot_vs_targets <- function(co2,
                          iso2s = "EU",
                          colors,
                          filepath,
                          year_f,
                          min_year = 2010,
                          title = "EU CO2 emissions vs. 2030 and 2050 reduction targets",
                          width = 8,
                          height = 5,
                          dpi=300
                          ) {

   # https://climateactiontracker.org/countries/eu/targets/
  # As per CAT, we assume the target translates into 44% below 2010 levels in 2030 and 93% in 2050
  # excluding LULUCF and international aviation

  # Set default title if not provided
  region_name <- if(length(iso2s)==1) paste0(iso2_to_name(iso2s), " ") else ""
  colors <- get_colors()
  colors$Target <- "#DEDEDE"

  co2_filtered <- co2 %>%
    filter(iso2 %in% iso2s, estimate=="central", fuel!=FUEL_TOTAL) %>%
    filter(sector != SECTOR_TRANSPORT_INTERNATIONAL_AVIATION) %>%
    group_by(year=year(date), date=floor_date(date, "year"), sector) %>%
    summarise(value=sum(value, na.rm=T)/1e9, .groups="drop")

  # Get 2010 baseline for EU target calculation
  baseline_2010 <- co2_filtered %>%
    filter(year==2010) %>%
    summarise(value=sum(value, na.rm=T)) %>%
    pull(value)

  # Calculate targets
  target_2030 <- baseline_2010 * (1 - 0.44)
  target_2050 <- baseline_2010 * (1 - 0.93)


  # Historical emissions by sector
  historical <- co2_filtered %>%
    filter(year >= min_year, year <= year_f)

  # Remove one day to the last value to avoid overlapping with the target projection
  historical <- historical %>%
    mutate(date=if_else(year==max(year), date-1, date))

  # Create target projection data
  latest_total <- co2_filtered %>%
    filter(year==year_f) %>%
    summarise(value=sum(value, na.rm=T)) %>%
    pull(value)

  target_projection_2030 <- tibble(
    year = year_f:2030,
    value = seq(latest_total, target_2030, length.out=length(year_f:2030)),
    sector = "Target"
  ) %>%
    mutate(date=as.Date(paste0(year, "-01-01")))

  target_projection_2050 <- tibble(
    year = 2030:2050,
    value = seq(target_2030, target_2050, length.out=length(2030:2050)),
    sector = "Target"
  ) %>%
    mutate(date=as.Date(paste0(year, "-01-01")))

  plt_data <- bind_rows(
    recode_sector_names(historical),
    target_projection_2030,
    target_projection_2050
  )

  labels <- plt_data %>%
    filter(year==year_f) %>%
    arrange(value) %>%
    pull(sector)

  plt_data$sector <- factor(plt_data$sector, levels=labels)

  write_csv(plt_data, creahelpers::change_extension(filepath, "csv"))

  # Create plot
  plt <- ggplot(plt_data) +
    geom_area(aes(x=date, y=value, fill=sector),
              alpha=0.8) +
    # geom_area(data=bind_rows(
    #   target_projection_2030,
    #   target_projection_2050),
    #  ,
    #   alpha=0.5) +
    ggplot2::scale_fill_manual(values = colors) +
    rcrea::theme_crea_new() +
    # Reduce caption font size
    theme(
      plot.caption = element_text(size=7.5)
    ) +
    rcrea::scale_y_crea_zero() +
   scale_x_date(date_breaks = "5 year",
                date_labels = "%Y",
                limits = c(min(plt_data$date), max(plt_data$date)),
                expand = c(0, 0)) +
    labs(
      title = title,
      subtitle = "Billion tonnes of CO2 per year",
      caption = paste0(
        c("Source: CREA estimates based on EUROSTAT, IPCC and Climate Action Tracker (CAT).",
        "Targets are based on CAT interpretation of 2023 EU NDCs: 44% reduction by 2030 and 93% by 2050 compared to 2010 levels.",
        "Estimates include emissions from fossil fuel combustion alone and excludes LULUCF and international aviation."),
        collapse = "\n"
      ),
      y = NULL,
      x = NULL,
      fill=NULL,
      alpha=NULL
    )

  quicksave(filepath,
           width = width,
           height = height,
           plot = plt,
           dpi = dpi)

  return(plt)
}

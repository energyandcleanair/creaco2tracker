get_colors <- function() {
  elec_str <- "Power Generation"

  colors_fuel <- c(
    Coal = rcrea::pal_crea[["Black"]],
    Gas = "#666666",
    `Fossil Gas` = "#666666",
    Oil = rcrea::pal_crea[["Dark.purple"]],

    # Electricity
    Solar = "#fff2cc",
    Wind = rcrea::pal_crea[["Green"]],
    Hydro = rcrea::pal_crea[["Dark.blue"]],
    Nuclear = rcrea::pal_crea[["Orange"]],
    Other = "#cce7eb",
    `Total change` = rcrea::pal_crea[["Dark.red"]]
  )

  colors_sector <- setNames(
    c(
      rcrea::pal_crea[["Blue"]],
      rcrea::pal_crea[["Blue"]],
      rcrea::pal_crea[["Orange"]],
      colorspace::lighten(rcrea::pal_crea[["Orange"]], 0),
      colorspace::lighten(rcrea::pal_crea[["Orange"]], 0.3),
      colorspace::lighten(rcrea::pal_crea[["Orange"]], 0.5),
      rcrea::pal_crea[["Green"]],
      rcrea::pal_crea[["Green"]]
    ),
    c(
      elec_str,
      "Power\nGeneration",
      "Transport",
      "Transport (Domestic)",
      "Transport (International Aviation)",
      "Transport (International Shipping)",
      "Others (Industry & Buildings)",
      "Others"
    )
  )

  colors <- c(colors_fuel, colors_sector, c(Total = rcrea::pal_crea[["Dark.red"]]))
  return(colors)
}


recode_sector_names <- function(x, power_sector_name = "Power Generation") {

  x %>%
    mutate(sector = factor(sector,
                           levels = c(SECTOR_ELEC,
                                      SECTOR_TRANSPORT,
                                      SECTOR_TRANSPORT_DOMESTIC,
                                      SECTOR_TRANSPORT_INTERNATIONAL_AVIATION,
                                      SECTOR_TRANSPORT_INTERNATIONAL_SHIPPING,
                                      SECTOR_OTHERS,
                                      SECTOR_ALL),
                           labels = c(power_sector_name,
                                      "Transport",
                                      "Transport (Domestic)",
                                      "Transport (International Aviation)",
                                      "Transport (International Shipping)",
                                      "Others (Industry & Buildings)",
                                      "Total")))
}

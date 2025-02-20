plot_industrial_index <- function(industrial_indexes,
                                  by_fuel=T,
                                  year_f=year(today())-1,
                                  filepath=NULL,
                                  width=10,
                                  height=6){


  shorten_nace <- function(x){
    case_when(
      grepl("mining", x, ignore.case = T) ~ "Mining",
      grepl("food", x, ignore.case = T) ~ "Food & Beverages",
      grepl("textiles", x, ignore.case = T) ~ "Textiles",
      grepl("wood", x, ignore.case = T) ~ "Wood",
      grepl("paper", x, ignore.case = T) ~ "Paper",
      grepl("Printing", x, ignore.case = T) ~ "Printing",
      grepl("Coke", x, ignore.case = T) ~ "Coke & refined petroleum",
      grepl("Chemical", x, ignore.case = T) ~ "Chemical",
      grepl("Pharmaceutical", x, ignore.case = T) ~ "Pharmaceutical",
      grepl("Rubber", x, ignore.case = T) ~ "Rubber & Plastics",
      grepl("Non-metallic", x, ignore.case = T) ~ "Non-metallic",
      grepl("basic metals", x, ignore.case = T) ~ "Basic metals",
      grepl("Manufacture of machinery", x, ignore.case = T) ~ "Machinery",
      grepl("electronic", x, ignore.case = T) ~ "Electronics",
      grepl("electrical", x, ignore.case = T) ~ "Electrical",
      grepl("metal products", x, ignore.case = T) ~ "Metal products",
      grepl("motor vehicles", x, ignore.case = T) ~ "Motor vehicles",
      grepl("furniture", x, ignore.case = T) ~ "Furniture",

      T ~ x
    )
  }

  # industrial_indexes %>%
  #   distinct(nace_r2_code, nace_r2) %>%
  #   mutate(label=shorten_nace(nace_r2)) %>%
  #   View()

  colors <- get_colors()

  plt_data <- industrial_indexes %>%
    filter(iso2=="EU",
           year(date) %in% c(year_f-1, year_f)
    ) %>%
    # Set product to "All" if by_fuel is FALSE
    mutate(product = if(!by_fuel) "Total" else product) %>%
    group_by(iso2, geo, estimate, nace_r2,
             product,  # Now we can always include product
             nace_r2_code, year=year(date)) %>%
    summarise(energy_tj=sum(energy_tj)) %>%
    arrange(year) %>%
    mutate(diff_tj= energy_tj - lag(energy_tj)) %>%
    select(-energy_tj) %>%
    mutate(label=shorten_nace(nace_r2)) %>%
    filter(year==max(year)) %>%
    spread(estimate, diff_tj)  %>%
    # Keep most significant ones only
    group_by(product=stringr::str_to_title(product)) %>%
    filter(abs(central) > quantile(abs(central), 0.3)) %>%
    ungroup() %>%
    mutate(label=tidytext::reorder_within(label, central, product))



  plt <- ggplot(plt_data, aes(label, central, fill=product)) +
    geom_col(show.legend = F) +
    geom_errorbar(aes(ymin=lower, ymax=upper), linewidth=0.2, width=0.2, col="#999999") +
    # Only use faceting if by_fuel is TRUE and there's more than one product
    {if(by_fuel && n_distinct(plt_data$product) > 1)
      facet_wrap(~product, scales='free_x')
    } +
    tidytext::scale_x_reordered() +
    rcrea::theme_crea_new() +
    scale_fill_manual(values=get_colors()) +
    scale_y_continuous(labels=function(x) paste0(ifelse(x>0,"+",""),scales::comma(x))) +
    labs(title=paste("EU Change in energy consumption per industrial sector",
                    if(by_fuel) "and fuel family" else ""),
         subtitle="2024 vs 2023 in TeraJoule",
         x=NULL,
         y=NULL,
         caption=paste0(c(
           glue("Note: This chart assumes no change in energy intensity per sector{ifelse(by_fuel, 'and fuel','')}."),
           "Only sectors with the most significant changes are shown.",
           "Source: CREA analysis based on EUROSTAT."
         ), collapse = "\n")
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))



  if(!is.null(filepath)){
    filepath_csv <- creahelpers::change_extension(filepath, "csv")
    write_csv(plt_data, filepath_csv)

    rcrea::quicksave(filepath, width=width, height=height, plot=plt)
  }
}

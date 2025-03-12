#' Plot change in energy consumption yoy by sector
#'
#' @param industrial_indexes
#' @param by_fuel
#' @param year_f
#' @param filepath
#' @param width
#' @param height
#'
#' @return
#' @export
#'
#' @examples
plot_industrial_index_bar_yoy <- function(industrial_indexes,
                                          iso2="EU",
                                  by_fuel=T,
                                  year_f=year(today())-1,
                                  filepath=NULL,
                                  width=10,
                                  height=6){

  # industrial_indexes %>%
  #   distinct(nace_r2_code, nace_r2) %>%
  #   mutate(label=shorten_nace(nace_r2)) %>%
  #   View()

  colors <- get_colors()

  plt_data <- industrial_indexes %>%
    filter(iso2==!!iso2,
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
    # Keep top ten changes in abs terms
    group_by(product=stringr::str_to_title(product)) %>%
    top_n(12, abs(central)) %>%
    ungroup() %>%
    mutate(label=tidytext::reorder_within(label, central, product))



  plt <- ggplot(plt_data, aes(label, central, fill=product)) +
    geom_col(show.legend = F, width=0.6) +
    geom_errorbar(aes(ymin=lower, ymax=upper), linewidth=0.2, width=0.2, col="#999999") +
    # Only use faceting if by_fuel is TRUE and there's more than one product
    {if(by_fuel && n_distinct(plt_data$product) > 1)
      facet_wrap(~product, scales='free_x')
    } +
    tidytext::scale_x_reordered() +
    rcrea::theme_crea_new() +
    scale_fill_manual(values=get_colors()) +
    scale_y_continuous(labels=function(x) paste0(ifelse(x>0,"+",""),scales::comma(x))) +
    labs(title=paste(glue("{iso2_to_name(iso2)} Change in energy consumption per industrial sector"),
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
    grepl("repair", x, ignore.case = T) ~ "Repair",
    grepl("other transport", x, ignore.case = T) ~ "Other transport equipment",
    T ~ x
  )
}

plot_industrial_scatter <- function(industrial_indexes,
                                  industrial_trade,
                                  industrial_production,
                                  year_f=year(today())-1,
                                  filepath=NULL,
                                  width=8,
                                  height=6){


  plt_data <- industrial_indexes %>%
    filter(iso2=="EU",
           year(date) %in% c(year_f-1, year_f)) %>%
    group_by(iso2, nace_r2, nace_r2_code, estimate, year=year(date)) %>%
    summarise(energy_pj=sum(energy_tj)/1e3) %>%
    left_join(
      industrial_trade %>%
        ungroup() %>%
        filter(unit=="kg") %>%
        select(iso2, nace_r2_code, year, net_import) %>%
        left_join(
          industrial_production %>%
            ungroup() %>%
            filter(year==max(industrial_trade$year)-1,
                   unit=="kg") %>%
            select(iso2, nace_r2_code, production=value)
        )
    ) %>%
    arrange(year) %>%
    group_by(iso2, estimate, nace_r2, nace_r2_code) %>%
    mutate(production_change = (energy_pj/lag(energy_pj) - 1),
           # Handle sign changes in net imports
           import_change = case_when(
             TRUE ~ (net_import - lag(net_import)) / production  # Expressed as change of imports / production to be comparable with production change
           )) %>%
    filter(year==max(year)) %>%
    mutate(label=shorten_nace(nace_r2)) %>%
    filter(estimate=="central") %>%
    filter(label!="Machinery") %>%
    ungroup() %>%
    # Top 10 sectors by energy consumption
    top_n(10, energy_pj) %>%
    # Cap at 100%
    mutate(import_change_capped = pmin(import_change, 1),
           import_change_capped = pmax(import_change_capped, -1),
           production_change_capped = pmin(production_change, 1),
           production_change = pmax(production_change_capped, -1))


  xmax <- max(abs(plt_data$import_change))
  ymax <- max(abs(plt_data$production_change))

  ggplot(plt_data, aes(import_change_capped, production_change_capped, label=label, col=label, fill=label)) +
    geom_point(aes(size=energy_pj), alpha=0.6, show.legend = T) +

    geom_hline(yintercept=0, color="gray70") +
    geom_vline(xintercept=0, color="gray70") +
    # Add axis annotations
    # annotate("text", x=max(plt_data$kg_change), y=0,
    #          label="Increased imports", hjust=-0.1, vjust=1.5, size=3) +
    # annotate("text", x=min(plt_data$kg_change), y=0,
    #          label="Decreased imports", hjust=1.1, vjust=1.5, size=3) +
    # annotate("text", x=0, y=max(plt_data$tj_change),
    #          label="Increased production", hjust=-0.1, vjust=-0.1, size=3) +
    # annotate("text", x=0, y=min(plt_data$tj_change),
    #          label="Decreased production", hjust=-0.1, vjust=1.1, size=3) +
    rcrea::theme_crea_new() +
    scale_x_continuous(labels=rcrea::scale_percent_format(with_sign=T)) +
    # scale_color_distiller(palette="Reds") +
    scale_y_continuous(labels=rcrea::scale_percent_format(with_sign=T)) +
    # Center on 0
    coord_cartesian(xlim=c(-xmax, xmax), ylim=c(-ymax, ymax)) +
    rcrea::scale_color_crea_d(darken=0.3) +
    guides(color="none", fill="none") +
    ggrepel::geom_text_repel(size=3,
                             # hide segment
                             segment.color = NA,
                             show.legend = F
                             ) +
    labs(title="Changes in EU industrial production and trade, 2023-2024",
         # subtitle="",
         x="Change in net imports volume (% of 2023 production)",
         y="Change in production volume (% of 2023 production)",
         size="Annual energy\nconsumption (PJ)",
         caption=paste0(c(
           "Note:  Changes in net imports are expressed as percentage of production to be comparable.",
           "Only the top 10 sectors by energy consumption are shown.",
           "Source: CREA analysis based on EUROSTAT data."
         ), collapse = "\n")) -> plt

  plt
  if(!is.null(filepath)){
    filepath_csv <- creahelpers::change_extension(filepath, "csv")
    write_csv(plt_data, filepath_csv)
    rcrea::quicksave(filepath, width=width, height=height, plot=plt)

    plt_data %>%
      select(label, nace_r2_code, production_change, import_change) %>%
      mutate(
             production_change = rcrea::scale_percent(production_change, with_sign=T),
             import_change=rcrea::scale_percent(import_change, with_sign=T)) %>%
      clipr::write_clip()
  }

  return(plt)
}

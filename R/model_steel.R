get_steel <- function(){
  # Numbers were downloaded from https://worldsteel.org/data/annual-production-steel-data/?ind=P1_crude_steel_total_pub/
  filepath <- get_data_filepath('world-steel-association.csv')
  steel <- read_csv(filepath, skip=2) %>%
    select(geo=Country, `2023`, `2024`) %>%
    gather(key = "year", value = "value", -geo) %>%
    add_iso2() %>%
    filter(iso2 %in% get_eu_iso2s(include_eu=F))

  # Add EU
  steel %>%
    group_by(iso2="EU", geo=="European Union", year) %>%
    summarise(value = sum(value)) %>%
    bind_rows(steel) -> steel

  # Y-o-y
  steel %>%
    group_by(iso2) %>%
    mutate(value = value/lag(value) - 1) %>%
    filter(!is.na(value)) %>%
    select(-geo)
}

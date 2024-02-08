ember.get_eu_generation <- function(){
  read_csv('data/ember-eu-generation.csv') %>%
    filter(country_or_region=='Europe') %>%
    # Remove double counting
    filter(variable %in% c(
      'Bioenergy', 'Coal', 'Gas', 'Hydro', 'Nuclear', 'Other Fossil',
      'Other Renewables', 'Solar', 'Wind'
    )) %>%
    tibble()
}

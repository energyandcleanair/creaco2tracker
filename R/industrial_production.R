### THIS IS DIFFERENT FROM INDPROD
# This one is used to compare trade with production to have a % comparable with change in production from indprod
get_industrial_production <- function(){


  # The first 4 digits of a Prodcom code refer to the statistical classification of economic activities (NACE)
  url <- "https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/ds-056121/1.0/*.*.*.*?c[freq]=A&c[decl]=2027&c[indicators]=PRODQNT&c[TIME_PERIOD]=2014,2015,2016,2017,2018,2019,2020,2021,2022,2023&compress=false&format=csvdata&formatVersion=2.0&lang=en&labels=name"
  raw <- read_csv(url)

  production <- raw %>% select(year=TIME_PERIOD...12,
                 prccode,
                 PRCCODE,
                 indicator=indicators,
                 value=OBS_VALUE,

                 ) %>%
    mutate(nace_r2_code=glue("C{substr(prccode, 1, 2)}")) %>%
    filter(
      # Only keep manufacturing
      as.numeric(substr(prccode, 1, 2)) %in% 10:33
    ) %>%
    group_by(iso2="EU", year, nace_r2_code) %>%
    summarise(value=sum(value),
              unit="kg")


  # Group some of them to match indprod
  nace_groupings <- list(
    "C10-C12" = c("C10", "C11", "C12"),
    "C13-C15" = c("C13", "C14", "C15"),
    "C31_C32" = c("C31", "C32")
  ) %>%
    enframe(name="nace_group", value="nace_r2_code") %>%
    unnest(cols=nace_r2_code)

  production <- production %>%
    left_join(nace_groupings, by="nace_r2_code") %>%
    group_by(iso2, year, nace_group=coalesce(nace_group, nace_r2_code)) %>%
    summarise(value=sum(value),
              unit="kg") %>%
    ungroup() %>%
    mutate(nace_r2_code=nace_group) %>%
    select(-nace_group)

  # production %>%
  #   ggplot() +
  #   geom_line(aes(x=year, y=value, col=nace_r2_code)) +
  #   ggrepel::geom_text_repel(
  #     data=function(x) x %>% filter(year==max(year)),
  #     aes(x=year, y=value, label=nace_r2_code), col="black", size=3)
  #
  #
  # industrial_indexes %>%
  #   filter(iso2=="EU") %>%
  #   group_by(iso2, nace_r2, nace_r2_code, year=year(date)) %>%
  #   summarise(energy_pj=sum(energy_tj)/1e3) %>%
  #   filter(year >= "2014", nace_r2_code %in% c("C24")) %>%
  #   ggplot() +
  #   geom_line(aes(x=year, y=energy_pj, col=nace_r2_code))  +
  #   rcrea::scale_y_zero()
  #
  #
  # indprod %>%
  #   filter(iso2=="EU",
  #          nace_r2_code %in% c("C24"),
  #          time >= "2014-01-01",
  #          time < "2023-01-01"
  #          ) %>%
  #   group_by(iso2, nace_r2_code, year=year(time)) %>%
  #   summarise(values=sum(values)) %>%
  #   ggplot() +
  #   geom_line(aes(x=year, y=values, col=nace_r2_code)) +
  #   rcrea::scale_y_zero()


  return(production)


}

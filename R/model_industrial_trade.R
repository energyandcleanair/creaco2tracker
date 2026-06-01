#' Get net_imports by year and nacer2 code (those from industrial indexes)
#'
#' @param industrial_indexes
#'
#' @return
#' @export
#'
#' @examples
get_industrial_trade <- function(industrial_indexes){

  # https://ec.europa.eu/eurostat/databrowser/view/ds-059331/legacyMultiFreq/table?lang=en&category=ext_go.ext_go_detail
  get_trade_from_sitc <- function(sitc){
    years <- seq(2021,2024)
    url <- glue("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/ds-059331/1.0/*.*.*.*.*.*?c[freq]=A&c[reporter]=EU27_2020&c[partner]=EXT_EU27_2020&c[product]={paste0(sitc, collapse=',')}&c[flow]=1,2&c[indicators]=VALUE_EUR,QUANTITY_KG&c[TIME_PERIOD]={paste0(years, collapse=',')}&compress=false&format=csvdata&formatVersion=2.0&labels=name")
    read_csv(url)
  }

  nace_codes <- unique(industrial_indexes$nace_r2_code)

  # https://wits.worldbank.org/product_concordance.html
  corr_sitc3_nace1 <- read_csv(get_data_filepath('Concordance_S3_to_NC.csv')) %>%
    `names<-`(c("sitc3", "sitc3_label", "nace1", "nace1_label"))

  corr_sitc3_nace1 %>% distinct(nace1)
  #https://unstats.un.org/unsd/classifications/Econ
  sitc3_sitc4 <- readxl::read_xls(get_data_filepath('sitc_3_4.xls'),
                                  skip=9
                                  ) %>%
    `names<-`(c("sitc3", "ex", "sitc4")) %>%
    select(-ex) %>%
    mutate_at(vars(sitc3, sitc4), ~gsub("\\.", "", .))


  # Nace R2 -> R1
  # GPT O1 + https://circabc.europa.eu/sd/a/acbc9240-03b9-43df-97b5-146d102fc18e/NACE%20Rev.%201.1%20-%20NACE%20Rev.%202%20-%20short%20version.pdf
  # + manual check
  nace2_to_1 <- list(
    "B" = c("101","102","103","111","112","120","131","132","141","142","143","144","145"),

    # Combine food (C10), beverages (C11), tobacco (C12)
    "C10-C12" = c(
      "151","152","153","154","155","156","157","158",  # from C10
      "159",                                                   # from C11
      "160"                                                    # from C12
    ),

    # Combine textiles (C13), apparel (C14), leather (C15)
    "C13-C15" = c(
      "171","172","173","174","175","176","177",          # from C13
      "181","182","183",                                      # from C14
      "191","192","193"                                       # from C15
    ),

    "C16" = c("201","202","203","204","205"),
    "C17" = c("211","212"),
    "C18" = c("222","223"),
    "C19" = c("231","232","233"),
    "C20" = c("241","242","243","245","246","247"),
    "C21" = c("244"),
    "C22" = c("251","252"),
    "C23" = c("261","262","263","264","265","266","267","268"),
    "C24" = c("271","272","273","274","275"),
    "C25" = c("281","282","283","284","285","286","287"),
    "C26" = c("300","321","322","323","331","332","333","334","335"),
    "C27" = c("311","312","313","314","315","316"),
    "C28" = c("291","292","293","294","295","296","297"),
    "C29" = c("341","342","343"),
    "C30" = c("351","352","353","354","355"),

    # Combine furniture (C31) + other manufacturing (C32)
    "C31_C32" = c("361","362","363","364","365","366")

    # Repair & installation scattered across multiple 11 codes
    # "C33" = c("Various codes (eg 292, 316, 33x, etc)")

    # To compare with Columbia
    # Newly added subcodes:
    # "C201"    = c("241"),
    # "C231"    = c("261"),
    # "C241"    = c("271")
  )

  nace2_sitc4 <- nace2_to_1 %>%
    enframe(name="nace2", value="nace1") %>%
    unnest(nace1) %>%
    left_join(industrial_indexes %>% distinct(nace2=nace_r2_code, nace2_label=nace_r2)) %>%
    left_join(corr_sitc3_nace1 %>% select(nace1, sitc3, sitc3_label)) %>%
    # This one authorises many to many, really few items
    left_join(sitc3_sitc4, relationship="many-to-many")

  to_collect <- nace2_sitc4 %>%
    filter(!is.na(sitc4)) %>%
    group_by(nace2) %>%
    filter(!duplicated(sitc4)) %>%
    summarise(sitc4s=paste(sitc4, collapse=","))

  trade <- mapply(function(nace2, sitc4s){
    get_trade_from_sitc(sitc4s) %>%
      mutate(product=as.character(product)) %>%
      mutate(nace2=nace2)
  }, to_collect$nace2, to_collect$sitc4s, SIMPLIFY=FALSE) %>%
    bind_rows()

  # Visual check match
  # trade %>%
  #   distinct(nace2, PRODUCT) %>%
  #   left_join(industrial_indexes  %>% distinct(nace2=nace_r2_code, nace2_label=nace_r2)) %>%
  #   select(nace2_label, PRODUCT) %>%
  #   View()

  # Compare with Columbia paper
  # https://www.energypolicy.columbia.edu/publications/anatomy-of-the-european-industrial-gas-demand-drop/
  # C23 is -26% and -43%: we have -27% and -45%
  # C201 is -9% and -52%: we have +6% and -47%
  # C231 is +9% and +42%: we have +36% + 54%
  # C241 is +5% and -9%: we have +6% and -16%
  # trade %>%
  #   group_by(nace2, year=TIME_PERIOD...16, unit=INDICATORS, FLOW) %>%
  #   summarise(value=sum(OBS_VALUE)) %>%
  #   spread(FLOW, value) %>%
  #   mutate(net_import=IMPORT-EXPORT) %>%
  #   group_by(nace2, unit) %>%
  #   arrange(year) %>%
  #   mutate(diff_vs_2021=rcrea::scale_percent((net_import - net_import[year==2021])/net_import[year==2021],
  #                                            with_sign=T)) %>%
  #   filter(year %in% c(2022, 2023), nace2%in% c("C23", "C201", "C231", "C241", "C2442"), unit=="QUANTITY_KG") %>%
  #   arrange(nace2, year)


  trade %>%
    group_by(nace2, year=TIME_PERIOD...16, unit=INDICATORS, FLOW) %>%
    summarise(value=sum(OBS_VALUE)) %>%
    spread(FLOW, value) %>%
    mutate(net_import=IMPORT-EXPORT) %>%
    select(-c(IMPORT, EXPORT)) %>%
    mutate(unit=recode(unit, QUANTITY_KG="kg", VALUE_EUR="eur")) %>%
    rename(nace_r2_code=nace2) %>%
    inner_join(
      industrial_indexes %>% distinct(nace_r2_code, nace_r2)
    ) %>%
    mutate(iso2="EU")
}



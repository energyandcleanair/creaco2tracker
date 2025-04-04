agsi.get_storage_change <- function(date_from, date_to, iso2, verbose=F){
  pbapply::pblapply(iso2, function(iso2){
    message(glue("Getting storage change data for {iso2} from {date_from} to {date_to}"))

    # Add api key in header
    api_key <- Sys.getenv("AGSI_API_KEY")
    if(api_key==""){
      warning("AGSI_API_KEY not set. Returning NULL")
      return(NULL)
    }

    url <- sprintf("https://agsi.gie.eu/api?country=%s&from=%s&to=%s&page=1&size=100000",
                   iso2, date_from, date_to)

    httpResponse <- httr::GET(url, httr::add_headers("x-key" = api_key), httr::accept_json())
    data <- jsonlite::fromJSON(httr::content(httpResponse, "text", encoding="UTF-8"))
    data <- data$data

    if(nrow(data)==0 || !'netWithdrawal' %in% names(data)){
      message(glue("No data for {iso2} from {date_from} to {date_to}"))
      return(NULL)
    }

    data %>%
      select(iso2=code,
             date=gasDayStart,
             value_gwh=netWithdrawal) %>%
      mutate(date=lubridate::date(date),
             value_gwh=as.numeric(value_gwh),
             value_m3=value_gwh*1e6/gcv_kwh_m3,
             type='storage_drawdown') %>%
      tibble()
  }) %>%
    bind_rows()
}

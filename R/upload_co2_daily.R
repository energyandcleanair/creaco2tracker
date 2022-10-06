get_pg_url <- function(production){
  readRenviron(".Renviron")
  if(production){
    Sys.getenv("CREA_DB_PRODUCTION")
  }else{
    Sys.getenv("CREA_DB_DEVELOPMENT")
  }
}


upload_co2_daily <- function(co2_daily, production=T, clear_all_first=F){

  print(sprintf("=== Uploading co2_daily (%s) ===", ifelse(production,"production","development")))

  unique_cols <-  c('region', 'date', 'fuel', 'sector', 'unit', 'frequency')

  p <- co2_daily %>%
    select(region, date, fuel, sector, unit, frequency, version, value)

  db <- dbx::dbxConnect(adapter="postgres",
                        url=get_pg_url(production=production))
  if(clear_all_first){
    dbx::dbxExecute(db, sprintf("DELETE FROM co2_emission;"))
  }

  dbx::dbxUpsert(db, "co2_emission", p, where_cols=unique_cols)
  dbx::dbxDisconnect(db)
}

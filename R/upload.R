get_pg_url <- function(production){
  readRenviron(".Renviron")
  if(production){
    Sys.getenv("CREA_DB_PRODUCTION")
  }else{
    Sys.getenv("CREA_DB_DEVELOPMENT")
  }
}

get_energy_pg_url <- function(production){
  readRenviron(".Renviron")
  if(production){
    Sys.getenv("POWER_DB_PRODUCTION")
  }else{
    Sys.getenv("POWER_DB_DEVELOPMENT")
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


upload_corrected_demand <- function(corrected_demand, production=T, clear_all_first=F){
  
  print(sprintf("=== Uploading corrected_demand (%s) ===", ifelse(production,"production","development")))
  
  unique_cols <-  c('region_id', 'date', 'fuel', 'data_source', 'sector', 'unit', 'frequency')
  
  p <- corrected_demand %>%
    select(region_id, region_type, date, fuel, data_source, sector, unit, frequency, value)
  
  db <- dbx::dbxConnect(adapter="postgres",
                        url=get_energy_pg_url(production=production))
  if(clear_all_first){
    dbx::dbxExecute(db, sprintf("DELETE FROM demand WHERE data_source='%s';", unique(gas_demand$data_source)))
  }
  
  dbx::dbxUpsert(db, "demand", p, where_cols=unique_cols)
  dbx::dbxDisconnect(db)
}

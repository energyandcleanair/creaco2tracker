get_pg_url <- function(production){
  readRenviron(".Renviron")
  if(production){
    Sys.getenv("CREA_DB_PRODUCTION")
  }else{
    Sys.getenv("CREA_DB_DEVELOPMENT")
  }
}

upload_co2_daily <- function(co2_daily, production=T, clear_all_first=F){

  # Formatting / cleaning for db
  co2_daily_formatted <- format_co2_for_db(co2_daily)

  print(sprintf("=== Uploading co2_daily (%s) ===", ifelse(production,"production","development")))

  unique_cols <-  c('region', 'date', 'fuel', 'sector', 'unit', 'frequency', 'version')

  p <- co2_daily_formatted %>%
    select(region, date, fuel, sector, unit, frequency, version, value, value_lower, value_upper)

  db <- dbx::dbxConnect(adapter="postgres",
                        url=get_pg_url(production=production))
  if(clear_all_first){
    dbx::dbxExecute(db, sprintf("DELETE FROM co2_emission;"))
    # Insert all in one go
    dbx::dbxInsert(db, "co2_emission", p)
  }else{
    dbx::dbxUpsert(db, "co2_emission", p, where_cols=unique_cols)
  }
  dbx::dbxDisconnect(db)
}


upload_corrected_demand <- function(corrected_demand, production=T, clear_all_first=F){

  print(sprintf("=== Uploading corrected_demand (%s) ===", ifelse(production,"production","development")))

  unique_cols <-  c('region_id', 'date', 'fuel', 'data_source', 'sector', 'unit', 'frequency')

  p <- corrected_demand %>%
    select(region_id, region_type, date, fuel, data_source, sector, unit, frequency, value)

  db <- dbx::dbxConnect(adapter="postgres",
                        url=get_pg_url(production=production))
  if(clear_all_first){
    dbx::dbxExecute(db, sprintf("DELETE FROM energy.demand WHERE data_source='%s' and fuel = any(ARRAY['%s']);",
                                unique(p$data_source),
                                paste(unique(p$fuel), collapse="','")))
  }
  table <- DBI::Id(schema="energy", table="demand")
  dbx::dbxUpsert(db, table, p, where_cols=unique_cols)
  dbx::dbxDisconnect(db)
}


upload_gas_demand <- function(gas_demand, production=T, clear_all_first=F){

  print(sprintf("=== Uploading gas demand (%s) ===", ifelse(production,"production","development")))

  unique_cols <-  c('region_id', 'date', 'fuel', 'data_source', 'sector', 'unit', 'frequency')

  p <- gas_demand %>%
    select(region_id, region_type, date, fuel, data_source, sector, unit, frequency, value)

  db <- dbx::dbxConnect(adapter="postgres",
                        url=get_pg_url(production=production))
  if(clear_all_first){
    dbx::dbxExecute(db, sprintf("DELETE FROM energy.demand WHERE fuel='%s' and sector='%s' and data_source='%s';",
                                unique(gas_demand$fuel),
                                unique(gas_demand$sector),
                                unique(gas_demand$data_source)))
  }
  table <- DBI::Id(schema="energy", table="demand")
  dbx::dbxUpsert(db, table, p, where_cols=unique_cols)
  dbx::dbxDisconnect(db)
}

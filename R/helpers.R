expand.dates <- function(df, datecol='date.of.departure', targetdates=NULL) {
  groupvarlist <- df %>% select(all_of(dplyr::group_vars(df))) %>% as.list() %>% lapply(unique)

  if(is.null(targetdates))
    targetdates <- seq.Date(min(df[[datecol]]),
                                 max(df[[datecol]]),
                                 by='day')

  if(!is.list(targetdates)) targetdates %<>% list

  names(targetdates) <- datecol
  full_join(df, expand.grid(c(groupvarlist, targetdates)))
}

get_yoy <- function(x, date) {
  lastyr <- date
  year(lastyr) %<>% subtract(1)
  ind = match(lastyr, date)
  x / x[ind] - 1
}


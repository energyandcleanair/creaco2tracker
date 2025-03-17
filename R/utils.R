create_dir <- function(folder){
  if(!is.null(folder) & !rlang::is_empty(folder)){
    dir.create(folder, showWarnings = F, recursive = T)
  }
}

is_null_or_empty <- function(x){
  is.null(x) | (length(x)==0)
}

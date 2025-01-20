create_dir <- function(folder){
  if(!is.null(folder) & !is_empty(folder)){
    dir.create(folder, showWarnings = F, recursive = T)
  }
}

shrink.list <- function(x.list){
  id <- sapply(x.list, function(x){if(!all(is.na(x))) TRUE else FALSE})
  if (all(id==FALSE)) {
    x.list <- NA
  } else {
    x.list <- x.list[id]
  }
  x.list
}
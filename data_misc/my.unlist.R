my.unlist <- function(x, y=""){
  res <- list()
  for (i in names(x)){
    id <- if(y=="") i else paste(y,i,sep="_")
    if (is.list(x[[i]]) && !is.data.frame(x[[i]])) {
      tmp <- my.unlist(x[[i]], y=id)
      res <- c(res,tmp)
    } else {
      res[[id]] <- x[[i]]
    }
  }
  res
}
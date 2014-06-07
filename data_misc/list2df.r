list2df <- function(list.vec){
  len <- max(sapply(list.vec,length))
  df  <- sapply(list.vec, function(x) c(x,rep(NA,len - length(x))))
  if (is.vector(df)) df <- t(df)
  return(df)
}
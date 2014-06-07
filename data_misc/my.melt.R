my.melt <- function(x,...){
  x <- data.frame(Value=as.numeric(x), Row=rep(rownames(x), ncol(x)),
                  Col=rep(colnames(x), each=nrow(x)),stringsAsFactors=F)
  return(x)
}
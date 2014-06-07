fs.n <- function(value, thr=NULL, ordering = c("less", "greater"),...)
{
  ordering <- match.arg(ordering)
  
  value <- as.matrix(value)
  if(is.null(thr))  
    thr <- c(0.001, 0.005, 0.01, 0.05) 

  res <- sapply(thr, function(x){
    fsn  <- if (ordering == "less") apply((value <= x),2,sum, na.rm=T)
            else apply((value >= x),2,sum, na.rm=T)
    c(Threshold=x, fsn)
  })
  res <- t(res)
  rownames(res)<-1:nrow(res)
  res
}
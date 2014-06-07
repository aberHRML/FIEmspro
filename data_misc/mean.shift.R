mean.shift <- function(x,y, method="mean", log.f=FALSE)
{
  x   <- as.data.frame(x)
  x   <- log(x+1)
  mat <- x

  g.mean <- sapply(x, function(x) tapply(x,y,method))
  o.mean <- apply(g.mean,2, method)
  g.mean <- sweep(g.mean,2,o.mean,"-")

  g.mean.mat <- sapply(1:ncol(x), function(i) g.mean[,i][y])
  x  <- x - g.mean.mat
  x <- sapply(x, function(x) {
     min.v <- min(x)
     if (min.v < 0 ) x <- x - min.v
     x
  })
  x[mat==0] <- 0
  if (!log.f)  x <- exp(x) - 1
  x <- as.data.frame(x)
  return(x)
}
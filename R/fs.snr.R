`fs.snr` <-
function(x,y)
{
  y <- as.factor(y)
  if (length(levels(y)) != 2) 
    stop("'y' must have two classes")

  g.mn <- sapply(data.frame(x), function(x) tapply(x,y,mean))
  g.sd <- sapply(data.frame(x), function(x) tapply(x,y,sd))
  snr  <- abs(g.mn[1,] - g.mn[2,])/(g.sd[1,] + g.sd[2,])
  
fs.rank <- rank(-abs(snr), na.last=T, ties.method="random")
fs.order <- order(fs.rank, na.last=T)

  names(fs.rank) <- names(snr)
  nam <- names(snr[fs.order])
  if (!is.null(nam))
     fs.order <- noquote(nam)

  res <- list(fs.rank=fs.rank, fs.order=fs.order, stats=abs(snr))
  return(res)
}


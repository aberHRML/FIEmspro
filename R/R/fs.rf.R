`fs.rf` <-
function(x,y,...)
{
  tmp  <- randomForest(x,y,importance=T,...)
  meas <- tmp$importance[,ncol(tmp$importance)-1]
  meas[meas <= 0] <- 0        

fs.rank <- rank(-meas, na.last=T, ties.method="random")
fs.order <- order(fs.rank, na.last=T)


  names(fs.rank) <- names(meas)
  nam <- names(meas[fs.order])
  if (!is.null(nam))
     fs.order <- noquote(nam)

  res <- list(fs.rank=fs.rank, fs.order=fs.order, stats=meas)
  return(res)
}


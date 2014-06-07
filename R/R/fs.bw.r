## ===============================================================
## lwc-12-04-2007: feature selection using ratio of between-group
##                 to within-group sums of squres (BW).
##
fs.bw <- function(x,y)
{
  if (!is.data.frame(x)) x <- as.data.frame(x)
  if (length(y) != nrow(x))
    stop("x and y is not consistent.")

  bw <- sapply(x, function(z){
    ## z <- x[,1]      ## for debug
    mn.all <- mean(z)
    mn.grp <- tapply(z,y,mean)
    tmp.1  <- tmp.2 <- 0
    for (i in 1:length(z)){
      cls   <- y[i]           ## which class
      tmp.1 <- tmp.1 + (mn.grp[[cls]] - mn.all)^2
      tmp.2 <- tmp.2 + (z[i] - mn.grp[[cls]])^2
    }
    tmp.1 / tmp.2
  })

  fs.order <- order(bw,decreasing=T, na.last=T)
  fs.rank  <- order(fs.order)

  names(fs.rank) <- names(bw)
  nam <- names(bw[fs.order])
  if (!is.null(nam))
     fs.order <- noquote(nam)

  res <- list(fs.rank=fs.rank, fs.order=fs.order, stats=bw)
  return(res)
}


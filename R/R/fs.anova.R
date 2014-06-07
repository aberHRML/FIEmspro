`fs.anova` <-
function(x,y,...)
{
  tmp <- sapply(data.frame(x),function(x) 
                                {tmp <- oneway.test(x ~ y)
                                c(tmp$statistic,tmp$p.value)})

  stats   <- tmp[1,]
  pval    <- tmp[2,]

fs.rank <- rank(-abs(stats), na.last=T, ties.method="random")
fs.order <- order(fs.rank, na.last=T)

  names(fs.rank) <- names(stats)
  nam <- names(stats[fs.order])
  if (!is.null(nam))
     fs.order <- noquote(nam)

  res <- list(fs.rank=fs.rank, fs.order=fs.order, stats=abs(stats),pval=pval)
  return(res)
}


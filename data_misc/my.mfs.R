my.mfs <- function(x,y, fs.method, fs.pars,...){
  res <- lapply(fs.method, function(m) {
    cat("\nFeature Selector = :",m,"\n"); flush.console()
    feat.rank.re(x,y,method=m,pars=fs.pars,...)
  })
  names(res)  <- fs.method
  fs.rank     <- sapply(res,function(x) x$fs.rank)
  fs.stats    <- sapply(res,function(x) x$fs.stats)
  fs.tab      <- feat.tab(fs.stats)
  fs.order    <- sapply(res, function(x) x$fs.order)
  list(fs.order=fs.order, fs.rank=fs.rank, fs.stats=fs.stats, fs.tab=fs.tab,
       all=res)
}
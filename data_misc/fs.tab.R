fs.tab <- function(fs.res, DF,...){
  fs.stats <- lapply(fs.res, function(x){
    val <- lapply(x, function(y){
      fs.tmp  <- y[["all"]]
      stats <- lapply(names(fs.tmp), function(z){
        tmp  <- fs.scores(fs.tmp[[z]], method=z)
      })
      names(stats) <- names(fs.tmp)
      stats
    })
  })
  fs.stats <- my.unlist(fs.stats)

  fs.tab <- lapply(fs.stats, function(x){
    list(fs=names(x), val=x)
  })
  fs.tab <- list2df(my.unlist(fs.tab))
}
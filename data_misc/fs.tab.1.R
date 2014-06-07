fs.tab.1 <- function(fs.res, fs.ord, DF,...){
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

  nam.1 <- names(fs.ord)
  nam.2 <- names(fs.ord[[1]])
  fs.stats <- lapply(nam.1, function(x){
    tmp.1 <- lapply(nam.2, function(y){
      fs    <- fs.ord[[x]][[y]]
      stats <- fs.stats[[x]][[y]]
      tmp.2 <- lapply(stats, function(z) z[fs])
    })
    names(tmp.1) <- nam.2
    tmp.1
  })
  names(fs.stats) <- nam.1

  fs.stats <- my.unlist(fs.stats)

  fs.tab <- lapply(fs.stats, function(x){
    list(fs=names(x), val=x)
  })
  fs.tab <- list2df(my.unlist(fs.tab))
}
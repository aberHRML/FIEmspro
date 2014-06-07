fs.scores.plot <- function(fs.res, DF,...){
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

  stats <- lapply(names(fs.stats), function(x){
    n <- length(fs.stats[[x]])
    data.frame(values=fs.stats[[x]], ind=rep(x, n), idx=1:n, stringsAsFactors=F)
  })
  stats  <- do.call(rbind,stats)
  stats$ind <- factor(stats$ind, levels=unique(stats$ind))
  rownames(stats) <- 1:nrow(stats)

  stats.p <- xyplot(values~idx|ind, data=stats,
                  as.table=T, type=c("g","l"),
                  scales=list(cex =.75,relation="free"),
                  par.strip.text = list(cex=0.65),
                  ylab="Values", xlab="Index of feature rank",
                  main=paste(DF,"FS assessment",sep=":"))

}
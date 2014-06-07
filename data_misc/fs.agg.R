fs.agg <- function(fs.rank.list){
  fs.score  <- apply(fs.rank.list,1,sum)
  fs.order  <- order(fs.score, decreasing=F)
  fs.rank   <- order(fs.order, decreasing=F)
  names(fs.rank) <- rownames(fs.rank.list)
  temp     <- names(fs.rank[fs.order])
  if (!is.null(temp)) fs.order <- temp
  return(list(fs.order=fs.order, fs.rank=fs.rank))
}
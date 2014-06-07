fs.rf.ad <- function(x,y,...)
{
  y      <- factor(y)
  fs     <- fs.rf(x,y,...)
  scores <- fs$stats[fs$fs.order]         
  
  scores.ad <- cumsum(scores/sum(scores))   

  mat  <- list(fs.order=as.character(fs$fs.order), scores.ad=scores.ad, scores=scores)
  return(mat)
}
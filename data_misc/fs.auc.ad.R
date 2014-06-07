fs.auc.ad <- function(x,y,...)
{
  y      <- factor(y)
  fs     <- fs.auc(x,y,...)
  auc    <- fs$stats[fs$fs.order]         
  auc.ad <- auc   

  mat  <- list(fs.order=as.character(fs$fs.order), auc.ad=auc.ad, auc=auc)
  return(mat)
}
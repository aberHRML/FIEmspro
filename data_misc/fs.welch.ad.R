fs.welch.ad <- function(x,y,method="fdr",...)
{
  y     <- factor(y)
  fs    <- fs.welch(x,y,...)
  p     <- fs$pval[fs$fs.order]
  p.ad  <- p.adjust(p, method=method)
  stats <- fs$stats[fs$fs.order]
  mat   <- list(fs.order=as.character(fs$fs.order), pval.ad=p.ad, pval=p, stats=stats)

  return(mat)
}
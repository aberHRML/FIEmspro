fs.scores <- function(fs.res, method=c("fs.rf", "fs.welch", "fs.anova","fs.auc"),...)
{
  method   <- match.arg(method)

  pval.list <- lapply(fs.res$all, function(x) as.data.frame(sapply(x, function(y) y$pval)))
  if (!is.null(unlist(pval.list))){
    pval.list   <- do.call("cbind",pval.list)
    fs.val <- apply(pval.list, 1, mean)
    fs.val <- fs.val[order(fs.val)]
  } else {
    fs.val <- fs.res$fs.stats
    fs.val <- fs.val[rev(order(fs.val))]
  }

  if (method=="fs.welch" || method=="fs.anova"){
    fs.val <- p.adjust(fs.val, method="fdr")
    fs.val <- sort(fs.val)
  } else {
    fs.val <- sort(fs.val,decreasing=T)
  }

  return(fs.val)
}
fs.sel.re <- function(fs.res, method=c("fs.rf", "fs.welch", "fs.auc"),thr,...){
  method   <- match.arg(method)
  ordering <- if (method == "fs.auc") "greater" else "less"

  pval.list <- lapply(fs.res$all, function(x) as.data.frame(sapply(x, function(y) y$pval)))
  if (!is.null(unlist(pval.list))){
    pval.list   <- do.call("cbind",pval.list)
    fs.val <- apply(pval.list, 1, mean)
    fs.val <- fs.val[order(fs.val)]
  } else {
    fs.val <- fs.res$fs.stats
    fs.val <- fs.val[rev(order(fs.val))]
  }

  fs <- names(fs.val)

  if (method == "fs.rf") {
    fs.val.1 <- cumsum(fs.val/sum(fs.val))
  } else if (method=="fs.welch" || method=="fs.anova"){
    fs.val.1 <- p.adjust(fs.val, method="fdr")
  } else if (method=="fs.auc"){
    fs.val.1 <- fs.val
  }

  fs.num <- fs.n(fs.val.1,thr,ordering,...)
  colnames(fs.num) <- c("Threshold", "fs.num")

  fs.order <- lapply(1:nrow(fs.num), function(i){
    len    <- fs.num[i,2]
    fs.tmp <- if (len==0) NA else fs[1:len]
  })
  names(fs.order) <- paste("thr",thr, sep="_")
  fs.order <- list2df(fs.order)
  res <- list(fs.num=fs.num, fs.order=fs.order,
              fs=data.frame(fs.order=fs, value.ad=fs.val.1, value=fs.val))
}
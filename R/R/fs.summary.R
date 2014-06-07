fs.summary<-function (res1, res2, padjust = "fdr", sorting=TRUE) 
{
  pvalnam=paste("mrp",res2$qtl,sep="-")
  if (!is.null(res1$all$pval)) {
     summ = cbind(res1$all$stats, res1$fs.rank, res1$all$pval, p.adjust(res1$all$pval,
         padjust), res2$stats, res2$sdrank, res2$mrpval)
     dimnames(summ)[[1]] = names(res1$all$stats)
     dimnames(summ)[[2]] = c(res1$method, "OriRk", "pval", "pval.ad",
         "AvgRk", "SdevRk", pvalnam)
  } else{
    summ = cbind(res1$all$stats, res1$fs.rank, res2$stats, res2 $sdrank, res2$mrpval)
    dimnames(summ)[[1]] = names(res1$all$stats)
    dimnames(summ)[[2]] = c(res1$method, "OriRk", "AvgRk", "SdevRk",
    pvalnam)
  }
  if(sorting==TRUE){
   lso=sort(summ[,2],decreasing=F,index.return=T)$ix
   summ=summ[lso,]
  }
  summ
}

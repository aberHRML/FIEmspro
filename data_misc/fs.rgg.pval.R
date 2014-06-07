fs.rgg.pval <- function(dat.all, cls, pw, fs.ord){
  pval <- lapply(dn, function(i){
    dat.pair <- dat.sel(dat.all[[i]], cls, choices=pw)
    com      <- apply(dat.pair$com, 1, paste, collapse="~")
    val <- lapply(com, function(j){
      dat.com <- dat.pair$dat[[j]]
      cl.com  <- dat.pair$cl[[j]]
      p <- sapply(as.data.frame(dat.com), function(k) {
         tmp <- t.test(k ~ cl.com,var.equal=F)$p.value
      })
    })
    names(val) <- com
    val
  })
  names(pval) <- dn
  pval  <- my.unlist(pval)
  ford  <- my.unlist(fs.ord)

  tmp <- lapply(names(ford), function(x){
   pval[[x]][ford[[x]]]
  })
  names(tmp) <- names(ford)
  tmp.1 <- lapply(tmp, p.adjust, method="fdr")
  pval <- lapply(names(tmp), function(x){
    list(fs=names(tmp[[x]]), pval=tmp[[x]],pval.ad=tmp.1[[x]])
  })
  names(pval) <- names(tmp)
  pval <- list2df(my.unlist(pval))

}
cor.coef.wrap <- function(dat, fs.order, cutoff=0.75,...){

  fs.order <- as.matrix(fs.order)
  if (is.null(colnames(fs.order)))
    colnames(fs.order) <- paste("fs",1:ncol(fs.order), sep="_")
  fs.cname <- colnames(fs.order)

  res.cor <- lapply(fs.cname, function(i){
    fs <- fs.order[,i]
    fs <- fs[!is.na(fs)]
    if (length(fs) < 1) {
      co <- NA
    } else {
      mat <- dat[,fs,drop=F]
      co  <- lapply(cutoff, function(j) cor.coef(mat, j))
      names(co) <- paste("cor",cutoff, sep="_")
    }
    co
  })
  names(res.cor) <- fs.cname
  res.cor
}
fs.cor.vec <- function(dat, fs.order, cutoff=0.75, fig=T, fig.title="Clus",...){
  fs <- fs.order
  fs <- fs[!is.na(fs)]
  if (length(fs) == 0) {
    hc <- NA
  } else {
    mat <- dat[,fs,drop=F]
    hc  <- lapply(cutoff, function(j){
      hc  <- fs.cor.bas(mat, cutoff=j,fig=fig, main=paste(fig.title,"cor",j,sep="_"))
      hc[-length(hc)]
    })
    names(hc) <- paste("cor",cutoff, sep="_")
    hc$all <- cor(mat)
  }
  hc
}
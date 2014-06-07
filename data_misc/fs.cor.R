fs.cor <- function(dat, fs.order, cutoff=0.75, fig=T, fig.title="Clus",...){
  fs.order <- as.matrix(fs.order)
  if (is.null(colnames(fs.order)))
    colnames(fs.order) <- paste("fs",1:ncol(fs.order), sep="_")
  fs.cname <- colnames(fs.order)
  res.cor <- lapply(fs.cname, function(i){
    fs.cor.vec(dat, fs.order=fs.order[,i], cutoff=cutoff, fig=fig,
               fig.title=paste(fig.title,i, sep="_"))
  })
  names(res.cor) <- fs.cname
  res.cor
}
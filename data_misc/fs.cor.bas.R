fs.cor.bas <- function(mat, cutoff=0.75,fig=T,...){
  fs.cor <- cor(mat)
  res <- list()
  if (ncol(fs.cor) <= 1) {    
    res$all <- fs.cor
  } else {
    hc  <- hclust(as.dist(1 - fs.cor))
    if (fig && ncol(fs.cor) > 2 ) {
      plot(hc, hang=-1,sub="", ylab="1 - correlation", xlab="Features",cex=0.6,...)
      abline(h=1-cutoff, col="red")
    }
    id  <- cutree(hc, h = 1 - cutoff)
    res <- lapply(unique(id), function(x){
      cent <- mat[,id == x, drop = FALSE]
      res  <- if (ncol(cent) < 2 ) NA  else cor(cent)
    })
  
    id <- sapply(res, function(x){if(!any(is.na(x))) TRUE else FALSE})
    if (all(id==FALSE)) {       
      res$all <- fs.cor
    } else {
      res <- res[id]
      names(res) <- paste("Cluster",1:length(res), sep="_")
      res$all <- fs.cor
    } 
  } 
  return(res)
}
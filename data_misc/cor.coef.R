cor.coef <- function(x,cutoff=0.75){
  co <- cor(x)
  co[upper.tri(co)] <- NA
  diag(co) <- NA
  co <- co[-1,-ncol(co),drop=F]
  ## idx <- which(abs(co) >= cutoff,arr.ind = T)
  idx <- which(co >= cutoff,arr.ind = T)
  if (length(idx) !=0) {
    fs1 <- rownames(co)[idx[,1]]
    fs2 <- colnames(co)[idx[,2]]
    res <- data.frame(com1=fs1, com2=fs2, cor=co[idx])
  } else res <- NA
  res
}
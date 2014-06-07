`fs.mrpval` <-
function(x,qtl=0.75)
{

#### input is an object from feat.rk.re
  x <- as.matrix(x$rank.list)
  
  avgrank  <- apply(x,1,mean)
  Ug       <- which(avgrank >= quantile(avgrank,probs=qtl))
  dnull    <- as.vector(x[Ug,])
  pval     <- rep(1,length(avgrank))
  
  for(k in 1:length(avgrank))
    pval[k] <- sum(dnull < avgrank[k])
    
  pval <- pval/length(dnull)
  names(pval) <- dimnames(x)[[1]]

fs.rank <- rank(abs(avgrank), na.last=T, ties.method="random")
fs.order <- order(fs.rank, na.last=T)

  names(fs.rank) <- names(avgrank)
  nam <- names(avgrank[fs.order])
  if (!is.null(nam))
     fs.order <- noquote(nam)

  
  res <- list(stats=avgrank,fs.rank=fs.rank,fs.order=fs.order,sdrank=apply(x,1,sd),mrpval=pval,Ug=Ug,nnull=length(dnull),qtl=qtl)

  return(res)
  
}


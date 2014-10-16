#' Significance of Feature Ranking
#' 
#' Computation of the pseudo mrp-value from a resampling based feature ranking
#' strategy. \code{qtl} represents the fraction of presumedly informative
#' features. The decision is based on the average rank across all resampling
#' steps. 1-\code{qtl} represents the fraction of features that serves to
#' estimate the null distribution of ranks (i.e. ranks of uninformative
#' variables).
#' 
#' 
#' @usage fs.mrpval(x,qtl=0.75)
#' @param x A list returned from \code{\link{feat.rank.re}}.
#' @param qtl A numeric value of probability with values in [0,1].
#' @return A list with components: \item{stats}{Original feature ranking
#' statistics.} \item{fs.rank}{Feature ranking vector.} \item{fs.order}{Feature
#' order vector.} \item{sdrank}{Feature rank standard deviation.}
#' \item{mrpval}{Individual feature mrp-value. } \item{Ug}{Uninformative
#' variables.} \item{nnull}{Total number of uninformative variables.}
#' \item{qtl}{Quantile \code{qtl} used.}
#' @author David Enot \email{dle@@aber.ac.uk} and Wanchang Lin
#' \email{wll@@aber.ac.uk}
#' @seealso \code{\link{feat.rank.re}}, \code{\link{fs.summary}}
#' @references Zhang, C., Lu,X. and Zhang, X. (2006). Significance of Gene
#' Ranking for Classification of Microarray Samples. \emph{IEEE/ACM
#' Transactions on Computational Biology and Bioinformatics}, VOL. 3, NO. 3,
#' pp. 312-320.
#' @keywords classif
#' @examples
#' 
#' ## load abr1
#' data(abr1)
#' y   <- factor(abr1$fact$class)
#' x <- preproc(abr1$pos , y=y, method=c("log10","TICnorm"),add=1)[,110:500]  
#' ## Select classes 1 and 2
#' dat <- dat.sel(x, y, choices=c("1","2"))
#' x <- dat$dat[[1]]
#' y <- dat$cl[[1]]
#' 
#' ## partitioning
#' pars   <- valipars(sampling="boot",niter=2,nreps=5)
#' tr.idx <- trainind(y,pars=pars)
#' 
#' ## multiple rankings using AUC
#' z      <- feat.rank.re(x,y,method="fs.auc",pars = pars,tr.idx=tr.idx)
#' 
#' ## Compute stability mr-p value using the 25% worst features as irrelevant
#' res <- fs.mrpval(z,qtl=0.75)
#' 
#' ## print content of res
#' names(res)
#' 
#' ## list of features to form the null distribution of ranks
#' print(res$Ug)
#' 
#' 
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


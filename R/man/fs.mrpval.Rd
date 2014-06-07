% wll-15-03-2007

\name{fs.mrpval}
\alias{fs.mrpval}
\title{
  Significance of Feature Ranking
}
\description{
  Computation of the pseudo mrp-value from a resampling based feature
  ranking strategy. \code{qtl} represents the fraction of presumedly 
  informative features. The decision is based on the average rank across all 
  resampling steps. 1-\code{qtl} represents the fraction of features that serves 
  to estimate the null distribution of ranks (i.e. ranks of uninformative variables).
}
\usage{
  fs.mrpval(x,qtl=0.75)
}

% --------------------------------------------------------------------
\arguments{
  \item{x}{
  A list returned from \code{\link{feat.rank.re}}.
  }
  \item{qtl}{ 
   A numeric value of probability with values in [0,1].
  }
}

% ----------------------------------------------------------------------------
\value{
  A list with components:
  \item{stats}{Original feature ranking statistics.}
  \item{fs.rank}{Feature ranking vector.}
  \item{fs.order}{Feature order vector.}
  \item{sdrank}{Feature rank standard deviation.}
  \item{mrpval}{Individual feature mrp-value. }
  \item{Ug}{Uninformative variables.}
  \item{nnull}{Total number of uninformative variables.}
  \item{qtl}{Quantile \code{qtl} used.}
}

% ----------------------------------------------------------------------------
\author{
  David Enot \email{dle@aber.ac.uk} and Wanchang Lin \email{wll@aber.ac.uk}
}

\references{
Zhang, C., Lu,X. and Zhang, X. (2006). Significance of Gene Ranking for 
Classification of Microarray Samples. \emph{IEEE/ACM Transactions on 
Computational Biology and Bioinformatics}, VOL. 3, NO. 3, pp. 312-320.
}

\seealso{
  \code{\link{feat.rank.re}}, \code{\link{fs.summary}}
}

% ----------------------------------------------------------------------
\examples{
## load abr1
data(abr1)
y   <- factor(abr1$fact$class)
x <- preproc(abr1$pos , y=y, method=c("log10","TICnorm"),add=1)[,110:500]  
## Select classes 1 and 2
dat <- dat.sel(x, y, choices=c("1","2"))
x <- dat$dat[[1]]
y <- dat$cl[[1]]

## partitioning
pars   <- valipars(sampling="boot",niter=2,nreps=5)
tr.idx <- trainind(y,pars=pars)

## multiple rankings using AUC
z      <- feat.rank.re(x,y,method="fs.auc",pars = pars,tr.idx=tr.idx)

## Compute stability mr-p value using the 25\% worst features as irrelevant
res <- fs.mrpval(z,qtl=0.75)

## print content of res
names(res)

## list of features to form the null distribution of ranks
print(res$Ug)

}

\keyword{classif}

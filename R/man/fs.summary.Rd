% 29-03-2007: 
%
\name{fs.summary}
\alias{fs.summary}
\title{
  Feature Ranking Resampling Summary
}
\description{
  Wrapper that aggregates results obtained from a feature ranking resampling strategy. 
  If p values have been calculated by 
the feature ranking technique on the overall dataset, adjusted p-values by one of 
the methods available in \code{\link{p.adjust}} are also returned.
}

\usage{
  fs.summary(res1,res2,padjust="fdr",sorting=TRUE)
}

% ----------------------------------------------------------------------------
\arguments{
  \item{res1}{A list returned from \code{\link{feat.rank.re}}.}
  \item{res2}{ A list returned from \code{\link{fs.mrpval}}.}
  \item{padjust}{One of the methods in \code{\link{p.adjust}}.}
  \item{sorting}{Should the results be sorted according to the
   feature ordering calculated on the overall data?}

}

% ----------------------------------------------------------------------------
\value{
  A matrix of statistics. For details, see \code{Note} below.
}

\note{
The output matrix with number of rows corresponding to the number of variables 
and number of columns to:
    \itemize{
    \item Feature ranking quantity computed on the whole dataset
    \item Feature rank in decreasing order of saliency
    \item p-value if available with feature ranking technique (optional)
    \item adjusted p-value if p-value available (optional)
    \item average feature rank across every resampling steps
    \item standard deviation of the feature rank across every resampling steps
    \item pseudo p-value calculated from the resampling strategy
    }
}

%% wll-19-09-2007: Can't put the section of \itemize into the section of 
%%                 \value{}. Is this kind of bug in R?


% ----------------------------------------------------------------------------
\author{
  David Enot and Wanchang Lin \email{dle, wll@aber.ac.uk}.
}

\seealso{
  \code{\link{feat.rank.re}}, \code{\link{fs.mrpval}}, \code{\link{p.adjust}}
}


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

## Compute stability mr-p value using the 75\% worst features as irrelevant
res <- fs.mrpval(z,qtl=0.25)

## print content of res
names(res)

res.1 <- fs.summary(z, res, sorting=TRUE)

## Print the 10 best features
print(res.1[1:10,])

##### Example of output with a feature ranking technique that returns p-value
z      <- feat.rank.re(x,y,method="fs.welch",pars = pars,tr.idx=tr.idx)
res <- fs.mrpval(z,qtl=0.25)
names(res)
## p-value correction with fdr
res.1 <- fs.summary(z, res, padjust = "fdr", sorting=TRUE)
## Print the 10 best features
res.1[1:10,]
}

\keyword{manip}

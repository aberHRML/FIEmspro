\name{summ.ftrank}
\alias{summ.ftrank}
\title{Summarise multiple resampling based feature ranking outputs}
\description{
  This routine performs further computations on a list of \code{feat.rank.re} objects contained in a 
  \code{mfr.obj} object (see \code{\link{ftrank.agg}}). If only one \code{feat.rank.re} result from \code{\link{feat.rank.re}} is analyzed, it is easier to pass it first to  \code{\link{ftrank.agg}} (see example).Two calculations are made: 1) Computation of the pseudo mrp-value from the resampling based feature (see \code{\link{fs.mrpval}}) and 2) adjusted p values if p values have calculated (see  \code{\link{p.adjust}}).
}
\usage{
summ.ftrank(lclas, lmod = NULL, qtl = 0.25, padjust = "fdr")
}
\arguments{
  \item{lclas}{mfr.obj object - See details in \code{\link{ftrank.agg}}}
  \item{lmod}{List of models to be considered in lclas}
  \item{qtl}{Quantile  - See details in \code{\link{fs.mrpval}}}
  \item{padjust}{p value adjustement method - See details in \code{\link{p.adjust}}}
}
\details{
The resulting list as two component: one is equal to the total number of \code{feat.rank.re} objects (i.e one resampling experiment) and one field is a table that summarises each \code{feat.rank.re} (as for \code{\link{ftrank.agg}}). In the first component, each table may have a different number of columns depending if the FR method outputs p-values or not:
\describe{
     \item{Stat:}{Original statistics calculated on the overall data.}
     \item{Rank:}{Original feature rank calculated on the overall data.}
     \item{pval:}{Original feature p-value calculated on the overall data (optional).}
     \item{pval-xxx:}{Feature adjusted p-value by method xxx if p-value available (i.e. previous column).}
     \item{mrpval-xxx:}{Pseudo multiple resampling p-value using a given \code{qtl} value xxx.}
     \item{AvgRk:}{Average feature rank calculated from the ranks found for each training data partition.}
     \item{SdevRk:}{Associated feature rank standard deviation calculated from the ranks found for each training data partition.}
}
}

\value{
  \code{mfr.sum} object:
    \item{frsum}{List of tables corresponding to each \code{feat.rank.re} object - See details}
    \item{frdef}{Summary of each \code{feat.rank.re} object as in  \code{\link{ftrank.agg}}}
}

\author{ David Enot \email{dle@aber.ac.uk}}
\seealso{\code{\link{fs.mrpval}}, \code{\link{tidy.ftrank}}, \code{\link{p.adjust}}}
\examples{
data(abr1)
y   <- factor(abr1$fact$class)
x <- preproc(abr1$pos , y=y, method=c("log10","TICnorm"),add=1)[,110:500]  
## Select classes 1 and 2
dat <- dat.sel1(x, y, pwise="1",mclass=NULL,pars=valipars(sampling="boot",niter=2,nreps=5))

reswelch = feat.rank.re(dat[[1]],method="fs.welch")
mfr=ftrank.agg(reswelch)
print(mfr$frdef)
frsum=summ.ftrank(mfr,lmod=1,qtl=.3)

## print the FR components
print(frsum$frdef)

## have a look at the first 5 variables
print(frsum$frsum[[1]][1:5,])

}
\keyword{manip}
\keyword{classif}

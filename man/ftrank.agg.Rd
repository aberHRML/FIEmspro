\name{ftrank.agg}
\alias{ftrank.agg}
\title{ Aggregation of resampling based feature ranking results}
\description{
 Aggregate \code{feat.rank.re} objects and list of  \code{feat.rank.re} objects to form \code{ftrank.agg} object. The main utilities of this function is to concatenate in a single list various results derived from several \code{feat.rank.re} calls in order to facilitate post analysis additional treatments and sorting of the results.
}
\usage{
ftrank.agg(...)
}
\arguments{
  \item{\dots}{\code{feat.rank.re} objects and/or list of  \code{feat.rank.re} objects}
}
\details{
 \code{ftdef} filed in the result list is a table with 9 columns which are automatically generated to summarise the content of each individual \code{feat.rank.re}. it is also aimed at  avoiding confusions if the same method is applied on the same discrimination problem but with different settings, different resampling partitioning or even different data sets. Each column is described as follows:
\describe{
    \item{\bold{Mod}:}{Unique identifier for each resampling based feature rankings.}
    \item{\bold{Alg}:}{Name of the classification technique as specified in the call of \code{\link{feat.rank.re}}.}
    \item{\bold{Arg}:}{Arguments passed to the FR technique during the call of \code{\link{feat.rank.re}}.}
    \item{\bold{Pars}:}{Summary of the resampling strategy adopted during the call of \code{\link{feat.rank.re}}.}
    \item{\bold{Dis}:}{Discrimination task involved. By default, this is equal to the actual levels of the class vector passed to \code{\link{feat.rank.re}} separated by \code{~}.}
    \item{\bold{AlgId}:}{Unique algorithm identifier based on the columns Alg, Arg and Pars so that no confusion is possible with Alg if several rankings have been built with the same FR technique but with different parameters and/or resampling strategy. This column can be modified by the user.}
    \item{\bold{DisId}:}{Unique algorithm identifier based on the columns Dis in order to simplified the name of the discrimination task if there are many classes involved and/or  class level have a long name. This column can be modified by the user.}
    \item{\bold{Other}:}{Empty column that can be amended to store extra information.}
}
}
\value{
  \code{ftrank.agg} objects:
    \item{ftrank}{List of \code{feat.rank.re} objects}
    \item{frdef}{Summary of each \code{feat.rank.re} object - See details}
}
\author{David Enot \email{dle@aber.ac.uk}}
\seealso{\code{\link{feat.rank.re}}}
\examples{
data(abr1)
y   <- factor(abr1$fact$class)
x <- preproc(abr1$pos , y=y, method=c("log10","TICnorm"),add=1)[,110:500]  
dat <- dat.sel1(x, y, pwise=list(c("1","2"),c("3","2")),mclass=NULL,
  pars=valipars(sampling="boot",niter=2,nreps=5))

resauc = lapply(dat, function(x) feat.rank.re(x,method="fs.auc"))
resrf = lapply(dat, function(x) feat.rank.re(x,method="fs.rf",ntree=100))

mfr=ftrank.agg(resauc,resrf)

### Print out characteristics of each individual FR objects
print(mfr$frdef)

### Number of objects in mfr
length(mfr$ftrank)

### This is FR object num.1
mfr$ftrank[[1]]
 
}
\keyword{manip}

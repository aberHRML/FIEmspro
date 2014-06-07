\name{mc.agg}
\alias{mc.agg}
\alias{mc.agg.default}
\alias{print.mc.agg}

\title{ Aggregation of classification results}
\description{
 Aggregate \code{accest} objects and list of  \code{accest} objects to form \code{mc.agg} object. The main utilities of this function is to concatenate in a single list various results derived from several \code{accest} calls in order to facilitate post analysis additional treatments as well as exporting the results.
}
\usage{
mc.agg(\dots)

\method{mc.agg}{default}(\dots)

}
\arguments{
  \item{\dots}{accest objects and or list of  accest objects}
}
\details{
 The length of the resulting list is equal to the total number of \code{accest} objects (i.e one resampling experiment) plus one field that summarises each \code{accest}. The later is a table with 8 columns which are automatically generated to and also avoid confusions if the same method is applied on the same discrimination problem but with different settings, different resampling partitioning or even different data sets. Note that columns 1 to 5 are automatically generated from the output of \code{\link{accest}} where as columns 6 is based on columns 2-4. Each column is described as follows:
\describe{
    \item{\bold{Mod}:}{Unique identifier for each resampling based feature rankings.}
    \item{\bold{Alg}:}{Name of the classification technique as specified in the call of \code{\link{accest}}.}
    \item{\bold{Arg}:}{Arguments passed to the classifier during the call of \code{\link{accest}}.}
    \item{\bold{Pars}:}{Summary of the resampling strategy adopted during the call of \code{\link{accest}}.}
    \item{\bold{Dis}:}{Discrimination task involved. By default, this is equal to the actual levels of the class vector passed to \code{\link{accest}} separated by \code{~}.}
    \item{\bold{AlgId}:}{Unique algorithm identifier based on the columns Alg, Arg and Pars so that no confusion is possible with Alg column if several classification have been performed with the same discrimination technique but with different parameters and/or resampling strategy. This column can be modified by the user.}
    \item{\bold{DisId}:}{Unique algorithm identifier based on the columns Dis in order to simplified the name of the discrimination task if there are many classes involved and/or  class level have a long name. This column can be modified by the user.}
    \item{\bold{Other}:}{Empty column that can be amended to store extra information.}
}
}
\value{
  \code{mc.agg} objects:
    \item{clas}{List of \code{accest} objects}
    \item{cldef}{Summary of each \code{accest} object - See details}
}
\author{David Enot \email{dle@aber.ac.uk}}
\seealso{\code{\link{accest}},\code{\link{mc.summary}}, \code{\link{mc.comp.1}}, \code{\link{mc.roc}}}
\examples{
data(iris)
dat=as.matrix(iris[,1:4])
cl=as.factor(iris[,5])
lrnd=sample(1:150)[1:50]
cl[lrnd]=sample(cl[lrnd])
pars   <- valipars(sampling = "boot",niter = 2, nreps=10)
dat1=dat.sel1(dat,cl,pwise=list(),mclass=list(),pars=pars)

res1=accest(dat1[[1]],clmeth="randomForest",ntree=100,seed=1)
res2=lapply(dat1,function(x) accest(x,clmeth="lda"))

mc=mc.agg(res1,res2)
###Print the content
mc

### Classification task num. 1:
mc$clas[[1]]

## As other functions are using the column 5 and 6 to sort and print the results,
## you can replace them by something more informative
## for e.g. Alg and Dis
mc$cldef[,6]<-mc$cldef[,2]
mc$cldef[,7]<-c("set~vir","set~vir","set~vir","ver~vir","ver~vir")
mc$cldef

}
\keyword{manip}

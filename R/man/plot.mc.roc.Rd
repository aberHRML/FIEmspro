\name{plot.mc.roc}
\alias{plot.mc.roc}
\title{Plot multiple ROC curves}
\description{
Plot multiple ROC curves contained in \code{mc.roc} objects (see details in \code{\link{mc.roc}}).
}
\usage{
\method{plot}{mc.roc}(x, leg = "Model", llty = NULL, lcol = NULL, xleg = 0.5, yleg = 0.4, ...)
}
\arguments{
  \item{x}{\code{mc.roc} object - See details in \code{\link{mc.roc}}}
  \item{leg}{User defined legend or select information from \code{x$cldef}}
  \item{llty}{List of symbols}
  \item{lcol}{List of colors}
  \item{xleg}{Upper left corner co-ordinate on the x-axis}
  \item{yleg}{Upper left corner co-ordinate on the y-axis}
  \item{\dots}{Further arguments to be passed to lines}
}
\value{
NULL
}
\author{David Enot \email{dle@aber.ac.uk}}
\seealso{\code{\link{mc.roc}}}
\examples{
data(iris)
dat=as.matrix(iris[,1:4])
cl=as.factor(iris[,5])
lrnd=sample(1:150)[1:50]
cl[lrnd]=sample(cl[lrnd])
pars   <- valipars(sampling = "cv",niter = 2, nreps=10)
dat1=dat.sel1(dat,cl,pwise="virginica",mclass=NULL,pars=pars)

res1=lapply(dat1,function(x) accest(x,clmeth="lda"))
res2=lapply(dat1,function(x) accest(x,clmeth="randomForest",ntree=50))
mc=mc.agg(res1,res2)

roc.sv=mc.roc(mc,lmod=1:4)

### Default plot
plot(roc.sv)

###Improve plotting by using the names contained in the "DisId" and "Alg"
plot(roc.sv,leg=c("DisId","Alg"),llty=c(1,1,2,2),lcol=c("blue","red","blue","red"))

###Improve plotting by setting the legend
plot(roc.sv,leg=c("roc1","roc2","roc3","roc4"),llty=c(1,1,2,2),lcol=c("blue","red","blue","red"))

}
\keyword{hplot}

\name{mc.roc}
\alias{mc.roc}
\alias{mc.roc.default}
\title{Generate ROC curves from several classifiers}
\description{
 Convenience function to generate a ROC curve from several runs and iterations for one model or a selection of models contained in a \code{mc.acc} object (see details \code{\link{mc.agg}}). This function allows the averaging of several ROC curves produced on each data partitioning of the resampling strategy (i.e cross-validation runs repeated or not several times). Three methods are available to perform the averaging: "horiz" (horizontal), "vert" (vertical) and "thres" (thresholding). (see reference for further details). The default value ("all") means that each data points from individual ROC curves are strictly concatenated into a single ROC curve. }
\usage{
mc.roc(mc.obj, lmod = 1, method = "all")
\method{mc.roc}{default}(mc.obj, lmod = 1, method = "all")
}
\arguments{
  \item{mc.obj}{\code{mc.agg} object  - See details \code{\link{mc.agg}}}
  \item{lmod}{List of models to be considered - Default: all of them}
  \item{method}{Aggregation method ("all", "thres", "vert", "horiz")}
}
\value{
\code{roc.list} object is a list of two components:
\item{roc}{List of ROC curves of length equal to the number of models. For each curve true positive (tpr), false positive rate (fpr), decision boundary threshold (thres) and type (type) of ROC aggregation are given.}
\item{cldef}{Identical to \code{cldef} in \code{\link{mc.agg}}.}
}
\author{David Enot \email{dle@aber.ac.uk}}
\references{
  Fawcett, T. (2004). ROC graphs: notes and practical considerations for researchers.
  \code{Technical Report HPL-2003-4}
}
\seealso{\code{\link{plot.mc.roc}},\code{\link{mc.agg}}}
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

roc.sv=mc.roc(mc,lmod=1:4,method="thres")
print(roc.sv)

}
\keyword{manip}

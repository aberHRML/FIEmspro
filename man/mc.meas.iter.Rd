\name{mc.meas.iter}
\alias{mc.meas.iter}
\title{Summary of a predictor in mc.agg object}
\description{
 Convenience function to output statistics related to accuracy, AUC or margins at each iteration for one model or a selection of models contained in a \code{mc.agg} object (see details \code{\link{mc.agg}}).
}
\usage{
mc.meas.iter(mc.obj, lmod = NULL,type="acc",nam="Model")
}
\arguments{
  \item{mc.obj}{\code{mc.agg} object  - See details \code{\link{mc.agg}}}
  \item{lmod}{List of models to be considered - Default: all models}
  \item{type}{Predictor type - Can be either acc (accuracy), auc (AUC), mar (margin or equivalent)}
  \item{nam}{List of names to be used in the result - Names given here corresponds to the column name of \code{mc.obj$cldef}}
}
\value{
Data frame containing statistic of interest at each iteration.
}
\author{David Enot \email{dle@aber.ac.uk}}
\seealso{\code{\link{mc.agg}}}
\examples{
data(iris)
dat=as.matrix(iris[,1:4])
cl=as.factor(iris[,5])
lrnd=sample(1:150)[1:50]
cl[lrnd]=sample(cl[lrnd])  ## add a bit of misclassification for fun
pars   <- valipars(sampling = "cv",niter = 10, nreps=4)
dat1=dat.sel1(dat,cl,pwise="virginica",mclass=NULL,pars=pars)

res1=lapply(dat1,function(x) accest(x,clmeth="lda"))
res2=lapply(dat1,function(x) accest(x,clmeth="randomForest",ntree=50))

## Aggregate res1 and res2
mc=mc.agg(res1,res2)

## AUC in each model
auc.iter<-mc.meas.iter(mc,type="auc",nam=c("DisId","Alg"))
## Plot them
boxplot(auc.iter)
## Print on the screen
print(auc.iter)
}
\keyword{manip}

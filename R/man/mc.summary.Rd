\name{mc.summary}
\alias{mc.summary}
\alias{mc.summary.default}
\alias{print.mc.summary}
\title{Summary of multiple classifiers objects}
\description{
 Convenience function to output statistics related to accuracy, AUC and margins for a selection of models. If \code{sortDis=TRUE}, results are grouped by discrimination task (value contained in DisId column of \code{mc.obj$cldef}). If \code{sortDis=FALSE}, results are grouped by classifier algorithm (value contained in column AlgId of \code{mc.obj$cldef}).
}
\usage{
mc.summary(mc.obj,lmod=NULL,sortDis=TRUE)

\method{mc.summary}{default}(mc.obj, lmod = NULL, sortDis = TRUE)

%\method{print}{mc.summary}(mc.anal,digits=3,file=NULL)
}
\arguments{
  \item{mc.obj}{\code{mc.agg} object}
  \item{lmod}{List of models to be considered - Default: all of them}
  \item{sortDis}{Should the results be sorted by discrimination task? If FALSE, results are group by classifier techniques}
%  \item{mc.anal}{}
 % \item{digits}{}
%  \item{file}{}
}
\value{
 \code{mc.summary} object:
  \item{res}{List of results}
  \item{cltask}{Discrimination task(s) or classification algorithm(s) used.}
  \item{title}{Title for printing function.}
}
\author{David Enot \email{dle@aber.ac.uk}}
\seealso{ \code{\link{mc.agg}}}
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

## Aggregate res1 and res2
mc=mc.agg(res1,res2)

## Sort results by discrimination task
mc.summary(mc)

## Sort results by algorithm
mc.summary(mc,sortDis=FALSE)

## See what is in
names(mc.summary(mc))

## Print results in a file
\dontrun{print(mc.summary(mc,sortDis=FALSE),digits=2,file="tmp.csv")}
}
\keyword{manip}

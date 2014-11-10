\name{mc.comp.1}
\alias{mc.comp.1}
\alias{mc.comp.1.default}
\alias{print.mc.comp.1}
\title{Multiple Classifier Predictions Comparison}
\description{
  Function to test for significant differences between predictions made by various classifiers (method and/or settings) built on the same partitioning schema. This function requires that explicit class predictions for each fold and iteration are contained in the classifier object of the type of \code{\link{accest}}. For each pairwise comparison: mean of the differences, variance associated, student t-statistics and corresponding p value are reported in a table. Subsequent multiple testing correction is applied if more than two classifiers are involved. Note that column \bold{DisId} is used to sort the classifiers according to the discrimination task and \bold{DisId} and \bold{AlgId} will be used to report the results. Of course, it is also assumed that partitioning for models built with two different classifiers is identical. 
}
\usage{
mc.comp.1(mc.obj,lmod=NULL,p.adjust.method="holm")

\method{mc.comp.1}{default}(mc.obj,lmod=NULL,p.adjust.method="holm")

%\method{print}{mc.comp.1}(mc.anal,digits=3,file=NULL)
}
\arguments{
  \item{mc.obj}{\code{mc.agg} object - See details \code{\link{mc.agg}}}
  \item{lmod}{List of models to be considered - Default: all of them}
  \item{p.adjust.method}{Multiple testing correction. See details in \code{\link{p.adjust}}}
%  \item{mc.anal}{}
 % \item{digits}{}
  %\item{file}{}
}
\value{
 \code{mc.comp.1} object:
  \item{res}{Summary of classifier pairwise comparisons for each discrimination task}
  \item{cltask}{Discrimination task(s).}
  \item{title}{Title for printing function.}
}
\references{
Berrar, D., Bradbury, I. and Dubitzky, W. (2006). Avoiding model selection bias in 
small-sample genomic datasets. \emph{Bioinformatics}. Vol.22, No.10, 1245-125.

Bouckaert, R.R.,and Frank, E., (2004). Evaluating the Replicability of Significance Tests for 
Comparing Learning Algorithms. 
\emph{Proc 8th Pacific-Asia Conference on Knowledge Discovery and Data Mining}. Vol.3054, 3-12
}
\author{David Enot \email{dle@aber.ac.uk}}
\note{ 
See publications mentioned below.
}
\seealso{ \code{\link{mc.agg}}}
\examples{

data(iris)
x <- as.matrix(subset(iris, select = -Species))
y <- iris$Species
pars   <- valipars(sampling = "cv",niter = 10, nreps=5, strat=TRUE)
tr.idx <- trainind(y,pars=pars)
## RF model based one tree
acc1   <- accest(x, y, clmeth ="randomForest", pars = pars, tr.idx=tr.idx,ntree=1)
## RF model based 100 trees
acc2   <- accest(x, y, clmeth = "randomForest", pars = pars, tr.idx=tr.idx,ntree=100)
### RF model where the minimum size of terminal nodes is set to a value greater 
## than the maximum number of samples per class (oups!)
acc3   <- accest(x,y, clmeth = "randomForest", pars = pars, tr.idx=tr.idx,ntree=1,nodesize=80)

clas=mc.agg(acc1,acc2,acc3)
res.comp<-mc.comp.1(clas,p.adjust.method="holm")

## No significant differences between 1 and 2
## Of course classifiers 1 and 2 performs significantly better than 3
## by default
res.comp

## with a few more decimals...
print(res.comp,digits=4)

## Print results in a file
\dontrun{print(res.comp,digits=2,file="tmp.csv")}


}
\keyword{htest}

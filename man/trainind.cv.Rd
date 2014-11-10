\name{trainind.cv}
\alias{trainind.cv}
\title{Constrained Generation of Training Samples Indices}

\description{
Special case of cross-validation where the fold are defined by the user. Data are partitioned according to values contained in \code{cv}.
}

\usage{
trainind.cv(cv)
}
\arguments{
  \item{cv}{Vector containing the fold information}
}

\value{
  Returns a list of training index. 
}

% ----------------------------------------------------------------------------
\author{
  David Enot \email{dle@aber.ac.uk}
}

\seealso{
\code{\link{trainind}},\code{\link{valipars}}, \code{\link{accest}}

}

\examples{
## Load abr1
data(abr1)

## cl is the class of interest
cl  <- factor(abr1$fact$class)
dat <- preproc(abr1$pos , y=cl, method=c("log10","TICnorm"),add=1)[,110:500]  

## For illustration, we use sample replicates id to form the CV folds
re  <- factor(abr1$fact$rep)

## Check representativity of cl in each fold
table(re,cl)

## Generate a trainind object using re
tr.idx <- trainind.cv(re)
pars   <- valipars(sampling="cv", niter=1, nreps=5)

## for fold num. 1
## check sample indices and replicates ids in the training set
tmp    <- tr.idx[[1]]
cl[tmp[[1]]];table(re[tmp[[1]]])     ## train idx
## and in the test set
cl[-tmp[[1]]];table(re[-tmp[[1]]])   ## test idx

##  accuracy estimation with constrained CV
acc  <- accest(dat, cl, pars = pars, clmeth = "nlda", tr.idx=tr.idx)
acc
## compare it with random CV
pars.rnd   <- valipars(sampling="cv", niter=10, nreps=5)
tr.idx.rnd <- trainind(cl,pars.rnd)
acc.rnd  <- accest(dat, cl, pars = pars.rnd, clmeth = "nlda", tr.idx=tr.idx.rnd)

## plot the histogram of the accuracies at each iteration
hist(acc.rnd$acc.iter)

}

\keyword{manip}

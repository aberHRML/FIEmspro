% wll-16-09-2006
%
\name{dat.sel1}
\alias{dat.sel1}
\title{
  Generate Data Set List
}
\description{
  Generate subset data sets based on class labels. This function allows generation of a selection of pairwise problems and/or multiple class problems. The main objective of this function is to alleviate generation of several variables containing data matrices, class information or validation strategy. Each subset can enter \code{\link{accest}} and \code{\link{feat.rank.re}} analysis without specifying both class and data matrix. Additionally, each subset can also contain validation parameters so that direct comparison between classifiers and feature ranking technique can be easily done.
}
\usage{
  dat.sel1(dat, cl, pwise = NULL, mclass = list(),pars=NULL)
}

% --------------------------------------------------------------------
\arguments{
  \item{dat}{
  A data frame or matrix. 
  }
  \item{cl}{
  A factor or vector of class.
  }
  \item{pwise}{
  The vector or list of class labels to be chosen for binary classification.
  }
  \item{mclass}{
  The vector or list of class labels to be chosen to be included in a multiple classification task.
  }
  \item{pars}{
  Partitioning information.
  }
}

\details{
  This function is used to provide the data set for the binary combination of the 
  class factor. If \code{pwise} is \code{list()}, the binary combination of for all 
  class labels will be done. If \code{pwise} has one class label, the 
  comparisons between this one and any other class are done. If \code{pwise} 
  has more than three class lables, enumerate the combinations or permutations 
  of the elements of \code{pwise}. For details, see \code{examples} below.
}

% ----------------------------------------------------------------------
\value{
  A list with components:
  \item{nam}{
  Discrimination task.
  }
  \item{dat}{
  Subset of the dataset.
  }
  \item{cl}{
  Class labels.
  }
  \item{pars}{
  Object of type pars.
  }  
  \item{tr.idx}{
  Object of type trainind.
  }
  \item{lsamp}{
  Sample ids in the original data matrix.
  }
 
}

% ----------------------------------------------------------------------------
\author{
  David \email{dle@aber.ac.uk}
}

% ----------------------------------------------------------------------
\examples{
data(abr1)
x<-abr1$pos[,110:120]
y<-factor(abr1$fact$day) 

## generate data set with all pairwise containing class "1"
dat1 <- dat.sel1(x,y,pwise="1",mclass=NULL)
unlist(lapply(dat1,function(x) x$name))

## generate data set with pairwise between classes "1" and "H"
dat1 <- dat.sel1(x,y,pwise=c("1","H"),mclass=NULL)
unlist(lapply(dat1,function(x) x$name))

## generate data set with pairwise between classes "1", "2", "H"
dat1 <- dat.sel1(x,y,pwise=c("2","1","H"),mclass=NULL)
unlist(lapply(dat1,function(x) x$name))

## generate data set with all pairwises containing "1" and "H"
dat1 <- dat.sel1(x,y,pwise=list("1","H"),mclass=NULL)
unlist(lapply(dat1,function(x) x$name))

## generate data set with 3 classes "1", "2" and "H"
dat1 <- dat.sel1(x,y,pwise=NULL,mclass=c("1","2","H"))
unlist(lapply(dat1,function(x) x$name))

## generate data set with all pairwises containing "1" and "H"
## on which partitioning is a 1*5CV
pars=valipars(sampling = "cv", niter = 1, nreps = 5)
dat1 <- dat.sel1(x,y,pwise=list("1","H"),mclass=NULL, pars=pars)
unlist(lapply(dat1,function(x) x$name))

print(dat1[[1]])

}

\keyword{manip}

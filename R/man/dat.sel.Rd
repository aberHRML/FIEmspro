% wll-16-09-2006
%
\name{dat.sel}
\alias{dat.sel}
\title{
  Generate Pairwise Data Set Based on Class Labels
}
\description{
  Generate pairwise data set based on class labels.
}
\usage{
  dat.sel(dat, cl, choices = NULL)
}

% --------------------------------------------------------------------
\arguments{
  \item{dat}{
  A data frame or matrix. 
  }
  \item{cl}{
  A factor or vector of class.
  }
  \item{choices}{
  The vector or list of class labels to be chosen for binary classification.
  }
}

\details{
  This function is used to provide the data set for the binary combination of the 
  class factor. If \code{choices} is \code{NULL}, the binary combination of for all 
  class labels will be done. If \code{choices} has one class label, the 
  comparisons between this one and any other class are done. If \code{choices} 
  has more than three class lables, enumerate the combinations or permutations 
  of the elements of \code{choices}. For details, see \code{examples} below.
}

% ----------------------------------------------------------------------
\value{
  A list with components:
  \item{dat}{
  Pairwise data set.
  }
  \item{cl}{
  Pairwise class label.
  }
  \item{com}{
  A matrix of the combinations or permutations of the elements of pairwise vector.
  }
}

% ----------------------------------------------------------------------------
\author{
  Wanchang Lin \email{wll@aber.ac.uk}
}

% ----------------------------------------------------------------------
\examples{
data(iris)
x <- subset(iris, select = -Species)
y <- iris$Species
 
## generate data set with class "setosa" and "virginica"
(binmat.1 <- dat.sel(x,y,choices=c("setosa","virginica")))

## generate data sets for "setosa" vs other classes. These are: 
## "setosa" and "versicolor", "setosa" and "virginica".
(binmat.2 <- dat.sel(x,y,choices=c("setosa")))

## generate data set with combination of each class. These are:  
## "setosa" and "versicolor", "setosa" and "virginica",  
## "versicolor" and "virginica" 
(binmat.3 <- dat.sel(x,y,choices= NULL))

}

\keyword{manip}

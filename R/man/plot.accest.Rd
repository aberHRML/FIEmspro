% wll-17-09-2007
%
\name{plot.accest}
\alias{plot.accest}
\title{
  Plot Method for Class 'accest'
}
\description{
  Plots accuracy, margin or AUC at each iteration.
}

\usage{
  \method{plot}{accest}(x, toplot="acc", main = NULL, xlab = NULL, ylab = NULL, \dots)
}

% ----------------------------------------------------------------------------
\arguments{
  \item{x}{
  An object of class \code{accest}.
  }
  \item{toplot}{Quantity to plot (default=acc, mar, auc) }
  
  \item{main}{
  An overall title for the plot.  
  }
  \item{xlab}{
   A title for the x axis.
  }
  \item{ylab}{
  A title for the y axis.
  }
  \item{\dots}{
  Additional arguments to the plot. 
  }
}

% ----------------------------------------------------------------------------
\details{
This function is a method for the generic function \code{plot()} for class 
\code{accest}. It plots a performance estimate against iteration index. 
}

% ----------------------------------------------------------------------------
\author{
  David Enot and Wanchang Lin \email{dle,wll@aber.ac.uk}.
}


\seealso{
  \code{\link{accest}}, \code{\link{accest}}
}

\examples{
# Iris data
data(iris)
x <- as.matrix(subset(iris, select = -Species))
y <- iris$Species
pars <- valipars(sampling="cv", niter=10, nreps=5, strat=TRUE)

acc  <- accest(x,y, clmeth = "randomForest", pars = pars, ntree=50)
acc
## plot accuracies
plot(acc)

## plot margins
plot(acc,toplot="mar")
}

\keyword{hplot}

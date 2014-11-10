% wll-26-02-2007
% dle-15-09-2007

\name{koptimp}
\alias{koptimp}
\title{
  Imputation of Low Values
}

\description{
  Wrapper function to select an optimal number of neighbours (\code{k}) in \code{impute.knn} 
  from the IMPUTE package. For several values of \code{k}, predictions made on 
  random data points by \code{impute.knn} are compared to their original value to 
  calculate the root mean squared error. In the original matrix, \code{thres} 
  corresponds to the limit under which intensities are considered missing. \code{perc} 
  represents the percentage of "non missing" intensities randomly selected to 
  estimate RMSE. The optimal number \code{koptim} corresponds to number of \code{k} 
  that improves RMSE by less than 10\%. This value is automatically used for 
  computing the resulting matrix \code{x} matrix.
}

\usage{
  koptimp(x,thres=1,log.t=TRUE,lk=3:10,perc=0.1,niter=10,\dots)
}

\arguments{
  \item{x}{A data frame or matrix to be imputed. }
  \item{thres}{Threshold below which intensities in \code{x} are considered missing.}
  \item{log.t}{ 
  A logical which specifies whether or not the log transformation 
  is performed on the data set before imputation.
  }
  \item{lk}{A vector of numbers of neighbours to be tested.}
  \item{perc}{Percentage of non-low value to be randomly selected.}
  \item{niter}{Number of iteration.}
  \item{\dots}{Arguments passed to or from other methods.}
}

\value{
  A list containing the following components:
  \item{x}{An imputed data matrix using \code{k=koptim}.}
  \item{koptim}{Optimal number of neighbors found in \code{lk}. }
  \item{rmse}{Root mean squared error matrix (\code{niter} by length of \code{lk}).}
}

\note{Version of package \code{impute} must be 1.8.0 or greater. At the moment of the package writing, only the package available on the Bioconductor website seemed to be regularly updated}

\references{
  Hastie, T., Tibshirani, R., Sherlock, G., Eisen, M., Brown, P. and
  Botstein, D.(1999). Imputing Missing Data for Gene Expression Arrays,
  \emph{Stanford University Statistics Department Technical report}.
  http://www-stat.stanford.edu/~hastie/Papers/missing.pdf 
  
  Olga Troyanskaya, Michael Cantor, Gavin Sherlock, Pat Brown,
  Trevor Hastie, Robert Tibshirani, David Botstein and Russ B. Altman, (2001). 
  Missing value estimation methods for DNA microarrays. \emph{Bioinformatics}. 
  Vol. 17, no. 6, Pages 520-525. 
}

\author{
  David Enot \email{dle@aber.ac.uk}
}

\examples{
  ## load data
  data(abr1)
  mat <- abr1$pos[,110:300]

 ## find an optimal number of k between 3 and 6 to impute values lower than 1
 ## 10 perc. of intensities >1 are used to evaluate each solution
 ## imputation is done with the log transformed matrix
  res <- koptimp(mat,thres=1,log.t=TRUE,lk=3:6,perc=0.1,niter=5)
  names(res)
  
  ## check RMSE of the solutions at various k
  boxplot(res$rmse,xlab="Number of neighbours",ylab="Root mean square error")

  ## Do the imputation with a given k
  ## thres=1 and log.t=TRUE
  mat[mat <= 1] <- NA ; mat <- log(mat) 
  ## uses k=6 for example
  mimp <- t(impute.knn(t(mat), k = 6, 1, 1, maxp = ncol(mat))$data) 
  ## transform to the original space
  mimp <- exp(mimp)
}

\keyword{manip}

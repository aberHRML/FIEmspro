% lwc-14-09-2006: First draft
% lwc-27-10-2006: change name and minor changes.
%
\name{valipars}
\alias{valipars}

\title{Generate Control Parameters For Validation / Resampling}
\description{
  Generate the control parameters for resampling or validation process. 
}

% ----------------------------------------------------------------------------
\usage{
valipars(sampling="cv", niter=10, nreps=10, strat=FALSE,div = 2/3) 
}

% ----------------------------------------------------------------------------
\arguments{
  \item{sampling}{Sampling scheme. Valid options are:
    \itemize{
      \item \code{loocv}. Leave-one-out cross-validation
      \item \code{cv}. K-fold cross-validation  (default)
      \item \code{rand}. Randomised validation (holdout)
      \item \code{boot}. Bootstrap
    }
  }
  \item{niter}{Number of iteration or repeat for validation.}
  \item{nreps}{
    Number of replications in each iteration (number of folds for \code{sampling=cv} and 
    bootstrap for \code{sampling=boot}).
  }
  \item{strat}{
    A logical value indicating if stratification is applied to 
    \code{sampling=cv, rand} and \code{boot}.
  }
  \item{div}{
    Proportion of training data randomly selected for \code{sampling=rand}.
  }   
}

% ----------------------------------------------------------------------------
\details{
  \code{valipars} provides a list of control parameters for the resampling or validation 
  in the process of accuracy evaluation or feature selection process. 
}

% ----------------------------------------------------------------------------
\value{
  An object of class \code{valipars} containing all the above
  parameters (either the defaults or the user specified values).
}

% ----------------------------------------------------------------------------
\author{
  Wanchang Lin \email{wll@aber.ac.uk}
}

\seealso{
\code{\link{trainind}}, \code{\link{accest}}
} 

\examples{
## generate control parameters for the re-sampling scheme with 5-fold 
## cross-validation and iteration of 10 times
valipars(sampling = "cv", niter = 10, nreps = 5)

## generate control parameters for the re-sampling scheme with 
## 25-replication bootstrap and iteration of 100 times
valipars(sampling = "boot", niter = 100, nreps = 25,strat=TRUE)

## generate control parameters for the re-sampling scheme with 
## leave-one-out cross-validation
valipars(sampling = "loocv")

}

\keyword{manip}

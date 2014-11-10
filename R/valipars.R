#' Generate Control Parameters For Validation / Resampling
#' 
#' Generate the control parameters for resampling or validation process.
#' 
#' \code{valipars} provides a list of control parameters for the resampling or
#' validation in the process of accuracy evaluation or feature selection
#' process.
#' 
#' @usage valipars(sampling="cv", niter=10, nreps=10, strat=FALSE,div = 2/3)
#' @param sampling Sampling scheme. Valid options are: \itemize{ \item
#' \code{loocv}. Leave-one-out cross-validation \item \code{cv}. K-fold
#' cross-validation (default) \item \code{rand}. Randomised validation
#' (holdout) \item \code{boot}. Bootstrap }
#' @param niter Number of iteration or repeat for validation.
#' @param nreps Number of replications in each iteration (number of folds for
#' \code{sampling=cv} and bootstrap for \code{sampling=boot}).
#' @param strat A logical value indicating if stratification is applied to
#' \code{sampling=cv, rand} and \code{boot}.
#' @param div Proportion of training data randomly selected for
#' \code{sampling=rand}.
#' @return An object of class \code{valipars} containing all the above
#' parameters (either the defaults or the user specified values).
#' @author Wanchang Lin \email{wll@@aber.ac.uk}
#' @seealso \code{\link{trainind}}, \code{\link{accest}}
#' @keywords manip
#' @examples
#' 
#' ## generate control parameters for the re-sampling scheme with 5-fold 
#' ## cross-validation and iteration of 10 times
#' valipars(sampling = "cv", niter = 10, nreps = 5)
#' 
#' ## generate control parameters for the re-sampling scheme with 
#' ## 25-replication bootstrap and iteration of 100 times
#' valipars(sampling = "boot", niter = 100, nreps = 25,strat=TRUE)
#' 
#' ## generate control parameters for the re-sampling scheme with 
#' ## leave-one-out cross-validation
#' valipars(sampling = "loocv")
#' 
#' 
`valipars` <-
function(sampling = "cv", niter = 10, nreps = 10, 
                     strat = FALSE,div = 2/3) 
{
  if (sampling == "cv" && nreps < 2)
       stop("Number of fold (nreps) for cv must greater than 1")

  sampling <- match.arg(sampling, c("loocv","cv","boot","rand"))
  
  if (sampling == "loocv"){
    res <- list(sampling=sampling, niter=1)
  } else if (sampling == "rand"){
    res <- list(sampling=sampling, niter=niter, nreps=nreps, strat=strat,div=div)
  } else {
    res <- list(sampling=sampling, niter=niter, nreps=nreps, strat=strat)
  }  
  
  class(res) <- "valipars"
  return(res)
}


#' Imputation of Low Values
#' 
#' Wrapper function to select an optimal number of neighbours (\code{k}) in
#' \code{impute.knn} from the IMPUTE package. For several values of \code{k},
#' predictions made on random data points by \code{impute.knn} are compared to
#' their original value to calculate the root mean squared error. In the
#' original matrix, \code{thres} corresponds to the limit under which
#' intensities are considered missing. \code{perc} represents the percentage of
#' "non missing" intensities randomly selected to estimate RMSE. The optimal
#' number \code{koptim} corresponds to number of \code{k} that improves RMSE by
#' less than 10\%. This value is automatically used for computing the resulting
#' matrix \code{x} matrix.
#' 
#' 
#' @usage koptimp(x,thres=1,log.t=TRUE,lk=3:10,perc=0.1,niter=10,\dots{})
#' @param x A data frame or matrix to be imputed.
#' @param thres Threshold below which intensities in \code{x} are considered
#' missing.
#' @param log.t A logical which specifies whether or not the log transformation
#' is performed on the data set before imputation.
#' @param lk A vector of numbers of neighbours to be tested.
#' @param perc Percentage of non-low value to be randomly selected.
#' @param niter Number of iteration.
#' @param \dots Arguments passed to or from other methods.
#' @return A list containing the following components: \item{x}{An imputed data
#' matrix using \code{k=koptim}.} \item{koptim}{Optimal number of neighbors
#' found in \code{lk}. } \item{rmse}{Root mean squared error matrix
#' (\code{niter} by length of \code{lk}).}
#' @note Version of package \code{impute} must be 1.8.0 or greater. At the
#' moment of the package writing, only the package available on the
#' Bioconductor website seemed to be regularly updated
#' @author David Enot \email{dle@@aber.ac.uk}
#' @references Hastie, T., Tibshirani, R., Sherlock, G., Eisen, M., Brown, P.
#' and Botstein, D.(1999). Imputing Missing Data for Gene Expression Arrays,
#' \emph{Stanford University Statistics Department Technical report}.
#' http://www-stat.stanford.edu/~hastie/Papers/missing.pdf
#' 
#' Olga Troyanskaya, Michael Cantor, Gavin Sherlock, Pat Brown, Trevor Hastie,
#' Robert Tibshirani, David Botstein and Russ B. Altman, (2001).  Missing value
#' estimation methods for DNA microarrays. \emph{Bioinformatics}.  Vol. 17, no.
#' 6, Pages 520-525.
#' @keywords manip
#' @examples
#' 
#'   ## load data
#'   data(abr1)
#'   mat <- abr1$pos[,110:300]
#' 
#'  ## find an optimal number of k between 3 and 6 to impute values lower than 1
#'  ## 10 perc. of intensities >1 are used to evaluate each solution
#'  ## imputation is done with the log transformed matrix
#'   res <- koptimp(mat,thres=1,log.t=TRUE,lk=3:6,perc=0.1,niter=5)
#'   names(res)
#'   
#'   ## check RMSE of the solutions at various k
#'   boxplot(res$rmse,xlab="Number of neighbours",ylab="Root mean square error")
#' 
#'   ## Do the imputation with a given k
#'   ## thres=1 and log.t=TRUE
#'   mat[mat <= 1] <- NA ; mat <- log(mat) 
#'   ## uses k=6 for example
#'   mimp <- t(impute.knn(t(mat), k = 6, 1, 1, maxp = ncol(mat))$data) 
#'   ## transform to the original space
#'   mimp <- exp(mimp)
#' 
`koptimp` <- function(x,thres=1,log.t=TRUE,lk=3:10,perc=0.1,niter=10,...)
{
  x <- as.matrix(x)  
  
  x[x <= thres] <- NA
  if(log.t) x <- log(x)
  ncells <- sum(!is.na(x))
  lmiss  <- which(is.na(x),arr.ind=T)  ## array indices of NA
  lnmiss <- which(!is.na(x))
  rmse    <- NULL

  cat("Iteration (",niter,"):",sep="")
  for(k in 1:niter){
    cat(" ", k, sep = "")
    flush.console()   ## for Windows

    ## generate a random seed for this iteration
    curr.seed=sum(proc.time(),na.rm=T)
    set.seed(curr.seed)
    lmissr = sample(lnmiss)[1:floor(perc*ncells)]
    m2 = x
    m2[lmissr] = NA
    tmp <- NULL
    for(i in lk){
      set.seed(curr.seed)
      mimp <- t(impute.knn(t(m2),k=i,1,1,maxp=ncol(x))$data)
      tmp  <- c(tmp,sqrt(mean((mimp[lmissr]-x[lmissr])^2)))
    }
    rmse = rbind(rmse,tmp)
  }
  cat("\n")

  rownames(rmse) <- NULL      ## wll-15-09-07: aoid warning 
  rmse <- data.frame(rmse)  ## convert to data frame for boxplot
  dimnames(rmse) <- list(paste("iter-",1:niter, sep=""),lk)
  
  ## find the optimal number of neighbors
  koptim1 <- lk[which.min(apply(rmse,2,mean))]
  koptim2 <- lk[which(c(diff(apply(rmse,2,mean)),0)/apply(rmse,2,mean)>-0.01)[1]]
  koptim  <- min(c(koptim1,koptim2))
  
  ## main <- paste("RMSE (Optimum number of k:",koptim, ")", sep="")  
  boxplot(rmse, ylab = paste("RMSE over ",niter, " runs", sep=""),
          xlab = "Number of neighbours",...)

  ## finally, impute the low value using the optimal number of neighbors
  mimp   <- t(impute.knn(t(x),k=koptim,1,1,maxp=ncol(x))$data)
  
  if(log.t) mimp <- exp(mimp)

  return(list(x=mimp,koptim=koptim,rmse=rmse))
}


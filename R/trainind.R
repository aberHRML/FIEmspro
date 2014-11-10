#' Generation of Training Samples Indices
#' 
#' Generate training samples indices. The sampling scheme includes
#' leave-one-out cross-validation (\code{loocv}), cross-validation (\code{cv}),
#' randomised validation (\code{random}) and bootstrap (\code{boot}).
#' 
#' 
#' @usage trainind(cl, pars = valipars())
#' @param cl A factor or vector of class.
#' @param pars A list of sampling parameters for generating training index. It
#' has the same structure as the output of \code{valipars}. See \code{valipars}
#' for details.
#' @return Returns a list of training indices.
#' @note
#' 
#' To avoid any errors when using subsequent classification techniques or
#' others, training partitions contain at least two members of each class and
#' prediction sets at least one sample of each class. Therefore, a minimum of 3
#' samples per class is required.  As \code{trainind} is a fairly fast, hanging
#' during the function may come from a low number of replicate in a class
#' and/or the choice of an inadequate value (\code{nreps}).
#' @author Wanchang Lin \email{wll@@aber.ac.uk}
#' @seealso \code{\link{valipars}}, \code{\link{accest}}
#' @keywords manip
#' @examples
#' 
#' ## A trivia example
#' x <- as.factor(sample(c("a","b"), 20, replace=TRUE))
#' table(x)
#' pars <- valipars(sampling="rand", niter=2, nreps=4, strat=TRUE,div=2/3)
#' (temp <- trainind(x,pars=pars))
#' (tmp  <- temp[[1]])
#' x[tmp[[1]]];table(x[tmp[[1]]])     ## train idx
#' x[tmp[[2]]];table(x[tmp[[2]]])
#' x[tmp[[3]]];table(x[tmp[[3]]])
#' x[tmp[[4]]];table(x[tmp[[4]]])
#' 
#' x[-tmp[[1]]];table(x[-tmp[[1]]])   ## test idx
#' x[-tmp[[2]]];table(x[-tmp[[2]]])
#' x[-tmp[[3]]];table(x[-tmp[[3]]])
#' x[-tmp[[4]]];table(x[-tmp[[4]]])
#' 
#' # iris data set
#' data(iris)
#' dat <- subset(iris, select = -Species)
#' cl  <- iris$Species
#' 
#' ## generate 5-fold cross-validation samples
#' cv.idx <- trainind(cl, pars = valipars(sampling="cv", niter=2, nreps=5))
#' 
#' ## generate leave-one-out cross-validation samples
#' loocv.idx <- trainind(cl, pars = valipars(sampling = "loocv"))
#' 
#' ## generate bootstrap samples with 25 replications
#' boot.idx <- trainind(cl, pars = valipars(sampling = "boot", niter=2,
#'                                            nreps=25))
#' 
#' ## generate randomised samples with 1/4 division and 10 replications. 
#' rand.idx <- trainind(cl, pars = valipars(sampling = "rand", niter=2, 
#'                                            nreps=10, div = 1/4))
#' 
#' 
#' 
`trainind` <-
function(cl, pars = valipars())
{
  if (!inherits(pars, "valipars")) 
    stop("pars not of class valipars")
  
  cl <- factor(cl)    ## wll-17-09-2007: drop the factor levels
    
  ## ----------------------------------------------------------------------
  idx.func <- function(cl,pars=valipars()) { 
    n         <- length(cl)

    if (pars$sampling == "loocv"){
       train.ind <- lapply(1:n, function(i) seq(1,n)[-i])
    } else {    
      ## 1) each class must have at least 2 samples in training index
      ## 2) each class must have at least 1 sample in test index
      emp_f <- T
      while (emp_f) {     
        emp_f <- !emp_f
        switch(pars$sampling,
               "cv"    = {train.ind <- cv.idx(cl, pars$nreps, strat=pars$strat)},
               "rand"  = {train.ind <- rand.idx(cl, pars$nreps, strat=pars$strat, div=pars$div)},
               "boot"  = {train.ind <- boot.idx(cl, pars$nreps, strat=pars$strat)}
               )
        for (i in 1:length(train.ind)) {
          if (any(table(cl[train.ind[[i]]]) < 2) || any(table(cl[-train.ind[[i]]]) < 1)) { 
            emp_f <- !emp_f
            break
          } 
        }  ## end of for
      }  ## end of while
    }  ## end of else

    return(train.ind)
  }  
  ## ----------------------------------------------------------------------
  
  tr.idx <- list()
  for (i in 1:pars$niter) {
    tr.idx[[i]] <- idx.func(cl,pars=pars)
  }
  names(tr.idx) <- paste("Iter_",1:pars$niter,sep="")  

  return(tr.idx)

}


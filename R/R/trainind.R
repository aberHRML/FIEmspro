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


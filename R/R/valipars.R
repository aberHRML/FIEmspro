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


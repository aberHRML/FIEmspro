`cv.idx` <-
function(x, nreps,strat=FALSE) 
{
  ## ---------------- from package ipred ----------------------------
  ## One change has been made to get different results for each calling.
  ssubset <- function(y, k, strat=TRUE) {
    if (!is.factor(y)) stop("y is not of class factor")
    N <- length(y)
    nlevel <- table(y)
    nindx <- list()
    ## ------- Changed by Wanchang Lin, 29-10-2006 -------
    indx <- sample(1:N,N,replace=F)
    y    <- y[indx]
    ##  indx <- 1:N
    ## ---------------------------------------------------
    outindx <- list()
    if (strat) {
      for (j in 1:length(nlevel))
        nindx <- c(nindx, list(indx[which(y == levels(y)[j])]))
      kmat <- kfoldcv(k, N, nlevel)
      for (i in 1:k) {
        sset <- kmat[,i]
        kindx <- c()
        for (j in 1:length(nlevel)) {
          if (i > 1)
            kindx <- c(kindx, nindx[[j]][(sum(kmat[j,
                       1:(i-1)])+1):sum(kmat[j,1:i])])
          else
            kindx <- c(kindx, nindx[[j]][1:kmat[j,1]])
        }
        kindx <- kindx[!is.na(kindx)]
        outindx <- c(outindx, list(kindx))
      }
      return(outindx)
    } else {
      kmat <- kfoldcv(k, N)
      nindx <- indx
      for (i in 1:k) { 
        if (i > 1)
          outindx <- c(outindx,
                    list(nindx[(sum(kmat[1:(i-1)])+1):sum(kmat[1:i])]))
        else
          outindx <- c(outindx, list(nindx[1:kmat[1]]))
      }
    }
    return(outindx)
  }
  
  ## ----------------- from package ipred ---------------------
  kfoldcv <- function(k,N, nlevel=NULL) {
    if (is.null(nlevel)) {
      # no stratification
      if (k > N) return(c(rep(1, N), rep(0, k-N)))
      fl <- floor(N/k)
      ce <- ceiling(N/k)
      if (fl == ce) return(rep(fl, k)) 
        else 
      return(c(rep(ce, round((N/k - fl)*k)), rep(fl, round((1 - (N/k -
                       fl))*k))))
    } else {
      # stratification
      # if (!is.integer(nlevel)) stop("nlevel is not a vector if integers")
      kmat <- matrix(0, ncol=k, nrow=length(nlevel))
      for (i in 1:length(nlevel))
        kmat[i,] <- kfoldcv(k, nlevel[i])
      return(kmat)
    }
  }
  ## ----------------------------------------------------------

  n <- length(x)
  ## get index of test
  test.ind  <- ssubset(x, nreps, strat=strat)  
  ## get index of training
  train.ind <- lapply(1:nreps, function(i) seq(1,n)[-test.ind[[i]]]) 
  ## shuffl the results
  train.ind <- lapply(train.ind, function(x) sample(x, length(x), replace=F))
  return(train.ind)
}


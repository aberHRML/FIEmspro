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


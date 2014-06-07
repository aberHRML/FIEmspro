`rand.idx` <-
function(x, nreps,strat=FALSE,div=2/3) 
{
  n <- length(x)
  
  if (strat) {
    x   <- factor(x)                ## drops the levels that do not occur
    idx <- sample(1:n,n,replace=F)  ## shuffl the original x, ## idx  <- c(1:n)
    x   <- x[idx]
    
    v     <- length(levels(x))
    ## index of each factor
    s.idx <- lapply(1:v, function(i) idx[which(x == levels(x)[i])])
    
    train.ind <- lapply(1:nreps, function(y){  ## y is not used.
                 tmp <- lapply(s.idx, function(x) sample(x, trunc(length(x)*div), replace=F))
                 do.call("c", tmp)
                 })
    ## shuffl the results
    train.ind <- lapply(train.ind, function(x) sample(x, length(x), replace=F))
  } else {
    train.ind <- lapply(1:nreps, function(x) sample(1:n, trunc(n * div), replace=F))
  }
  
  return(train.ind)
}


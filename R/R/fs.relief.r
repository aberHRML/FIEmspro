## ==================================================================
## lwc-06-04-07: Feature selection using RELIEF
fs.relief <- function(x, y)
{
  ## -------------------------------------------------------------------
  ## Find the nearest neighbor from a matrix
  nearest <- function(x, mat){
    ## Euclidean distance
    dis  <- sapply(as.data.frame(t(mat)), function(y) sqrt(sum((x-y)^2)))
    ind  <- which.min(dis)
    return(mat[ind, ,drop=T])
  }
  ## -------------------------------------------------------------------

  if (!is.matrix(x)) x <- as.matrix(x)
  if (!is.factor(y)) y <- as.factor(y)
  if (length(y) != nrow(x))
    stop("x and y is not consistent.")

  n    <- nrow(x)
  p    <- ncol(x)
  gp   <- levels(y)
  prio <- table(y)/n  ## Computing the priors

  ## Calculating the range of each feature. range = Max - Min
  rng <- sapply(as.data.frame(x), function(x) diff(range(x)))

  weight <- rep(0, p)
  for(i in 1:n) {
    ## split x by group
    dat <- split.data.frame(x[-i,,drop=F],y[-i])

    ## find nearest neighbours
    near <- lapply(dat, function(z) nearest(x[i,], z))

    hit  <- gp[gp==y[i]]
    miss <- gp[gp!=y[i]]

    delta <- rep(0,p)
    for (j in 1:p) {
      diff.hit  <- - abs(x[i,][j] - near[[hit]][j])
      diff.miss <- lapply(miss, function(z) {
        prio[z] * abs(x[i,][j] - near[[z]][j])
      })
      diff.miss <- do.call("sum", diff.miss)
      diff.miss <- diff.miss/(1 - prio[hit])

      delta[j]  <- (1/n) * ((diff.hit + diff.miss)/rng[j])
    }
    ## updat weight
    weight <- weight + delta
  }

  names(weight) <- colnames(x)
  fs.order <- order(weight,decreasing=T, na.last=T)
  fs.rank  <- order(fs.order)

  names(fs.rank) <- names(weight)
  nam <- names(weight[fs.order])
  if (!is.null(nam))
     fs.order <- noquote(nam)

  res <- list(fs.rank=fs.rank, fs.order=fs.order, stats=weight)
  return(res)
}

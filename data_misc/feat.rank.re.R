feat.rank.re <- function(x,y,method,pars=valipars(),tr.idx=NULL,...)
{
  if (missing(x) || missing(y))
    stop("data set or class are missing")
  if(length(dim(x)) != 2)
    stop("'x' must be a matrix or data frame")
  y <- as.factor(y)
  if (nrow(x) != length(y)) stop("x and y don't match.")
  if (length(unique(y)) < 2)
    stop("Classification needs at least two classes.")
  if (any(is.na(x)) || any(is.na(y)))
    stop("NA is not permitted in data set or class labels.")

  n   <- nrow(x)
  p   <- ncol(x)

  if(is.null(tr.idx)){
    if (pars$sampling == "cv" && pars$nreps > n ){
      pars$sampling <- "loocv"
      pars$niter    <- 1
    }
    if (pars$sampling == "cv" && pars$nreps < 2)
         stop("Number of fold (nreps) for cv must greater than 1")
    tr.idx <- trainind(y, pars = pars)
  } else {
    pars$sampling <- c("user")
  }
  pars$niter    <- length(tr.idx)
  pars$nreps    <- length(tr.idx[[1]])

  cat("Iter (",pars$niter,"):",sep="")
  res.all <- lapply(1:pars$niter, function(i){
    cat(" ", i, sep = ""); flush.console()
    train.ind <- tr.idx[[i]]
    res <- lapply(1:pars$nreps, function(j) {
      x.tr  <- x[train.ind[[j]],,drop=F]
      y.tr  <- y[train.ind[[j]]]
      do.call(method, c(list(x=x.tr,y=y.tr), list(...)))
    })
    names(res) <- paste("Reps", 1:pars$nreps, sep="_")
    res
  })
  cat("\n")
  names(res.all) <- paste("Iter",1:pars$niter, sep="_")

  rank.list  <- lapply(res.all, function(x) as.data.frame(sapply(x, function(y) y$fs.rank)))
  order.list <- lapply(res.all, function(x) as.data.frame(sapply(x, function(y) y$fs.order)))
  stats.list <- lapply(res.all, function(x) as.data.frame(sapply(x, function(y) y$stats)))
  rank.list  <- do.call("cbind",rank.list)
  order.list <- do.call("cbind",order.list)
  stats.list <- do.call("cbind",stats.list)

  fs.stats   <- apply(stats.list, 1, mean)

  fs.score  <- apply(rank.list,1,sum)
  fs.order  <- order(fs.score, decreasing=F)
  fs.rank   <- order(fs.order, decreasing=F)
  names(fs.rank) <- rownames(rank.list)
  temp     <- names(fs.rank[fs.order])
  if (!is.null(temp)) fs.order <- noquote(temp)

  res <- list(fs.order   = fs.order,
              fs.rank    = fs.rank,
              fs.stats   = fs.stats,
              all        = res.all)
  return(res)
}
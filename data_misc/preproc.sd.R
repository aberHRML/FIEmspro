preproc.sd <- function(dat, cl=NULL)
{
  if (is.null(cl)) {
    id  <- which(apply(dat,2,sd) > .Machine$double.eps)
    dat <- dat[, id]
    return(dat)
  } else {
    cl <- factor(cl)
    z    <- sapply(data.frame(dat), function(x) tapply(x,cl,sd))
    z.1  <- sapply(data.frame(z), function(x) min(x))

    if (any(z.1 <= .Machine$double.eps)) {
      z.2  <- which(z.1 <= .Machine$double.eps)
      dat  <- dat[,-z.2, drop=F]
    }
    return(dat)
  }
}
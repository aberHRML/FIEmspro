my.cl <- function(x,y,cl.method, cl.pars,...) {
  val <- accest(x, y, clmeth=cl.method, pars=cl.pars,...)
  acc <- val$acc
  auc <- ifelse(!is.null(val$auc), val$auc, NA)
  mar <- ifelse(!is.null(val$mar), val$mar, NA)
  res <- c(acc=acc, auc=auc, mar=mar)
  return(round(res, digits=3))
}
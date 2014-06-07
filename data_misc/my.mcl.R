my.mcl <- function(x,y,cl.method, cl.pars,...) {
  res <- lapply(cl.method, function(m) {
    cat("\nClassifier = :",m,"\n"); flush.console()
    my.cl(x,y,cl.method=m, cl.pars,...)
  })
  names(res) <- cl.method
  res <- do.call(rbind, res)
}
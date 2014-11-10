`print.nlda` <-
function(x, ...)
{
  cat("\nCall:\n", deparse(x$call), "\n")
  cat("\nStatistics of training:\n")
  print(x$stats, ...)
  ## cat("\nCoefficients of linear discriminants:\n")
  ## print(x$loadings, ...)
  ## cat("\nProportion of trace:\n")
  ## print(x$Tw, ...)
  cat("\nConfusion matrix of training data:\n")
  print(x$conf)
  ## cat("\nAccurary rate of training data:\n")
  ## print(x$acc)
  invisible(x)
}


`print.summary.nlda` <-
function (x, ...) {
  print.nlda(x)
  
  ## cat("\nNumber of Classes: ", length(x$lev), "\n\n")
  ## cat("Levels:", if(is.numeric(x$lev)) "(as integer)", "\n", x$lev)
  ## cat("\n\n")
}


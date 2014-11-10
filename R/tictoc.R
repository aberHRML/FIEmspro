###
### TICTOC.R - Stopwatch timer
### NOTE: These two function are modified from R package matlab (MATLAB emulation
###       package by P. Roebuck. Thanks to P. Roebuck.

##-----------------------------------------------------------------------------

`tic` <-
function (gcFirst = FALSE) {
  if (gcFirst == TRUE) {gc(verbose = FALSE)}
  assign("savedTime", proc.time()[3], envir = .GlobalEnv)
  invisible()
}

`toc` <-
function (echo = TRUE) {
  prevTime <- get("savedTime", envir = .GlobalEnv)
  diffTimeSecs <- proc.time()[3] - prevTime
  if (echo) {
    cat(sprintf("elapsed time is %f seconds", diffTimeSecs),"\n")
    return(invisible())
  }
  else {return(diffTimeSecs) }
}

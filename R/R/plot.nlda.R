## wll-20-06-2007: fix a bug
`plot.nlda` <-
function(x, panel = panel.nlda, cex=0.7, dimen, abbrev = FALSE, ...)
{
  panel.nlda <- function(x, y, ...) {
    text(x, y, as.character(g.nlda), cex=tcex, col=unclass(g),...)
  }
  eig.val <- function(x, dimen) { 
    tmp <- rownames(x$stats)
    tmp <- tmp[dimen]
    eig <- x$stats[dimen,1]   ## Eigenvalues
    per <- x$stats[dimen,2]   ## Percentage
    tmp <- paste(tmp, " (", format(eig, digits = 2, trim = TRUE), ", ",
                 format(per, digits = 2, trim = TRUE),"%)", sep = "")
  }
  
  xval <- x$x
  g    <- x$cl
  
  if(abbrev) levels(g) <- abbreviate(levels(g), abbrev)
  assign("g.nlda", g)
  assign("tcex", cex)

  if (missing(dimen)){
    dimen <- seq(along=colnames(xval))
  } else {
    if (!all(dimen %in% c(1:ncol(xval)))){  
      ## stop("dimen is not valid")
      warning("dimen is not valid. Use default plotting.")
      dimen <- seq(along=colnames(xval))
    }
  }

  xval   <- xval[, dimen, drop=FALSE]
  varlab <- eig.val(x, dimen)
  nDimen <- length(dimen)

  if (nDimen <= 2) {
    if (nDimen == 1) {    ## One component
      MASS:::ldahist(xval[,1], g, ...)        
    } else {              ## Second component versus first
      xlab <- varlab[1]
      ylab <- varlab[2]
      MASS:::eqscplot(xval, xlab=xlab, ylab=ylab, type="n", ...)
      panel(xval[, 1], xval[, 2], ...)
    }
  } else {               ## Pairwise scatterplots of several components
    pairs(xval, labels = varlab, panel=panel, ...)
  }

  invisible(NULL)
  
}


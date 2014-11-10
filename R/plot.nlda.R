## wll-20-06-2007: fix a bug


#' Plot Method for Class 'nlda'
#' 
#' Plots a set of data on one, two or more linear discriminants.
#' 
#' This function is a method for the generic function \code{plot()} for class
#' \code{nlda}. The behaviour is determined by the value of \code{dimen}. For
#' the length of \code{dimen} is greater than 2, a \code{pairs} plot is used.
#' For the length of \code{dimen} is equal to 2, a scatter plot is drawn.
#' Otherwise, a set of histograms or density plots are drawn.
#' 
#' @usage \method{plotnlda}(x, panel = panel.nlda, cex = 0.7, dimen, abbrev =
#' FALSE, \dots{})
#' @param x An object of class \code{nlda}.
#' @param panel The panel function used to plot the data.
#' @param cex Graphics parameter \code{cex} for labels on plots.
#' @param dimen The index of linear discriminants to be used for the plot.
#' @param abbrev Whether the group labels are abbreviated on the plots. If
#' \code{abbrev > 0} this gives \code{minlength} in the call to
#' \code{abbreviate}.
#' @param \dots Additional arguments to \code{plot}.
#' @author Wanchang Lin \email{wll@@aber.ac.uk} and David Enot
#' \email{dle@@aber.ac.uk}.
#' @seealso \code{\link{nlda}}, \code{\link{predict.nlda}},
#' \code{\link{hca.nlda}}
#' @keywords hplot
#' @examples
#' 
#' ## load abr1
#' data(abr1)
#' cl   <- factor(abr1$fact$class)
#' dat <- preproc(abr1$pos , y=cl, method=c("log10","TICnorm"),add=1)[,110:500]  
#' 
#' ## build model on all the data available
#' model    <- nlda(dat,cl)
#' 
#' ## Plot second component versus first
#' plot(model,dimen=c(1,2),main = "Training data",abbrev = TRUE)
#' 
#' ## Pairwise scatterplots of several components 
#' plot(model,main = "Training data",abbrev = TRUE)
#' 
#' 
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


#' Data Tranformation Wrapper
#' 
#' Wrapper for several techniques to perform data transformation of the
#' original fingerprint matrix.
#' 
#' Purpose of normalisation is to remove inter-spectrum sources of variability
#' that come mainly from different sample concentration, loss of sensitivity of
#' the detector over time or degradation of certain samples. Global
#' normalisation by rescaling each measurement within a spectrum by a constant
#' factor, such as the sum of all the spectra intensities (\code{TICnorm}
#' method). However, the default use of TIC normalisation can lead to the
#' generation of spurious knowledge if the overall sample intensity is
#' class/factor dependent. If this is the case, a straightforward approach is
#' to remove the difference, between the average scale of the corresponding
#' class and the average scale, from the scale factor by providing the
#' class/factor of interest (argument \code{y}) while calling \code{preproc}.
#' 
#' @usage preproc (x, y=NULL,method="log",add=1)
#' @param x A numeric data frame or matrix to be pre-processed.
#' @param method A method used to pre-process the data set. The following
#' methods are supported: \itemize{ \item \code{center:} Centering \item
#' \code{auto:} Auto scaling \item \code{range:} Range scaling \item
#' \code{pareto:} Pareto scaling \item \code{vast:} Vast scaling \item
#' \code{level:} Level scaling \item \code{log:} Log transformation (default)
#' \item \code{log10:} Log 10 transformation \item \code{sqrt:} Square root
#' transformation \item \code{asinh:} Inverse hyperbolic sine transformation
#' \item \code{TICnorm:} TIC normalisation }
#' @param add A numeric value for addition used in the logarithmic
#' transformations \code{log} and \code{log10}.
#' @param y A factor specifying the class for each observation. It is only used
#' by the method \code{TICnorm}.
#' @return Transformed sample matrix.
#' @author Wanchang Lin \email{wll@@aber.ac.uk} and David Enot
#' \email{dle@@aber.ac.uk}.
#' @references Berg, R., Hoefsloot, H., Westerhuis, J., Smilde, A. and Werf, M.
#' (2006), Centering, scaling, and transformations: improving the biological
#' information content of metabolomics data, \emph{BMC Genomics}, 7:142
#' @keywords manip
#' @examples
#' 
#' data(abr1)
#' cl   <- factor(abr1$fact$class)
#' dat  <- abr1$pos[,110:2000]
#' 
#' ## normalise data set using class dependent "TICnorm"
#' z.1  <- preproc(dat, y=cl, method="TICnorm")
#' 
#' ## scale data set using "log10"
#' z.2 <- preproc(dat,method="log10", add=1)
#' 
#' ## or scale data set using "log" and "TICnorm" sequentially
#' z.3 <- preproc(dat,method=c("log","TICnorm"), add=0.1)
#' 
#' 
preproc <- function(x, y=NULL,method="log",add=1)
{
  ## ------------------------------------------------------------
  ## TIC normalisation
  TICnorm <- function(x,y=NULL){
    scale <- apply(x, 1,function(x) sum(x,na.rm=T))
    
    if(!is.null(y)){
      grpm = as.vector(by(scale,y,mean))
      grpm = grpm - mean(scale)
      for(k in 1:nlevels(y)){
        scale[y==levels(y)[k]] <- scale[y==levels(y)[k]] - grpm[k]
      }
    }
    x <- sweep(x, 1, scale, "/")
  }
  ## ------------------------------------------------------------
  me  <- function(x) mean(x,na.rm=T)
  se  <- function(x) sd(x,na.rm=T)
  mx  <- function(x) max(x,na.rm=T)
  mn  <- function(x) min(x,na.rm=T)
  sm  <- function(x) sum(x,na.rm=T)
  ## ------------------------------------------------------------

  if (!is.matrix(x) && !is.data.frame(x))
    stop("x must be a matrix or data frame.")
  x <- as.data.frame(x)
  if(!is.null(y))
    y <- as.factor(y)

  
  for (i in method){
    i <- match.arg(i, c("center", "auto", "range","pareto","vast","level",
                        "log","log10","sqrt","asinh","TICnorm"))

    x <- switch(i,
                ## by colmns
                "center"  = sapply(x, function(x) (x - me(x))),
                "auto"    = sapply(x, function(x) (x - me(x))/se(x)),
                "range"   = sapply(x, function(x) (x - me(x))/(mx(x)-mn(x))),
                "pareto"  = sapply(x, function(x) (x - me(x))/sqrt(se(x))),
                "vast"    = sapply(x, function(x) (x - me(x))*me(x)/se(x)^2),
                "level"   = sapply(x, function(x) (x - me(x))/me(x)),
                ## by all
                "log"     = log(x+add),
                "log10"   = log10(x+add),
                "sqrt"    = sqrt(x),
                "asinh"   = asinh(x),
                ## by row     
                "TICnorm" = TICnorm(x,y) 
                )
  }
  
  rownames(x) <- 1:nrow(x)
  return(x)
}


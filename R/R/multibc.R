#' Multiple Metabolomics Fingerprint Baseline Correction
#' 
#' Wrapper for function \code{onebc} for several samples. An additional
#' plotting parameter for visualizing the baseline correction process is added.
#' 
#' 
#' @usage multibc(x,wsize=50,qtl=0.1,maxy=1000,plotting=FALSE,pause=0)
#' @param x A data frame or matrix to be processed.
#' @param wsize Window size.
#' @param qtl A numeric value of probability with values in [0,1].
#' @param maxy A numeric value which specifies maximal intensity to be plotted
#' on y axis.
#' @param plotting A logical value indicating whether or not plotting.
#' @param pause Time interval defining pause between each baseline correction
#' in order to visualise each individual plot during the BC process if
#' \code{plotting} is \code{TRUE}.
#' @return A list containing the following components: \item{x}{Sample matrix
#' after baseline subtraction.  } \item{bsl}{Baseline matrix.  }
#' @author David Enot \email{dle@@aber.ac.uk}
#' @seealso \code{\link{onebc}}
#' @keywords manip
#' @examples
#' 
#'   ## load abr1
#'   data(abr1)
#'   mat <- abr1$pos[1:5,110:2000]
#' 
#'   ## baseline correction
#'   res <- multibc(mat,plotting=TRUE,pause=0.5)
#'   
#' 
`multibc` <-
function(x,wsize=50,qtl=0.1,maxy=1000,plotting=FALSE,pause=0)
{
  x <- as.matrix(x)
  
  matbc <- bsl <- x
  for(i in 1:nrow(x)){
    res   <- onebc(x[i,],wsize=wsize,qtl=qtl,maxy=maxy,plotting=plotting,sampid=i)
    matbc[i,]<- res$x
    bsl[i,] <- res$bsl
    Sys.sleep(pause)
  }
  return(list(x=matbc,bsl=bsl))
}


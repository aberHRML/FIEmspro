## wll-29-08-2007: add 'title' to show title or not


#' Metabolomics Fingerprint Baseline Correction
#' 
#' Core function to perform baseline correction on one metabolomics
#' fingerprint.
#' 
#' The purpose of the baseline correction is to remove undesirable effects due
#' excess chemical noise. Possible consequences of baseline drift include the
#' possibility that the baseline may be discriminatory and that important
#' information may be obscured in areas affected by baseline problems. A simple
#' consensual approach consists in fitting a monotone local minimum curve to
#' each fingerprint. Basically, the fingerprint is divided into equally spaced
#' m/z intervals and a local minimum intensity value is returned as the
#' baseline estimate for this region. Finally, the whole fingerprint baseline
#' is computed by linear interpolation based on pairs made of the centre of the
#' interval and its corresponding local minima. Intervals (argument
#' \code{wsize}) are in the order of 30-70 amu as a trade off between the
#' removal of relevant chemical (small interval) or estimation bias due to use
#' of a larger interval.  Rather than using the minimum value of an interval,
#' it is also judicious to use the value corresponding to a low quantile
#' (argument \code{qtl}) to avoid any spurious estimates due to zeros or
#' abnormally low signals.
#' 
#' @usage onebc(x,wsize=50,qtl=0.1,maxy=1000,plotting=TRUE,title=TRUE,
#' sampid=NULL)
#' @param x A numeric vector to be processed.
#' @param wsize Window size.
#' @param qtl A numeric value of for lower quantile probability.
#' @param maxy A numeric value specifying y axis maximal value to be plotted.
#' @param plotting A logical value indicating whether or not plotting.
#' @param title A logical value indicating whether or not to show plot title.
#' @param sampid Sample ID to be written in the title if both plotting and
#' title are TRUE (useful when \code{onebc} called from \code{multibc}).
#' @return A list containing the following components: \item{x}{A numeric
#' vector of the resulting fingerprint after baseline correction.  }
#' \item{bsl}{A numeric vector of the baseline intensities.  }
#' @author David Enot \email{dle@@aber.ac.uk} and Wanchang Lin
#' \email{wll@@aber.ac.uk}.
#' @seealso \code{\link{multibc}}
#' @keywords manip
#' @examples
#' 
#'   data(abr1)
#'   cl  <- factor(abr1$fact$class)
#'   mat <- abr1$pos
#' 
#'   ## baseline correction
#'   res <- onebc(mat[1,110:2000], qtl=0.8, sampid="1")
#'   
#' 
`onebc` <-
function(x,wsize=50,qtl=0.1,maxy=1000,plotting=TRUE,title=TRUE,sampid=NULL)
{
  x   <- as.numeric(x)
  nmz <- length(x)
  addini = floor(nmz%%wsize/2)
  interv = cbind(c(1,seq(addini+wsize,nmz-wsize,wsize)),c(seq(addini
                 +wsize,nmz-wsize,wsize),nmz))
  interv = rbind(interv,interv[-1,]-floor(wsize/2))
  lso    = sort(interv[,1],index.return=T)$ix
  interv = interv[lso,]
  ymax  <- NULL
  for(k in 1:nrow(interv)){
    ymax = c(ymax,quantile(x[interv[k,1]:interv[k,2]], probs = qtl)) 
  }
  ymz = c(1,apply(interv,1,mean),nmz)
  ## ymax=runmed(c(ymax[1],ymax,ymax[nrow(interv)]),k=3)
  ymax = c(ymax[1],ymax,ymax[nrow(interv)])
  bsl = approx(ymz,ymax,xout=1:nmz)$y

  if(plotting==TRUE){
    oldpar <- par(mar = par()$mar+c(0,0,0,4))

    ylim=c(min(x-bsl),min(c(maxy,max(x))))
    plot(1:nmz,x-bsl,col="green",type="l",ylim=ylim, 
         xlab="Index of M/Z",ylab="Signal Intensity")
    lines(1:nmz, bsl, col = "red",lwd=2)
    lines(1:nmz, x, col = "blue")
    abline(h = 0, col = "gray")

    par(oldpar)
    oldpar <- par(xpd = TRUE)
    legend(x = nmz+0.05*nmz, y = ylim[2], cex=0.8,
       legend=c("Processed","Baseline","Raw"), 
       pch=19, lty=1,  bty="n",
       col=c("green","red", "blue"))
    
    if (title){
      if(is.null(sampid)){
        title(paste("Baseline correction (qtl=",qtl,",wsize=",wsize,")",sep=""))
      } else {
        title(paste("Baseline correction (Sample id=",sampid,",qtl=",qtl,",wsize=",wsize,")",sep=""))
      }
    }
    par(oldpar)
  }
  
  return(list(x=x-bsl,bsl=bsl))
}


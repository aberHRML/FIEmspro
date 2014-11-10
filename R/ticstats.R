## wll-29-08-2007: add some text for MAD range


#' Compute and Display Total Ion Count (TIC) statistics
#' 
#' By definition the total ion count of a spectrum is the sum of all the m/z
#' signal intensities (sum over the columns).  A robust regression can be built
#' to model the effect of the injection order on the TIC of each sample. The
#' fitting residuals are used to evaluate the median of the absolute deviations
#' (MAD) from the linear equation. Sample TIC and linear model are then plotted
#' according to the injection order to identify potential outliers and or
#' structure in the data.
#' 
#' As an easy diagnostic measure, the TIC can provide an estimation of factors
#' that may affect the overall intensity of the run such as gradual instrument
#' drift (e.g. resulting from loss of sensitivity of the ion source), or step
#' changes in instrument characteristics after maintenance.  Also, an
#' examination of the TIC can reveal suspicious samples where unusually low or
#' especially high signal intensities in some runs may be due to contamination
#' or poorly extracted samples. A regression can be built to model the effect
#' of the injection order on the TIC of each sample. As a conservative rule,
#' any sample that deviates more than 2/3 (argument \code{thres}) times from
#' the MAD must be examined manually to identify the origin of the different
#' intensity behaviour and then removed before further statistical analysis if
#' corrective measures do not improve the individual fingerprint. Further
#' assessment of outlying samples is discussed later. In any case where a
#' linear relationship (i.e. gradually changing TIC in sample set) is observed
#' between the injection order and sample TIC, this dependency will be removed
#' by TIC normalisation. If other structure related to the order of injection
#' is noticed, for example an analytical batch effect (i.e. a step change in
#' TIC at beginning or in middle of an injection series), the user must
#' identify its potential origin (e.g. changes in machine calibration or mobile
#' phase) and possibly create a new experimental factor (batch) where each step
#' change in level corresponds to the start of a new batch.
#' 
#' @usage ticstats(x,injorder=NULL,thres=3)
#' @param x A numeric data frame or matrix to be processed.
#' @param injorder A numeric vector corresponding to the injection order of
#' each sample.
#' @param thres A numeric value of threshold for detecting outlier.
#' @return A list containing the following components: \item{resid}{Sample
#' residuals.} \item{mod}{Robust linear model.} \item{loutl}{List of outlying
#' samples as defined by the \code{thres} argument.}
#' @author David Enot and Wanchang Lin \email{dle,wll@@aber.ac.uk}
#' @seealso \code{\link[MASS]{rlm}}.
#' @keywords manip
#' @examples
#' 
#'   data(abr1)
#'   dat <- abr1$pos
#'   res <- ticstats(dat,injorder=NULL)
#' 
#' 
`ticstats` <- function(x,injorder=NULL,thres=3)
{
  if(is.null(injorder)){injorder=1:dim(x)[1]}

  tic    = apply(x,1,function(x) sum(x,na.rm=T)) ## 15-09-2007
  mod    = rlm(tic~injorder)
  madres = mad(mod$wresid)
  lout   = which(abs(mod$wresid) > thres*madres)
 
  ## oldpar <- par(mar = par()$mar+c(0,0,0,4),xpd=TRUE)

  if(length(lout)==0){
    plot(tic, col=1,pch=19,xlim=c(min(injorder),max(injorder)),
         ylim = c(min(tic)*0.9,max(tic)*1.1),
         xlab = "Order of Injection",ylab="Total Ion Count (TIC)") 
  } else {
    plot(tic[-lout], col=1,pch=19,xlim=c(min(injorder),max(injorder)),
         ylim = c(min(tic)*0.9,max(tic)*1.1),
         xlab = "Order of Injection",ylab="Total Ion Count (TIC)") 
  }
  
  pred <- predict(mod)
  n    <- length(pred)
  
  lines(pred,col=2,lwd=2)
  
  lines(pred + thres*madres,col=1,lty=4,lwd=1.5)         ## thres MAD
  text(n-2,pred[n-1] + thres*madres,paste(thres," MAD",sep=""), col=2)
  
  lines(pred + (thres-1)*madres,col=1,lty=3,lwd=1.5)     ## thres-1 MAD
  text(n-2,pred[n-1] + (thres-1)*madres,paste(thres-1," MAD",sep=""), col=2)
  
  lines(pred - thres*madres,col=1,lty=4,lwd=1.5)
  text(n-2,pred[n-1] - thres*madres,paste("-",thres," MAD",sep=""), col=2)
  
  lines(pred - (thres-1)*madres,col=1,lty=3,lwd=1.5)
  text(n-2,pred[n-1] - (thres-1)*madres,paste("-",thres-1," MAD",sep=""), col=2)
  
  ## par(oldpar)
  if(length(lout)!=0){for(k in lout){text(injorder[k],tic [k],k,col=2,cex=1.5)}}

  return(list(resid=tic-pred,mod=mod,lout=lout))
}


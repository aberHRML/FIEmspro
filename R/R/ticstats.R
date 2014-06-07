## wll-29-08-2007: add some text for MAD range
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


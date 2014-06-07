## wll-29-08-2007: add 'title' to show title or not
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


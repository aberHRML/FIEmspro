`fs.mi` <-
function(x,y)
{
  ## library(KernSmooth)
  getinf <- function(x,y,N=101){
    optbw = (length(x)^-0.2)*sd(x)*1.06
    res   = bkde(x,bandwidth=optbw,gridsize=N)
    dx    = res$x[2]-res$x[1]
    px    = res$y*dx
    px    = px[px>0]
    HX    = -sum(px*log2(px),na.rm=T)
    px    = table(y)/length(y)
    HC    = -px%*%log2(px)
    HCX   = 0
    for(i in levels(y)){
      ## res1 = density(x[y==i],from=res$x[1],to=res$x[length(res$x)],n=10)
      res1 = bkde(x[y==i],bandwidth=optbw,range.x=c(min(res$x),max(res$x)),
                  gridsize=N)
      px   = res1$y*dx
      px   = px[px>0]
      HCX  = HCX-length(which(y==i))*sum(px*log2(px),na.rm=T)
    }
    HCX = HCX/length(y)
    return(list(HX=HX,HC=HC,HCX=HCX,optbw=optbw))
  }
  ## ---------------------------------------------------------------------
  
  meas <- rep(NA,ncol(x))
  lv   <- which(apply(x,2,sd)>0)
  for(i in lv){
    tmp     <- getinf(x[,i],y)
    meas[i] <- tmp$HC + tmp$HX - tmp$HCX
  }
  names(meas) <- colnames(x)

fs.rank <- rank(-abs(meas), na.last=T, ties.method="random")
fs.order <- order(fs.rank, na.last=T)


  names(fs.rank) <- names(meas)
  nam <- names(meas[fs.order])
  if (!is.null(nam))
     fs.order <- noquote(nam)

  res <- list(fs.rank=fs.rank, fs.order=fs.order, stats=abs(meas))
  return(res)
}


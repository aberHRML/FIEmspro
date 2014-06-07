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


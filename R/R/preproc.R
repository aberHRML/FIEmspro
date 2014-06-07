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


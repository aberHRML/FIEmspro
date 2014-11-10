`fs.auc` <-
function(x,y)
{
  y <- as.factor(y)
  if (length(levels(y)) != 2) 
    stop("'y' must have two classes")

  levels(y) <- c(0,1)      ## change levels as 1,0
  y <- as.numeric(levels(y))[as.integer(y)]
  auc <- sapply(as.data.frame(x),function(x) { ## AUC
         y  <- y[order(x,decreasing=TRUE)]
    	   tmp <- cumsum(y)/sum(y)
    	   mean(tmp[y==0])
         })
  
  auc[auc < 0.5] <- 1 - auc[auc < 0.5]

fs.rank <- rank(-abs(auc), na.last=T, ties.method="random")
fs.order <- order(fs.rank, na.last=T)

  names(fs.rank) <- names(auc)
  nam <- names(auc[fs.order])
  if (!is.null(nam))
     fs.order <- noquote(nam)

  res <- list(fs.rank=fs.rank, fs.order=fs.order, stats=abs(auc))
  return(res)
}


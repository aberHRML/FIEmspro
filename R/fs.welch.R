`fs.welch` <-
function(x,y,...)
{

mug<-ssg<-varg<-ng<-NULL
for(k in levels(y)){
l=which(y==k)
ng <- c(ng,length(l))
tmp=apply(x[l,],2,mean)
mug <- rbind(mug,tmp)
ssg <- rbind(ssg,apply(sweep(x[l,],2,tmp,"-")^2,2,sum))
varg <- rbind(varg,apply(x[l,],2,var))
}
mutot=apply(x,2,mean)

     stderr <- sqrt(sqrt(varg[1,]/ng[1])^2 + sqrt(varg[2,]/ng[2])^2)
     df <- stderr^4/(sqrt(varg[1,]/ng[1])^4/(ng[1] - 1) + sqrt(varg[2,]/ng[2])^4/(ng[2] - 1))
     stats <- (mug[1,] - mug[2,])/stderr
     ## pval <- 2 * pt(-abs(st), df)   ## commented by wll, 29-03-2007
     pval <- 2 * pt(-abs(stats), df)

pval[pval<10^-16]<-10^-16
  
## fs.rank <- rank(-abs(auc), na.last=T, ties.method="random") ## commented by wll, 29-03-2007
fs.rank <- rank(-abs(stats), na.last=T, ties.method="random")
fs.order <- order(fs.rank, na.last=T)

  names(fs.rank) <- names(stats)
  nam <- names(stats[fs.order])
  if (!is.null(nam))
     fs.order <- noquote(nam)

  res <- list(fs.rank=fs.rank, fs.order=fs.order, stats=abs(stats),pval=pval)
  return(res)
}


feat.rank.re.default <- function(x,y,method, pars = valipars(),tr.idx=NULL, clmpi=NULL, seed=NULL,...)
{
  ## validity checking
  if (missing(x) || missing(y)) 
    stop("data set or class are missing")
  if(length(dim(x)) != 2)
    stop("'x' must be a matrix or data frame")
  if (!is.factor (y)) 
    stop("y must be a factor.")
  if (nrow(x) != length(y)) stop("x and y don't match.")
  if (length(unique(y)) < 2) 
    stop("Classification needs at least two classes.")
  if (any(is.na(x)) || any(is.na(y))) 
    stop("NA is not permitted in data set or class labels.")

  cl <- as.factor(y)
  nam=paste(levels(y),collapse="~")
  dat <- as.matrix(x)
  n   <- nrow(x)        ## number of samples
  p   <- ncol(x)        ## size of feature

  ## construct index of train data
  if(is.null(tr.idx)){
    if (pars$sampling == "cv" && pars$nreps > n ){
      pars$sampling <- "loocv"
      pars$niter    <- 1
    }
    if (pars$sampling == "cv" && pars$nreps < 2)
         stop("Number of fold (nreps) for cv must greater than 1")
    tr.idx <- trainind(y, pars = pars)
  }
  pars$niter    <- length(tr.idx)
  pars$nreps    <- length(tr.idx[[1]])

  all=do.call(method, c(list(x,y), list(...)))

  rank.list  <- list()
  order.list <- list()
  stats.list <- list()
  cat("Iteration (",pars$niter,"):",sep="")

  for (i in 1:pars$niter) {
   cat(" ", i, sep = "")
   flush.console()   ## for Windows
   train.ind <- tr.idx[[i]]
    res <- list()    
        params<-list(x=x,y=y,train.ind=train.ind,method=method,
          args=list(...),seed=seed,i=i)
        if(!is.null(clmpi))
          res<-clusterApplyLB(clmpi,1:length(train.ind),
            FIEmspro:::inloopfeatrk,params)
        else{
          for (j in 1:length(train.ind)) {
            res[[j]] <- FIEmspro:::inloopfeatrk(j,params)
          }
        }
    rank.list[[i]]  <- sapply(res, function(x) cbind(x$fs.rank))
    order.list[[i]] <- sapply(res, function(x) cbind(x$fs.order))
    stats.list[[i]] <- sapply(res, function(x) cbind(x$stats))
    rownames(rank.list[[i]])  <- colnames(x)
    rownames(stats.list[[i]]) <- colnames(x)
    rownames(order.list[[i]]) <- 1:ncol(x)
  } 
  cat("\n")
  rank.list  <- do.call("cbind",rank.list) 
  order.list <- do.call("cbind",order.list) 
  stats.list <- do.call("cbind",stats.list) 
  fs.stats   <- apply(stats.list, 1, mean)
  
  ## -----------------------------------------------------------------------
  ## Use Borda count to get the final feature order
  fs.score  <- apply(rank.list,1,sum)
  fs.order  <- order(fs.score, decreasing=F)  ## feature order from best to worst.   
  fs.rank   <- order(fs.order, decreasing=F)  ## feature rank score.
  names(fs.rank) <- rownames(rank.list)   
  temp     <- names(fs.rank[fs.order])
  if (!is.null(temp)) 
    fs.order <- noquote(temp)
  ## -----------------------------------------------------------------------

dots<-list(...)
if(length(dots)==0)
 argfct="default"
else
 argfct=paste(paste(names(unlist(dots)),unlist(dots),sep="="),collapse=",")

parsfct<-function(pars){
if(pars$sampling=="loocv")
  rep="loocv"
else{
 str<-"NSt"
 if(pars$strat) str<-"St"
 if(pars$niter>1)
   rep<-paste(pars$niter,"x",str,"-",pars$nreps,"-",pars$sampling,sep="")
 else
   rep<-paste(str,"-",pars$nreps,"-",pars$sampling,sep="")
}
if(!is.null(pars$div))
  rep=paste(rep,"-",round(pars$div,2),sep="")

rep
}
####

  ret <- list(method     = method,
              fs.order   = fs.order,       ## feature order
              fs.rank    = fs.rank,        ## feature rank
              fs.stats   = fs.stats,       ## means of stats
              rank.list  = rank.list,      ## full feature rank list
              order.list = order.list,     ## full feature order list
              stats.list = stats.list,     ## full feature stats list
              pars       = pars,           ## resampling parameters
              tr.idx     = tr.idx,         ## index of training samples.
              argfct     = argfct,         ## arguemtns passed to fs method
              all        = all,            ## feature ranking without re-sampling    
              cl.task = nam, ### addedd by dle to keep trace of the classification task
               pars.mini = parsfct(pars)
              )
  class (ret) <- "feat.rank.re"
  return(ret)
}


inloopfeatrk<-function(j,params){
            x.tr <- as.matrix(params$x[params$train.ind[[j]], , drop = F])
            y.tr <- params$y[params$train.ind[[j]]]
            if(!is.null(params$seed))
              set.seed(10000*params$i + params$seed)
            if(length(params$arg)==0)
              do.call(params$method, list(x.tr, y.tr))
            else
              do.call(params$method, c(list(x.tr, y.tr), params$args))          
}

######################
feat.rank.re <- function (...) UseMethod ("feat.rank.re")

print.feat.rank.re <- function(x,...){

  cat("Method:\t\t", x$method)
  cat("\nArguments:\t", x$argfct)
  cat("\nDiscrimination:\t", x$cl.task)
  cat("\n\nNo. of iteration:\t", x$pars$niter)
  cat("\nSampling:\t\t", x$pars$sampling)
  cat("\nNo. of replications:\t", x$pars$nreps)

}

######################
feat.rank.re.dlist<-function(dlist,method,pars=NULL,tr.idx=NULL,...)
{
#  call <- match.call()
  if (!inherits(dlist, "dlist")) 
    stop("method is only for dlist objects")

  cl=as.factor(dlist$cl)
  dat=as.matrix(dlist$dat)
  if(is.null(pars)){
    if(!is.null(dlist$pars))
      pars=dlist$pars
    else
      pars=valipars()
  }
  if(!is.null(dlist$tr.idx) & is.null(tr.idx))
    tr.idx=dlist$tr.idx
  
  ret <- feat.rank.re.default(dat, cl, method=method, pars = pars, tr.idx = tr.idx, ...)
  return(ret)
}

######################
feat.rank.re.formula <- function (formula, data = NULL, ...)
{
  call <- match.call()
  if (!inherits(formula, "formula")) 
    stop("method is only for formula objects")
  m <- match.call(expand.dots = FALSE)
  if (identical(class(eval.parent(m$data)), "matrix"))
    m$data <- as.data.frame(eval.parent(m$data))
  m$... <- NULL
  m$scale <- NULL
  m[[1]] <- as.name("model.frame")
  m$na.action <- na.action
  m <- eval(m, parent.frame())
  Terms <- attr(m, "terms")
  attr(Terms, "intercept") <- 0
  x <- model.matrix(Terms, m)
  y <- model.extract(m, "response")
  ret <- feat.rank.re.default (x, y, ...)
  return (ret)
}

##########################
tidy.ftrank<-function(frsum,lmod=NULL,sorting="Stat",tidy="DisId",nam="AlgId",decreasing=TRUE,file=NULL){

    if (!inherits(frsum, "mfr.sum")) 
        stop("method is only valid for mfr.sum objects")
    if (is.null(lmod)) 
        lmod = 1:length(frsum$frsum)
    lnam <- NULL
    if (!is.null(nam)) 
        lnam = frsum$frdef[lmod, dimnames(frsum$frdef)[[2]] == nam]
    ltidy = list(lmod)
    nametidy <- NULL
    if (!is.null(tidy)) {
        tmp = frsum$frdef[lmod, dimnames(frsum$frdef)[[2]] == 
            tidy]
        ltidy = list()
        for (k in unique(tmp)) {
            ltidy = c(ltidy, list(lmod[tmp == k]))
            nametidy = c(nametidy, k)
        }
    }

    ### check if same number of rows
    add.rows=0
    num.rows=unique(unlist(lapply(frsum[[1]],nrow)))
    if(length(num.rows)>1){add.rows=max(num.rows)}
    
    listres <- lapply(ltidy, function(x) {
        res <- NULL
        for (i1 in 1:length(x)) {
            i = x[i1]
            tmp <- frsum$frsum[[i]]
            if (!is.null(sorting)) {
                col = which(dimnames(tmp)[[2]] == sorting)
                idx = order(tmp[, col],decreasing=TRUE)
                tmp <- tmp[idx, ]
            }
            tmp = cbind(dimnames(tmp)[[1]], tmp)
            dimnames(tmp)[[2]][1] = "VarName"
            if (!is.null(lnam)) 
                dimnames(tmp)[[2]] = paste(dimnames(tmp)[[2]], 
                  lnam[x[i1]], sep = "-")
        if(add.rows>1)
             tmp=rbind(tmp,matrix(NA,add.rows-nrow(tmp),ncol(tmp)))

            res = cbind(res, tmp)
        }
        if(add.rows>1)
         dimnames(res)[[1]]=1:nrow(res)
         
        return(res)
    })
    if (!is.null(nametidy)) 
        names(listres) = nametidy
    if (!is.null(file)) {
        lapply(1:length(listres), function(x) {
            fname = paste(file, "-allfr.csv", sep = "")
            if (!is.null(nametidy)) 
                fname = paste(file, "-", nametidy[x], ".csv", 
                  sep = "")
            write.table(listres[[x]], file = fname, sep = ",", 
                row.names = FALSE)
        })
    }
    else{
    	if(length(listres)==1)
    	   return(listres[[1]])
    	 
    	return(listres)
    }
} ## end of function


##############
summ.ftrank<-function(lclas,lmod=NULL,qtl=0.25,padjust="fdr"){

if(!inherits(lclas, "ftrank.agg"))
  stop("method is only valid for ftrank.agg objects")

infct<-function(tmp,qtl=NULL,padjust="fdr"){
lnames=c("Stat","Rank")
summ=cbind(tmp$all$stats,tmp$all$fs.rank)
if(!is.null(tmp$all$pval)){
 summ=cbind(summ,tmp$all$pval)
 lnames=c(lnames,"pval")
 for(ipadj in padjust){
   summ=cbind(summ,p.adjust(tmp$all$pval,ipadj))
   lnames=c(lnames,paste("pval",ipadj,sep="-"))
 }
}
if(!is.null(qtl))
 for(iqtl in qtl){
   mrpval=fs.mrpval(tmp,iqtl)$mrpval
   summ=cbind(summ,mrpval)
   lnames=c(lnames,paste("mrpval",iqtl,sep="-"))
 }
x<-tmp$rank.list
summ=cbind(summ,apply(x, 1, mean),apply(x, 1, sd))
lnames=c(lnames,"AvgRk", "SdevRk")
dimnames(summ)=list(names(tmp$all$fs.rank),lnames)
summ
}

if(is.null(lmod))
  lmod=1:length(lclas$ftrank)

res<-lapply(lmod,function(x) infct(lclas$ftrank[[x]],qtl,padjust))
            
res=list(frsum=res,frdef=lclas$frdef[lmod,])

class (res) <- "mfr.sum"
return(res)

}

#############
ftrank.agg<-function(...){

## add cleaning to remove identical lines
## accept mfr.obj as well

lclas=list(...)
ncla=length(lclas)
mfr.obj<-list()
for(i in 1:ncla){
if(inherits(lclas[[i]], "feat.rank.re"))
  mfr.obj[[length(mfr.obj)+1]]<-lclas[[i]]
else
 for(k in 1:length(lclas[[i]])){mfr.obj[[length(mfr.obj)+1]]<-lclas[[i]][[k]]}
}
cl.def<-lapply(mfr.obj,function(x) c(x$method,x$argfct,x$pars.mini,x$cl.task))
cl.def<-matrix(unlist(cl.def),ncol=4,byrow=TRUE)
cl.def<-cbind(paste("Mod_",1:nrow(cl.def),sep=""),cl.def)
tmp<-rep("",nrow(cl.def))
tmp2<-paste(cl.def[,2],cl.def[,3])
for(i in unique(tmp2)){
 tmp[tmp2==i]=which(unique(tmp2)==i)
}
cl.def<-cbind(cl.def,paste("Alg_",tmp,sep=""))
tmp<-rep("",nrow(cl.def))
for(i in unique(cl.def[,5])){
 tmp[cl.def[,5]==i]=which(unique(cl.def[,5])==i)
}
cl.def<-cbind(cl.def,paste("Dis_",tmp,sep=""))
cl.def<-cbind(cl.def,rep("",nrow(cl.def)))
dimnames(cl.def)=list(1:nrow(cl.def),
   c("Model","Alg","Arg","Pars","Dis","AlgId","DisId","Other"))
mfr.obj=list(ftrank=mfr.obj,frdef=cl.def)
class (mfr.obj) <- "ftrank.agg"
return(mfr.obj)
}
#####



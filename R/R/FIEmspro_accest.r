##===========================================================================
## Calculate classification accuracy.
##
##===========================================================================
accest.default <- function(dat, cl, clmeth, pars = NULL,tr.idx = NULL,verb=TRUE,clmpi=NULL,seed=NULL,...)
{

  dots <-list(...)

  ## validity checking
  if (missing(dat) || missing(cl))
    stop("data set or class are missing")
  if (missing(clmeth))
    stop("'clmeth' is missing")
  if (nrow(dat) != length(cl)) stop("mat and cl don't match.")
  if (length(unique(cl)) < 2)
    stop("Classification needs at least two classes.")
  if (any(is.na(dat)) || any(is.na(cl)))
    stop("NA is not permitted in data set or class labels.")

  cl <- as.factor(cl)
  nam=paste(levels(cl),collapse="~")
  dat <- as.matrix(dat)
  n   <- nrow(dat)
  rownames(dat) <- NULL       
 
  ## construct index of train data
  if(is.null(pars))
    pars=valipars(sampling="rand", niter = 1, nreps=10,strat=TRUE)
  if(is.null(tr.idx)){
    if (pars$sampling == "cv" && pars$nreps > n ){
      pars$sampling <- "loocv"
    }
    tr.idx <- trainind(cl, pars = pars)
  }
  pars$niter    <- length(tr.idx)
  pars$nreps    <- length(tr.idx[[1]])


## Initialize
  res.all  <- pred.all<- list()
  params<-list(dat=dat,cl=cl,tr.idx=tr.idx,verb=verb,seed=seed,clmeth=clmeth,dots=dots)

  if(verb)
     cat("ACCEST Iteration (",pars$niter,"):",sep="")

## Choose between MPI/PVM or single proc
  if(!is.null(clmpi)){
    tmp<-clusterApplyLB(clmpi,1:pars$niter,
#       loopfct,params)
       FIEmspro:::loopfct,params)
    for (i in 1:pars$niter) {
      res.all[[i]] <- tmp[[i]]$res.all
      pred.all[[i]] <- tmp[[i]]$pred.all
    }
   }
   else{
     for (i in 1:pars$niter) {
#       tmp<-loopfct(i,params)
       tmp<-FIEmspro:::loopfct(i,params)
       res.all[[i]] <- tmp$res.all
       pred.all[[i]] <- tmp$pred.all
     }
  }
## End  
  if(verb)
    cat("\n")
  names(res.all) <- paste("Iter_",seq(1,pars$niter),sep="")

  ## accurary
  err.all  <- parse_vec(res.all, "err")
  acc.iter <- 1-apply(err.all,2,mean)

  ## process bootstrap acc
  if (pars$sampling == "boot"){
    plist<-c(list(dat,cl,1:length(cl),clmeth),params$dots,rs.iter=1,rs.rep=1)
    if(is.null(getS3method("predict",clmeth,optional=TRUE)))
       resub<-do.call(FIEmspro:::classifier.1,plist)
    else
       resub<-do.call(FIEmspro:::classifier.0,plist)
#       resub<-do.call(classifier.1,plist)
#    else
#       resub<-do.call(classifier.0,plist)
    err.boot <- lapply(1-acc.iter, function(x) FIEmspro:::boot.err(x, resub))
    err.boot <- t(sapply(err.boot, function(x) do.call("c", x)))
    acc.boot <- 1 - err.boot
  }

  ## ---------------- overall confusion matrix ----------------------
  ## wll-01-06-2007: Do not use sapply here because sometimes get non-equal fold.
  all.cl <- lapply(res.all, function(x){
    foo <- function(x) lapply(x, function(y) y$cl)
    foo(x)
  })
  all.cl <- unlist(unlist(all.cl, recursive = F,use.names=F))
  
  all.pred <- lapply(res.all, function(x){
    foo <- function(x) lapply(x, function(y) y$pred)
    foo(x)
  })
  all.pred <- unlist(unlist(all.pred, recursive = F,use.names=F))

  ## overall confusion matrix
  conf <- table(all.cl, all.pred)

  ## ---------------------------------------------------------------
  ## calculate mean of margin
  if (!is.null(res.all[[1]][[1]]$margin)) {
    mar.all <- t(sapply(res.all, function(x) {
      func <- function(x) sapply(x, function(y) mean(y$margin,na.rm=TRUE))
      func(x)
    }))
    mar.iter <- apply(mar.all,1,mean)
    mar      <- mean(mar.iter)
  } else {
    mar.iter <- NULL 
    mar      <- NULL
  }  
  
  ## calculate AUC
  if (!is.null(res.all[[1]][[1]]$auc) && pars$sampling != "loocv") {
      auc.all <- t(sapply(res.all, function(x) {
       func <- function(x) sapply(x, function(y) FIEmspro:::auc(y$auc[,1],y$auc[,2]-1))
       func(x)
    })) 
    auc.iter <- apply(auc.all,1,mean)
    auc      <- mean(auc.iter)
  } else {
    auc.iter <- NULL 
    auc      <- NULL
  }  
  ## ---------------------------------------------------------------

########## dle addons 
## process arguments -> new component in returned list
if(length(dots)==0)
 argfct="default"
else
 argfct=paste(paste(names(unlist(dots)),unlist(dots),sep="="),collapse=",")

## process all $mod into one list -> new component in returned list if not null
all.mod<-list()
if(!is.null(res.all[[1]][[1]]$mod)){
 for(k in 1:length(res.all)){
   tmp<-list()
    for(l in 1:length(res.all[[k]])){
      tmp[[l]]=res.all[[k]][[l]]$mod
    }
    all.mod[[k]]=tmp
 }
}

#####

  ## construct output
  ret <- list(clmeth   = clmeth,
              acc      = mean(acc.iter),
              acc.iter = acc.iter,
              mar      = mar,
              mar.iter = mar.iter,
              auc      = auc,
              auc.iter = auc.iter,
              sampling = switch(pars$sampling,
                            "loocv"  = "leave-one-out cross-validation",
                            "cv"     = "cross validation",
                            "boot"   = "bootstrap",
                            "rand"   = "randomised validation (holdout)"
                         ),
               niter   = pars$niter,
               nreps   = pars$nreps,
               conf    = conf,
               argfct   = argfct,   ### added by dle to keep trace of the arguments passed to the clmeth
               mod   = all.mod,   ### added by dle to keep elements in $mod
               pred.all = pred.all, ### added by dle to keep all predictions
               cl.task = nam, ### addedd by dle to keep trace of the classification task
               pars = pars,
               pars.mini = parsfct(pars)
              )
   if (pars$sampling == "boot") {
    ## ret$err.boot <- err.boot
    ret$acc.boot <- acc.boot
   }

   class (ret) <- "accest"
   return(ret)
}

####
## Function to be called during parallelisation
loopfct<-function(i,params){

    if(params$verb)
      cat(" ", i, sep = "")
    flush.console()   ## for Windows
    train.ind <- params$tr.idx[[i]]
    dat<-params$dat
    cl<-params$cl
    res <- list()
    tmp<-list(cl=NULL,pr=NULL,fo=NULL,auc=NULL,cl2=NULL,lsamp=NULL)
    for (j in 1:length(train.ind)) {
      ltr<-train.ind[[j]]
      lte<-(1:length(cl))[-train.ind[[j]]]
      if(!is.null(params$seed))
        set.seed(params$seed+1000*i+j)

      plist<-c(list(dat,cl,ltr,params$clmeth,rs.iter=i,rs.rep=j),params$dots)
      if(is.null(getS3method("predict",params$clmeth,optional=TRUE)))
#        res[[j]]<-do.call(classifier.1,plist)
#      else
#        res[[j]]<-do.call(classifier.0,plist)
        res[[j]]<-do.call(FIEmspro:::classifier.1,plist)
      else
        res[[j]]<-do.call(FIEmspro:::classifier.0,plist)
      tmp$fo=c(tmp$fo,rep(j,length(lte)))
      tmp$cl=c(tmp$cl,as.character(cl[-ltr]))
      tmp$pr=c(tmp$pr,as.character(res[[j]]$pred))
      tmp$lsamp=c(tmp$lsamp,lte)
      if(nlevels(cl)==2){
        tmp$auc=c(tmp$auc,res[[j]]$auc[,1])
        tmp$cl2=c(tmp$cl2,res[[j]]$auc[,2])
      }
      else
        tmp$auc<-tmp$cl2<-NULL
    }
    tmp$cl <- factor(tmp$cl,levels=levels(cl))
    tmp$pr <- factor(tmp$pr,levels=levels(cl))
    list(res.all=res,pred.all=tmp)
 }
## End

## Fct to summurise pars
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

##========================================================================
## 
print.accest <- function(x, digits=3,...) {

  cat("Method:\t\t", x$clmeth)
  cat("\nArguments:\t", x$argfct)
  cat("\nDiscrimination:\t", x$cl.task)


  cat("\n\nNo. of iteration:\t", x$niter)
  cat("\nSampling:\t\t", x$sampling)
  cat("\nNo. of replications:\t", x$nreps)
  cat("\n\nAccuracy:\t\t", round(x$acc,digits))
  if (!is.null(x$auc)) 
    cat("\nAUC:\t\t\t", round(x$auc,digits))
  if (!is.null(x$mar)) 
    cat("\nMargin:\t\t\t", round(x$mar,digits))

  cat("\n\nOverall confusion matrix of training data:\n")
  print(x$conf)
  
  invisible(x)
}


##========================================================================
summary.accest <- function(object, ...)
  structure(object, class = "summary.accest")

##========================================================================
print.summary.accest <- function(x, digits=3,...) 
{
  print.accest(x)               
  cat("\nAccuracy on each iteration:\n")
  print(round(x$acc.iter,digits))
  invisible(x)
}

## =======================================================================
#### Modifications by dle to allow mar, acc and auc to be plotted

plot.accest <- function(x, toplot="acc", main = NULL, xlab = NULL, ylab = NULL, ...)
{
  if (x$niter == 1)
    stop("Number of iteration (niter) must be greater than 1")

  if (is.null(main))
    main <- paste("Performance of `",x$clmeth, "'", sep=" ","(",x$sampling,")" )
    
  if (is.null(xlab)) xlab <- "Iterations"
  if (is.null(ylab)) ylab <- paste("Indicator:",toplot)

vect<-NULL
eval(parse(text=paste("vect=x$",toplot,".iter",sep=""))) 
plot(vect, type = "b", main=main,xlab=xlab, ylab=ylab, col="blue",...)

}

##===========================================================================
accest <- function (...) UseMethod ("accest")

##===========================================================================
accest.dlist<-function(dlist,clmeth,pars=NULL,tr.idx=NULL,...)
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
  
  ret <- accest.default(dat, cl, clmeth=clmeth, pars = pars, tr.idx = tr.idx, ...)
  return(ret)
}

##===========================================================================
accest.formula <- function (formula, data = NULL, ..., subset, 
                            na.action = na.omit)
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
  ret <- accest.default (x, y, ...)
  return (ret)
}



## ======================================================================
## dle: a couple of functions to parse results from classifier.1

#### Parse one value from multiple lists: you know this one!
parse_vec <- function(mlist,nam){
  all.res<-NULL
  for(k in 1:length(mlist)){
      tmp<-NULL
      for(l in 1:length(mlist[[k]])){
         lv<-NULL
         eval(parse(text=paste("lv=mlist[[k]][[l]]$",nam,sep="")))
         tmp=c(tmp,lv)
       }
      all.res=cbind(all.res,tmp)
  }
  dimnames(all.res)[[2]]=1:length(mlist)
  all.res
}

#### Parse frequency from multiple lists: fairly specific
parse_freq<-function (mlist, nam, sorting = TRUE) 
{
    lvar <- lnam <- lfreq <- NULL
    for (k in 1:length(mlist)) {
        for (l in 1:length(mlist[[k]])) {
            lv<-NULL
            eval(parse(text = paste("lv=mlist[[k]][[l]]$", nam, 
                sep = "")))
            for (i in lv) {
                if (i %in% lvar) {
                  lfreq[lvar == i] = lfreq[lvar == i] + 1
                }
                else {
                  lvar = c(lvar, i)
                  lfreq = c(lfreq, 1)
                  if (!is.null(names(lv))) {
                    lnam = c(lnam, names(lv)[lv == i])
                  }
                }
            }
        }
    }
    mat = cbind(lvar, lfreq)
    if (!is.null(lnam)) {
        dimnames(mat) = list(lnam, c("VarId", "Freq"))
    }
    if (sorting) {
        lso = sort(-mat[, 2], index.return = T)$ix
        mat = mat[lso, ]
    }
    mat
}

###### Classifier function that handles formalized classification methods
### by wll
classifier.0<-function (dat,cl,ltr, clmeth, rs.iter,rs.rep,...) 
{

    dots <- list(...)
    
    dat.tr <- dat[ltr,,drop=F]
    cl.tr  <- cl[ltr]
    if(length(cl) > length(unique(ltr))){
      dat.te <- dat[-unique(ltr),,drop=F]
      cl.te  <- cl[-unique(ltr)]      
    }
    else{
      dat.te<-dat
      cl.te<-cl
    }

    if (hasArg(probability)) 
        dots$probability <- NULL
    knn.wrap <- function(train, cl, k = 1, l = 0, ...) list(train = train, 
        cl = cl, k = k, l = l, ...)
    if (clmeth == "knn") {
        clmeth <- c("knn.wrap")
        predict <- function(x, ...) {
            knn(train = x$train, cl = x$cl, k = x$k, l = x$l, 
                ...)
        }
    }

    idx <- which(apply(dat.tr, 2, sd) > .Machine$double.eps)
    dat.tr <- dat.tr[, idx, drop = F]
    dat.te <- dat.te[, idx, drop = F]
    nte <- length(cl.te)
    dat.all <- rbind(dat.te, dat.tr)
    cl.all <- factor(c(as.character(cl.te), as.character(cl.tr)))
    model <- do.call(clmeth, c(list(dat.tr, cl.tr), probability = T, 
        dots))
    pred <- predict(model, dat.all)
    if (!is.list(pred)) {
        pred.te <- pred[1:nte]
        if (clmeth == "svm") {
            prob <- attr(predict(model, dat.all, probability = TRUE), 
                "probabilities")
            prob.te <- prob[1:nte, , drop = F]
        }
        else if (clmeth == "randomForest") {
            prob <- predict(model, dat.all, type = "prob")
            prob.te <- prob[1:nte, , drop = F]
        }
        else {
            prob.te <- NULL
        }
    }
    else {
        if (!is.null(pred$class)) {
            pred.te <- pred$class[1:nte]
            prob.te <- pred$posterior[1:nte, , drop = F]
        }
        else {
            stop("predict does not return a list with component 'class'.")
        }
    }
    err <- sum(cl.te != pred.te)/length(cl.te)
    prob.te <- prob.te[, levels(cl.te), drop = F]
    if (!is.null(prob.te)) {
        margin <- FIEmspro:::marg(prob.te, cl.te)
        if (length(levels(cl.te)) == 2 && length(cl.te) > 1) {
            cl.tmp <- cl.te
            levels(cl.tmp) <- c(0, 1)
            stat <- prob.te[, 2]
            auc <- cbind(stat, cl.tmp)
        }
        else {
            auc <- NULL
        }
    }
    else {
        margin <- NULL
        auc <- NULL
    }
    ret <- list(err = err, cl = cl.te, pred = pred.te, posterior = prob.te, 
        margin = margin, auc = auc, mod=NULL, arg = dots)
    return(ret)

}

##===========================================================================
###### Classifier function that handles customised classification methods
### by dle
classifier.1<-function (dat,cl,ltr, clmeth, rs.iter,rs.rep, ...) 
{

    dots <- list(...)

### form training and test data
    dat.tr <- dat[ltr,,drop=F]
    cl.tr  <- cl[ltr]
    if(length(cl) > length(unique(ltr))){
      dat.te <- dat[-unique(ltr),,drop=F]
      cl.te  <- cl[-unique(ltr)]      
    }
    else{
      dat.te<-dat
      cl.te<-cl
    }
    data=list(tr=dat.tr,cl=cl.tr,te=dat.te,clte=cl.te,ltr=ltr,rs.iter=rs.iter,rs.rep=rs.rep)

### match arguments between clmeth with those contained in dots
   argdots<-names(dots)
   argfct<-names(formals(clmeth))

### check the common arguments between clmeth and dots
   argint<-intersect(argdots,argfct)
### add extra arguments if clmeth allows '...'
   if(is.element("...",argfct))
      argint<-c(argint,setdiff(argdots,argfct))
### form the list to be passed to clmeth
   lpar<-list(data=data)
   for(k in argint){
      eval(parse(text=paste("lpar$",k,"=dots$",k,sep="")))
   }
### end of argument matching

  model<-do.call(clmeth, lpar)

  prob.te<-model$prob
  pred.te<-model$pred

  ## calculate error rate
  err <- sum(cl.te != pred.te)/length(cl.te)
  prob.te <- prob.te[,levels(cl.te),drop=F]   ## wll-06-07-07: for AUC purpose
  if (!is.null(prob.te)){
    margin <- FIEmspro:::marg(prob.te, cl.te)
    if (length(levels(cl.te))==2  && length(cl.te) > 1) { ## calculate AUC if two-class problem
      cl.tmp <- cl.te
      levels(cl.tmp) <- c(0,1)
      stat <- prob.te[,2]
      auc <- cbind(stat,cl.tmp)
    } else {
      auc <- NULL
    }
  } else {
    margin <- NULL
    auc <- NULL
  }

  ret <- list(err=err,cl=cl.te,pred=pred.te, posterior=prob.te,
              margin=margin, auc=auc, mod=model$mod, arg=model$arg)
  return(ret)
}

## ======================================================================
## Calculate bootstrap, .632 bootstrap and .632 plus bootstrap error rate
boot.err <- function(err, resub)                       
{
  ## apparent error rate/resubstitution rate ( not training error rate)
  err.ae <- resub$err
  cl     <- resub$cl
  pred   <- resub$pred    

  ## .632 bootstrap error
  err.b632  <- 0.368*err.ae + 0.632*err

  gamma <- sum(outer(cl, pred, function(x, y) ifelse(x==y, 0, 1) )) /(length(cl)^2)
  r     <- (err - err.ae)/(gamma - err.ae)
  r     <- ifelse(err > err.ae & gamma > err.ae, r, 0)
  ## lwc-16-08-2006: if r still falls outside of [0,1], truncate it to 0.
  ## if (r > 1 || r < 0) r <- 0              

  errprime <- min(err, gamma)
  ## weight <- .632/(1-.368*r)
  ## err.b632p <- (1-weight)*err.ae + weight*err
  
  err.b632p  <- err.b632 + (errprime - err.ae)*(0.368*0.632*r)/(1-0.368*r)

  ret <- list(ae=err.ae, boot=err, b632=err.b632, b632p=err.b632p)

  return(ret)
}


## ============================================================================
## Claculate the margin of a classifier based on the posterior 
## NOTE: 1. This function hacked from package 'randomForest'. For more 
##       description, see package 'randomForest'.
##
marg <- function(prob, observed) {

  if (missing(observed) || missing(prob)) stop("arguments miss")
  if(length(observed) != nrow(prob)) stop("lengths differ")

  if( any(prob > 1, na.rm=TRUE) ) {  ## modif dle 02/10/07
    prob <- sweep(prob, 1, rowSums(prob), "/")
  }
  observed <- as.factor(observed)
  
  mat <- data.frame(prob, observed)
  names(mat) <- c(dimnames(prob)[[2]], "observed")
  ## NOTE-wll: Ater data.frame() operation, the colnames may be changed (if the 
  ## colnames are numbers). The above line is to restore the original colnames.
  
  nlev <- length(levels(observed))
  
  ans <- apply(mat, 1, function(x){
    pos <- match(x[nlev+1], names(x));
    t1  <- as.numeric(x[pos]);
    t2  <- max(as.numeric(x[-c(pos, nlev+1)]));
    t1 - t2 
  })

  names(ans) <- observed
  ans
}

## ====================================================================  
## Gnerates the pairwise data set based on the class label.
dat.sel <- function(dat, cl, choices = NULL) 
{
  ## ----------------------------------------------------------------------
  ## ======================================================================
  ## wll-29-10-2006: combnations is from package gtools.
  ## 
  ## $Id: combinations.R,v 1.7 2005/06/09 14:20:28 nj7w Exp $
  ##
  ## From email by Brian D Ripley <ripley@stats.ox.ac.uk> to r-help
  ## dated Tue, 14 Dec 1999 11:14:04 +0000 (GMT) in response to
  ## Alex Ahgarin <datamanagement@email.com>.  Original version was
  ## named "subsets" and was Written by Bill Venables.  
  ##
  ## ======================================================================
  combinations <- function(n, r, v = 1:n, set = TRUE, repeats.allowed=FALSE) {
    if(mode(n) != "numeric" || length(n) != 1 
       || n < 1 || (n %% 1) != 0) stop("bad value of n") 
    if(mode(r) != "numeric" || length(r) != 1 
       || r < 1 || (r %% 1) != 0) stop("bad value of r") 
    if(!is.atomic(v) || length(v) < n) 
      stop("v is either non-atomic or too short")
    if( (r > n) & repeats.allowed==FALSE)
      stop("r > n and repeats.allowed=FALSE")
    if(set) {
      v <- unique(sort(v))
      if (length(v) < n) stop("too few different elements")
    }
    v0 <- vector(mode(v), 0)
    
    ## Inner workhorse
    if(repeats.allowed)
      sub <- function(n, r, v){ 
        if(r == 0) v0 else
        if(r == 1) matrix(v, n, 1) else
        if(n == 1) matrix(v, 1, r) else
        rbind(cbind(v[1], Recall(n, r-1, v)), Recall(n-1, r, v[-1]))
      }
    else
      sub <- function(n, r, v){ 
        if(r == 0) v0 else
        if(r == 1) matrix(v, n, 1) else
        if(r == n) matrix(v, 1, n) else
        rbind(cbind(v[1], Recall(n-1, r-1, v[-1])), Recall(n-1, r, v[-1]))
      }
  
    sub(n, r, v[1:n])
  }

  ## ---------------------------------------------------------------------
  func <- function(choices){
    if (is.null(choices)) {
      choices <- g
    } else {
      choices <- unique(choices)
    }
    
    i <- pmatch(choices, g)
    if (any(is.na(i)))
      stop("'choices' should be one of ", paste(g, collapse = ", "))
  
    ## Get the binary combinations based on the class labels (package GTOOLS)
    if (length(choices) == 1) {
      com <- combinations(length(g),2,v=g) 
      idx <- sapply(1:nrow(com), function(x){
        if (match(choices, com[x,],nomatch=0) > 0) return (T) else (F)
      })
      com <- com[idx,]
    } else {
      com <- combinations(length(choices),2,v=choices) 
    }
    return(com)
  }
  ## ---------------------------------------------------------------------

  if(missing(dat) || missing(cl)) 
    stop(" The data set and/or class label are missing!")
  cl <- as.factor(cl)
  g  <- levels(cl)

  if (is.list(choices)) {
    com <- lapply(choices, function(x) func(x))
    com <- do.call("rbind", com)
    com <- unique(com)
  } else { 
    com <- func(choices)
  }
  
  ## process the data set labels being selected
  dat.sub <- list()
  cl.sub  <- list()
  for (i in (1:nrow(com))) {
    idx      <- (cl==com[i,][1])|(cl==com[i,][2])
    cl.sub[[i]]  <- cl[idx]
    cl.sub[[i]]  <- cl.sub[[i]][,drop=T]     ## drop the levels
    dat.sub[[i]] <- dat[idx,,drop = F]     
  }

  ## get comparison names
  com.names  <- apply(com, 1, paste, collapse="~") 
  names(dat.sub) <- names(cl.sub) <- com.names

  return(list(dat=dat.sub,cl=cl.sub, com=com))
}




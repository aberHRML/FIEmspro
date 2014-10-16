#############
mc.agg.default<-function(...){

## add cleaning to remove identical lines
## accept mc.agg as well

lclas=list(...)
ncla=length(lclas)
mc.obj<-list()
for(i in 1:ncla){
if(inherits(lclas[[i]], "accest"))
  mc.obj[[length(mc.obj)+1]]<-lclas[[i]]
else
 for(k in 1:length(lclas[[i]])){mc.obj[[length(mc.obj)+1]]<-lclas[[i]][[k]]}
}
cl.def<-lapply(mc.obj,function(x) c(x$clmeth,x$argfct,x$pars.mini,x$cl.task))
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
cl.def<-cbind(cl.def,paste("Dis_",tmp,sep=""),rep("",nrow(cl.def)))
dimnames(cl.def)=list(1:nrow(cl.def),
   c("Model","Alg","Arg","Pars","Dis","AlgId","DisId","Other"))
mc.obj=list(clas=mc.obj,cldef=cl.def,lclas=unique(cl.def[,2]),
   ltask=unique(cl.def[,3]))
class (mc.obj) <- "mc.agg"
return(mc.obj)
}



#' Aggregation of classification results
#' 
#' Aggregate \code{accest} objects and list of \code{accest} objects to form
#' \code{mc.agg} object. The main utilities of this function is to concatenate
#' in a single list various results derived from several \code{accest} calls in
#' order to facilitate post analysis additional treatments as well as exporting
#' the results.
#' 
#' The length of the resulting list is equal to the total number of
#' \code{accest} objects (i.e one resampling experiment) plus one field that
#' summarises each \code{accest}. The later is a table with 8 columns which are
#' automatically generated to and also avoid confusions if the same method is
#' applied on the same discrimination problem but with different settings,
#' different resampling partitioning or even different data sets. Note that
#' columns 1 to 5 are automatically generated from the output of
#' \code{\link{accest}} where as columns 6 is based on columns 2-4. Each column
#' is described as follows: \describe{ \item{list("Mod")}{Unique identifier for
#' each resampling based feature rankings.}\item{:}{Unique identifier for each
#' resampling based feature rankings.} \item{list("Alg")}{Name of the
#' classification technique as specified in the call of
#' \code{\link{accest}}.}\item{:}{Name of the classification technique as
#' specified in the call of \code{\link{accest}}.} \item{list("Arg")}{Arguments
#' passed to the classifier during the call of
#' \code{\link{accest}}.}\item{:}{Arguments passed to the classifier during the
#' call of \code{\link{accest}}.} \item{list("Pars")}{Summary of the resampling
#' strategy adopted during the call of \code{\link{accest}}.}\item{:}{Summary
#' of the resampling strategy adopted during the call of \code{\link{accest}}.}
#' \item{list("Dis")}{Discrimination task involved. By default, this is equal
#' to the actual levels of the class vector passed to \code{\link{accest}}
#' separated by \code{~}.}\item{:}{Discrimination task involved. By default,
#' this is equal to the actual levels of the class vector passed to
#' \code{\link{accest}} separated by \code{~}.} \item{list("AlgId")}{Unique
#' algorithm identifier based on the columns Alg, Arg and Pars so that no
#' confusion is possible with Alg column if several classification have been
#' performed with the same discrimination technique but with different
#' parameters and/or resampling strategy. This column can be modified by the
#' user.}\item{:}{Unique algorithm identifier based on the columns Alg, Arg and
#' Pars so that no confusion is possible with Alg column if several
#' classification have been performed with the same discrimination technique
#' but with different parameters and/or resampling strategy. This column can be
#' modified by the user.} \item{list("DisId")}{Unique algorithm identifier
#' based on the columns Dis in order to simplified the name of the
#' discrimination task if there are many classes involved and/or class level
#' have a long name. This column can be modified by the user.}\item{:}{Unique
#' algorithm identifier based on the columns Dis in order to simplified the
#' name of the discrimination task if there are many classes involved and/or
#' class level have a long name. This column can be modified by the user.}
#' \item{list("Other")}{Empty column that can be amended to store extra
#' information.}\item{:}{Empty column that can be amended to store extra
#' information.} }
#' 
#' @aliases mc.agg mc.agg.default print.mc.agg
#' @usage mc.agg(\dots{})
#' 
#' \method{mc.aggdefault}(\dots{})
#' @param \dots accest objects and or list of accest objects
#' @return \code{mc.agg} objects: \item{clas}{List of \code{accest} objects}
#' \item{cldef}{Summary of each \code{accest} object - See details}
#' @author David Enot \email{dle@@aber.ac.uk}
#' @seealso \code{\link{accest}},\code{\link{mc.summary}},
#' \code{\link{mc.comp.1}}, \code{\link{mc.roc}}
#' @keywords manip
#' @examples
#' 
#' data(iris)
#' dat=as.matrix(iris[,1:4])
#' cl=as.factor(iris[,5])
#' lrnd=sample(1:150)[1:50]
#' cl[lrnd]=sample(cl[lrnd])
#' pars   <- valipars(sampling = "boot",niter = 2, nreps=10)
#' dat1=dat.sel1(dat,cl,pwise=list(),mclass=list(),pars=pars)
#' 
#' res1=accest(dat1[[1]],clmeth="randomForest",ntree=100,seed=1)
#' res2=lapply(dat1,function(x) accest(x,clmeth="lda"))
#' 
#' mc=mc.agg(res1,res2)
#' ###Print the content
#' mc
#' 
#' ### Classification task num. 1:
#' mc$clas[[1]]
#' 
#' ## As other functions are using the column 5 and 6 to sort and print the results,
#' ## you can replace them by something more informative
#' ## for e.g. Alg and Dis
#' mc$cldef[,6]<-mc$cldef[,2]
#' mc$cldef[,7]<-c("set~vir","set~vir","set~vir","ver~vir","ver~vir")
#' mc$cldef
#' 
#' 
mc.agg<-function(...) UseMethod ("mc.agg")

print.mc.agg<-function(x,...)
  print(x$cldef)

#############
mc.roc.default<-function(mc.obj,lmod=1,method="all"){

if(!inherits(mc.obj, "mc.agg"))
  stop("method is only valid for mc.agg objects")

genroc<-function(dec.bound,cl2){

  fpr<-tpr<-0
  cut.off=sort(unique(dec.bound),decreasing=TRUE)
  for(klim in cut.off){
    l=which(dec.bound>=klim)
    fpr=c(fpr,sum(cl2[l]==1)/sum(cl2==1))
    tpr=c(tpr,sum(cl2[l]==2)/sum(cl2==2))
  }
  diffco=-diff(cut.off)
  diffco<-diffco[-which.max(diffco)]
  diffco<-max(cut.off)+mean(diffco)
  list(fpr=as.numeric(fpr),tpr=as.numeric(tpr),thr=c(diffco,cut.off))
}


roc.one<-function(mod,method="thres"){
 alldb<-NULL
 for(k in 1:length(mod)){
  alldb=rbind(alldb,cbind(mod[[k]]$auc,mod[[k]]$cl2,mod[[k]]$fo,rep(k,length(mod[[k]]$auc))))
 }
 alldb=cbind(alldb[,1:2],alldb[,3]+alldb[,4]*10000)
 if(method=="all"){
  res<-c(genroc(alldb[,1],alldb[,2]),list(type="all"))
  return(res)
 }
 allrocs<-NULL
 id<-1
 for(i in unique(alldb[,3])){
   l=which(alldb[,3]==i)
   tmp=genroc(alldb[l,1],alldb[l,2])
   allrocs<-rbind(allrocs,cbind(tmp$fpr,tmp$tpr,rep(id,length(tmp$fpr)),tmp$thr))
   id<-id+1
 }
 if(method=="thres"){
  mn=min(allrocs[,4],na.rm=TRUE)
  mx=max(allrocs[,4],na.rm=TRUE)
  thres.values <- rev(seq(mn,mx,length=max(table(allrocs[,3]))+1))
  fpragg<-tpragg<-NULL
  for(i in 1:max(allrocs[,3])){
     l<-which(allrocs[,3]==i)
     fpragg<-rbind(fpragg,approxfun(allrocs[l,4],allrocs[l,1],rule=2, ties=mean)(thres.values))
     tpragg<-rbind(tpragg,approxfun(allrocs[l,4],allrocs[l,2],rule=2, ties=mean)(thres.values))
  }
  return(list(fpr=apply(fpragg,2,mean),tpr=apply(tpragg,2,mean),thr=thres.values,type="thres"))
 }
 if(method=="vert"){
  mn=min(allrocs[,1],na.rm=TRUE)
  mx=max(allrocs[,1],na.rm=TRUE)
  x.values <- rev(seq(mn,mx,length=max(table(allrocs[,3]))+1))
  tpragg<-NULL
  for(i in 1:max(allrocs[,3])){
     l<-which(allrocs[,3]==i)
     tpragg<-rbind(tpragg,approxfun(allrocs[l,1],allrocs[l,2],rule=2, ties=mean)(x.values))
  }
  return(list(fpr=x.values,tpr=apply(tpragg,2,mean),thr=x.values,type="vert"))
 }
 if(method=="horiz"){
  mn=min(allrocs[,2],na.rm=TRUE)
  mx=max(allrocs[,2],na.rm=TRUE)
  y.values <- rev(seq(mn,mx,length=max(table(allrocs[,3]))+1))
  fpragg<-NULL
  for(i in 1:max(allrocs[,3])){
     l<-which(allrocs[,3]==i)
     fpragg<-rbind(fpragg,approxfun(allrocs[l,2],allrocs[l,1],rule=2, ties=mean)(y.values))
  }
  return(list(fpr=apply(fpragg,2,mean),tpr=y.values,thr=y.values,type="horiz"))
 }

}

res<-lapply(lmod,function(x) 
    roc.one(mc.obj$clas[[x]]$pred.all,method=method)
    )
roc.l=list(roc=res,cldef=mc.obj$cldef[lmod,])
class(roc.l) <- "mc.roc"
roc.l       
}



#' Generate ROC curves from several classifiers
#' 
#' Convenience function to generate a ROC curve from several runs and
#' iterations for one model or a selection of models contained in a
#' \code{mc.acc} object (see details \code{\link{mc.agg}}). This function
#' allows the averaging of several ROC curves produced on each data
#' partitioning of the resampling strategy (i.e cross-validation runs repeated
#' or not several times). Three methods are available to perform the averaging:
#' "horiz" (horizontal), "vert" (vertical) and "thres" (thresholding). (see
#' reference for further details). The default value ("all") means that each
#' data points from individual ROC curves are strictly concatenated into a
#' single ROC curve.
#' 
#' 
#' @aliases mc.roc mc.roc.default
#' @usage mc.roc(mc.obj, lmod = 1, method = "all")
#' \method{mc.rocdefault}(mc.obj, lmod = 1, method = "all")
#' @param mc.obj \code{mc.agg} object - See details \code{\link{mc.agg}}
#' @param lmod List of models to be considered - Default: all of them
#' @param method Aggregation method ("all", "thres", "vert", "horiz")
#' @return \code{roc.list} object is a list of two components: \item{roc}{List
#' of ROC curves of length equal to the number of models. For each curve true
#' positive (tpr), false positive rate (fpr), decision boundary threshold
#' (thres) and type (type) of ROC aggregation are given.}
#' \item{cldef}{Identical to \code{cldef} in \code{\link{mc.agg}}.}
#' @author David Enot \email{dle@@aber.ac.uk}
#' @seealso \code{\link{plot.mc.roc}},\code{\link{mc.agg}}
#' @references Fawcett, T. (2004). ROC graphs: notes and practical
#' considerations for researchers.  \code{Technical Report HPL-2003-4}
#' @keywords manip
#' @examples
#' 
#' data(iris)
#' dat=as.matrix(iris[,1:4])
#' cl=as.factor(iris[,5])
#' lrnd=sample(1:150)[1:50]
#' cl[lrnd]=sample(cl[lrnd])
#' pars   <- valipars(sampling = "cv",niter = 2, nreps=10)
#' dat1=dat.sel1(dat,cl,pwise="virginica",mclass=NULL,pars=pars)
#' 
#' res1=lapply(dat1,function(x) accest(x,clmeth="lda"))
#' res2=lapply(dat1,function(x) accest(x,clmeth="randomForest",ntree=50))
#' mc=mc.agg(res1,res2)
#' 
#' roc.sv=mc.roc(mc,lmod=1:4,method="thres")
#' print(roc.sv)
#' 
#' 
mc.roc <- function(mc.obj, lmod = 1, method = "all") UseMethod("mc.roc")

#############


#' Plot multiple ROC curves
#' 
#' Plot multiple ROC curves contained in \code{mc.roc} objects (see details in
#' \code{\link{mc.roc}}).
#' 
#' 
#' @usage \method{plotmc.roc}(x, leg = "Model", llty = NULL, lcol = NULL, xleg
#' = 0.5, yleg = 0.4, ...)
#' @param x \code{mc.roc} object - See details in \code{\link{mc.roc}}
#' @param leg User defined legend or select information from \code{x$cldef}
#' @param llty List of symbols
#' @param lcol List of colors
#' @param xleg Upper left corner co-ordinate on the x-axis
#' @param yleg Upper left corner co-ordinate on the y-axis
#' @param \dots Further arguments to be passed to lines
#' @return NULL
#' @author David Enot \email{dle@@aber.ac.uk}
#' @seealso \code{\link{mc.roc}}
#' @keywords hplot
#' @examples
#' 
#' data(iris)
#' dat=as.matrix(iris[,1:4])
#' cl=as.factor(iris[,5])
#' lrnd=sample(1:150)[1:50]
#' cl[lrnd]=sample(cl[lrnd])
#' pars   <- valipars(sampling = "cv",niter = 2, nreps=10)
#' dat1=dat.sel1(dat,cl,pwise="virginica",mclass=NULL,pars=pars)
#' 
#' res1=lapply(dat1,function(x) accest(x,clmeth="lda"))
#' res2=lapply(dat1,function(x) accest(x,clmeth="randomForest",ntree=50))
#' mc=mc.agg(res1,res2)
#' 
#' roc.sv=mc.roc(mc,lmod=1:4)
#' 
#' ### Default plot
#' plot(roc.sv)
#' 
#' ###Improve plotting by using the names contained in the "DisId" and "Alg"
#' plot(roc.sv,leg=c("DisId","Alg"),llty=c(1,1,2,2),lcol=c("blue","red","blue","red"))
#' 
#' ###Improve plotting by setting the legend
#' plot(roc.sv,leg=c("roc1","roc2","roc3","roc4"),llty=c(1,1,2,2),lcol=c("blue","red","blue","red"))
#' 
#' 
plot.mc.roc<-function(x,leg="Model",llty=NULL,lcol=NULL,xleg=0.5,yleg=0.4,...){

mc.roc<-x
if(!inherits(mc.roc, "mc.roc"))
  stop("method is only valid for mc.roc objects")

rep<-match(leg,c("Model","Alg","Arg","Pars","Dis","AlgId","DisId","Other"))
rep=rep[!is.na(rep)]
if(length(rep)>0){
  leg2<-mc.roc$cldef[,rep[1]]
  if(length(rep)>1)
    for(i in rep[-1]){
    leg2=paste(leg2,mc.roc$cldef[,rep[1]],sep="-")
    }
  leg<-leg2
}

if(is.null(llty))
  llty=c(rep(1,4),rep(2,4),rep(3,4))
if(is.null(lcol))
  lcol=rep(c("black","red","blue","green"),3)

plot.new()
plot.window(xlim = c(-0.03,1.01), ylim = c(-0.01,1.01), xaxs = "i")
for(id in 1:length(mc.roc$roc)){
lines(mc.roc$roc[[id]]$fpr,mc.roc$roc[[id]]$tpr,col=lcol[id],lty=llty[id],...)
}
axis(1,cex.axis=1,...)
axis(2,cex.axis=1,...)
segments(0,0,1,1,pch=3,col="gray")
#cl.list=unlist(lapply(mc.roc,function(x) c(x[[4]])))
#ncl=length(unique(cl.list))
#comp.list=unlist(lapply(mc.roc,function(x) c(x[[3]])))
#ncomp=length(unique(comp.list))
legend(xleg,yleg,leg,col=lcol[1:id],lty=llty[1:id])

invisible(NULL)

}

#############################


#' Multiple Classifier Predictions Comparison
#' 
#' Function to test for significant differences between predictions made by
#' various classifiers (method and/or settings) built on the same partitioning
#' schema. This function requires that explicit class predictions for each fold
#' and iteration are contained in the classifier object of the type of
#' \code{\link{accest}}. For each pairwise comparison: mean of the differences,
#' variance associated, student t-statistics and corresponding p value are
#' reported in a table. Subsequent multiple testing correction is applied if
#' more than two classifiers are involved. Note that column \bold{DisId} is
#' used to sort the classifiers according to the discrimination task and
#' \bold{DisId} and \bold{AlgId} will be used to report the results. Of course,
#' it is also assumed that partitioning for models built with two different
#' classifiers is identical.
#' 
#' 
#' @aliases mc.comp.1 mc.comp.1.default print.mc.comp.1
#' @usage mc.comp.1(mc.obj,lmod=NULL,p.adjust.method="holm")
#' 
#' \method{mc.comp.1default}(mc.obj,lmod=NULL,p.adjust.method="holm")
#' 
#' %\method{print}{mc.comp.1}(mc.anal,digits=3,file=NULL)
#' @param mc.obj \code{mc.agg} object - See details \code{\link{mc.agg}}
#' @param lmod List of models to be considered - Default: all of them
#' @param p.adjust.method Multiple testing correction. See details in
#' \code{\link{p.adjust}}
#' @return \code{mc.comp.1} object: \item{res}{Summary of classifier pairwise
#' comparisons for each discrimination task} \item{cltask}{Discrimination
#' task(s).} \item{title}{Title for printing function.}
#' @note See publications mentioned below.
#' @author David Enot \email{dle@@aber.ac.uk}
#' @seealso \code{\link{mc.agg}}
#' @references Berrar, D., Bradbury, I. and Dubitzky, W. (2006). Avoiding model
#' selection bias in small-sample genomic datasets. \emph{Bioinformatics}.
#' Vol.22, No.10, 1245-125.
#' 
#' Bouckaert, R.R.,and Frank, E., (2004). Evaluating the Replicability of
#' Significance Tests for Comparing Learning Algorithms.  \emph{Proc 8th
#' Pacific-Asia Conference on Knowledge Discovery and Data Mining}. Vol.3054,
#' 3-12
#' @keywords htest
#' @examples
#' 
#' 
#' data(iris)
#' x <- as.matrix(subset(iris, select = -Species))
#' y <- iris$Species
#' pars   <- valipars(sampling = "cv",niter = 10, nreps=5, strat=TRUE)
#' tr.idx <- trainind(y,pars=pars)
#' ## RF model based one tree
#' acc1   <- accest(x, y, clmeth ="randomForest", pars = pars, tr.idx=tr.idx,ntree=1)
#' ## RF model based 100 trees
#' acc2   <- accest(x, y, clmeth = "randomForest", pars = pars, tr.idx=tr.idx,ntree=100)
#' ### RF model where the minimum size of terminal nodes is set to a value greater 
#' ## than the maximum number of samples per class (oups!)
#' acc3   <- accest(x,y, clmeth = "randomForest", pars = pars, tr.idx=tr.idx,ntree=1,nodesize=80)
#' 
#' clas=mc.agg(acc1,acc2,acc3)
#' res.comp<-mc.comp.1(clas,p.adjust.method="holm")
#' 
#' ## No significant differences between 1 and 2
#' ## Of course classifiers 1 and 2 performs significantly better than 3
#' ## by default
#' res.comp
#' 
#' ## with a few more decimals...
#' print(res.comp,digits=4)
#' 
#' ## Print results in a file
#' \dontrun{print(res.comp,digits=2,file="tmp.csv")}
#' 
#' 
#' 
mc.comp.1 <- function(mc.obj,lmod=NULL,p.adjust.method="holm") UseMethod("mc.comp.1")
mc.comp.1.default<-function(mc.obj,lmod=NULL,p.adjust.method="holm"){

if(!inherits(mc.obj, "mc.agg"))
  stop("method is only valid for mc.agg objects")

if(is.null(lmod))
 lmod=1:nrow(mc.obj$cldef)

  infct<-function(clas, p.adjust.method="holm",clname=NULL){
    ncla = length(clas)
    acccv <- NULL
    for (k in 1:ncla) {
        trainrat <- tmp <- NULL
        tmp2 <- clas[[k]]$pred.all
        for (j in 1:length(tmp2)) {
            tmp3 <- as.numeric(tmp2[[j]]$cl == tmp2[[j]]$pr)
            forep = unclass(table(tmp2[[j]]$fo))
            tmp = c(tmp, tapply(tmp3, tmp2[[j]]$fo, sum)/forep)
            trainrat <- c(trainrat, forep/(sum(forep) - forep))
        }
        acccv <- cbind(acccv, tmp)
    }
    if(is.null(clname))
      clname = paste("Cl", 1:ncla, sep = "_")
    dimnames(acccv)[[2]] = clname
    kr <- length(trainrat)
    trainrat <- mean(trainrat)
    res <- lnam <- NULL
    for (i1 in 1:(ncla - 1)) {
        for (i2 in (i1 + 1):ncla) {
            lnam = c(lnam, paste(dimnames(acccv)[[2]][i2], dimnames(acccv)[[2]][i1], 
                sep = "-"))
            xij <- acccv[, i2] - acccv[, i1]
            mea <- mean(xij)
            sdev = sum((xij - mea)^2)/(kr - 1)
            stat.t <- 0
            if (sdev > 0) 
                stat.t <- mea/sqrt((1/kr + trainrat) * sdev)
            pval.t <- 2*pt(-abs(stat.t), kr - 1)
            res <- rbind(res, c(mea, sdev, stat.t,kr - 1, pval.t))
        }
    }
    res <- cbind(res, p.adjust(res[, 5], p.adjust.method))
    dimnames(res) = list(lnam, c("Diff", "Var", "t-stat","DF", "pval", 
        p.adjust.method))
    res
    }

lmod2<-list()
ludis=unique(mc.obj$cldef[lmod,7])
id=1
for(i in 1:length(ludis)){
 l=which(mc.obj$cldef[lmod,7]==ludis[i])
 if(length(l) > 1){
    lmod2[[id]]=list(lmod[l],ludis[i])
    id=id+1
 }
}

res<-lapply(lmod2,function(y){
     infct(lapply(y[[1]],function(x) mc.obj$clas[[x]]),
       p.adjust.method=p.adjust.method,
       clname=mc.obj$cldef[y[[1]],6])  
  })
  
ret<-list(res=res,task=unlist(lapply(lmod2,function(x) x[[2]])),
    title="Multiple classifiers comparisons")

class(ret) <- "mc.comp.1"
ret
}



############################
print.mc.summary<-function(x, digits=3,...){
  FIEmspro:::printmc.anal(x, digits,...)
}

print.mc.comp.1<-function(x, digits=3,...){
  FIEmspro:::printmc.anal(x, digits,...)
}


printmc.anal<-function(x,digits=3,file=NULL){

mc.anal<-x
#if(!inherits(mc.anal, "mc.anal"))
#  stop("method is only valid for mc.anal objects")

catcsv<-function(top,file){
if(is.null(dim(top))){
cat("\n",paste(names(top),collpase=","),"\n",file=file,append=TRUE)
cat(paste(top,collpase=","),"\n",file=file,append=TRUE)
}
else{
cat("\n,",paste(dimnames(top)[[2]],collpase=","),"\n",file=file,append=TRUE)
for(j in 1:nrow(top)){
cat(paste(c(rownames(top)[j],top[j,]),collpase=","),"\n",file=file,append=TRUE)
}
}
}

if(is.null(file))
  cat("\n",mc.anal$title,":\n")
else
   cat("\n",mc.anal$title,":\n",file=file,append=FALSE)

for(i in 1:length(mc.anal$task)){
  if(is.null(file)){
     cat("\n",mc.anal$task[i],"\n")
     print(round(mc.anal$res[[i]],digits))
  }
  else{
     cat("\n",mc.anal$task[i],"\n",file=file,append=TRUE)
     catcsv(round(mc.anal$res[[i]],digits),file)
  }
}
if(is.null(file))
  cat("\n")
else
   cat("\n",file=file,append=TRUE)

}


############################


#' Summary of multiple classifiers objects
#' 
#' Convenience function to output statistics related to accuracy, AUC and
#' margins for a selection of models. If \code{sortDis=TRUE}, results are
#' grouped by discrimination task (value contained in DisId column of
#' \code{mc.obj$cldef}). If \code{sortDis=FALSE}, results are grouped by
#' classifier algorithm (value contained in column AlgId of
#' \code{mc.obj$cldef}).
#' 
#' 
#' @aliases mc.summary mc.summary.default print.mc.summary
#' @usage mc.summary(mc.obj,lmod=NULL,sortDis=TRUE)
#' 
#' \method{mc.summarydefault}(mc.obj, lmod = NULL, sortDis = TRUE)
#' 
#' %\method{print}{mc.summary}(mc.anal,digits=3,file=NULL)
#' @param mc.obj \code{mc.agg} object
#' @param lmod List of models to be considered - Default: all of them
#' @param sortDis Should the results be sorted by discrimination task? If
#' FALSE, results are group by classifier techniques
#' @return \code{mc.summary} object: \item{res}{List of results}
#' \item{cltask}{Discrimination task(s) or classification algorithm(s) used.}
#' \item{title}{Title for printing function.}
#' @author David Enot \email{dle@@aber.ac.uk}
#' @seealso \code{\link{mc.agg}}
#' @keywords manip
#' @examples
#' 
#' data(iris)
#' dat=as.matrix(iris[,1:4])
#' cl=as.factor(iris[,5])
#' lrnd=sample(1:150)[1:50]
#' cl[lrnd]=sample(cl[lrnd])
#' pars   <- valipars(sampling = "cv",niter = 2, nreps=10)
#' dat1=dat.sel1(dat,cl,pwise="virginica",mclass=NULL,pars=pars)
#' 
#' res1=lapply(dat1,function(x) accest(x,clmeth="lda"))
#' res2=lapply(dat1,function(x) accest(x,clmeth="randomForest",ntree=50))
#' 
#' ## Aggregate res1 and res2
#' mc=mc.agg(res1,res2)
#' 
#' ## Sort results by discrimination task
#' mc.summary(mc)
#' 
#' ## Sort results by algorithm
#' mc.summary(mc,sortDis=FALSE)
#' 
#' ## See what is in
#' names(mc.summary(mc))
#' 
#' ## Print results in a file
#' \dontrun{print(mc.summary(mc,sortDis=FALSE),digits=2,file="tmp.csv")}
#' 
mc.summary <- function(mc.obj,lmod=NULL,sortDis=TRUE) UseMethod("mc.summary")
mc.summary.default<-function(mc.obj,lmod=NULL,sortDis=TRUE){

  if(!inherits(mc.obj, "mc.agg"))
    stop("method is only valid for mc.agg objects")

  if(is.null(lmod))
   lmod=1:nrow(mc.obj$cldef)


  col.nam=c("Acc.mean","Acc.sd","Mar.mean","Mar.sd","AUC.mean","AUC.sd")
  meanfct<-function(x)
       ifelse(is.null(x),NA,mean(x,na.rm=TRUE))
  sdfct<-function(x)
       ifelse(is.null(x),NA,sd(x,na.rm=TRUE))

  infct<-function(clas,clname=NULL){
       
       acc<-unlist(lapply(clas,function(x) meanfct(x$acc.iter)))
       acc2<-unlist(lapply(clas,function(x) sdfct(x$acc.iter)))

       mar<-unlist(lapply(clas,function(x) meanfct(x$mar.iter)))
       mar2<-unlist(lapply(clas,function(x) sdfct(x$mar.iter)))

       auc<-unlist(lapply(clas,function(x) meanfct(x$auc.iter)))
       auc2<-unlist(lapply(clas,function(x) sdfct(x$auc.iter)))
     
       tmp<-cbind(acc,acc2,mar,mar2,auc,auc2)
       if(is.null(clname))
         clname<-paste("Cl",1:nrow(tmp),sep="_")
       dimnames(tmp)=list(clname,col.nam)
       tmp
  }
 
  if(sortDis){ ### sorted by discrimination tasks
  lmod2<-list()
  ludis=unique(mc.obj$cldef[lmod,7])
  for(i in 1:length(ludis)){
     l=which(mc.obj$cldef[lmod,7]==ludis[i])
     lmod2[[i]]=list(lmod[l],ludis[i])
  }

  res<-lapply(lmod2,function(y){
     infct(lapply(y[[1]],function(x) mc.obj$clas[[x]]),
       clname=mc.obj$cldef[y[[1]],6])  
   })
  }
  else{ ### sorted by algo tasks
  lmod2<-list()
  ludis=unique(mc.obj$cldef[lmod,6])
  for(i in 1:length(ludis)){
     l=which(mc.obj$cldef[lmod,6]==ludis[i])
     lmod2[[i]]=list(lmod[l],ludis[i])
  }

  res<-lapply(lmod2,function(y){
     infct(lapply(y[[1]],function(x) mc.obj$clas[[x]]),
       clname=mc.obj$cldef[y[[1]],7])  
   })
  }
  
  ret<-list(res=res,task=unlist(lapply(lmod2,function(x) x[[2]])),
     title="Multiple classifiers predictions summary")

class(ret) <- "mc.summary"
ret
}

######################################################


#' Summary of a predictor in mc.agg object
#' 
#' Convenience function to output statistics related to accuracy, AUC or
#' margins at each iteration for one model or a selection of models contained
#' in a \code{mc.agg} object (see details \code{\link{mc.agg}}).
#' 
#' 
#' @usage mc.meas.iter(mc.obj, lmod = NULL,type="acc",nam="Model")
#' @param mc.obj \code{mc.agg} object - See details \code{\link{mc.agg}}
#' @param lmod List of models to be considered - Default: all models
#' @param type Predictor type - Can be either acc (accuracy), auc (AUC), mar
#' (margin or equivalent)
#' @param nam List of names to be used in the result - Names given here
#' corresponds to the column name of \code{mc.obj$cldef}
#' @return Data frame containing statistic of interest at each iteration.
#' @author David Enot \email{dle@@aber.ac.uk}
#' @seealso \code{\link{mc.agg}}
#' @keywords manip
#' @examples
#' 
#' data(iris)
#' dat=as.matrix(iris[,1:4])
#' cl=as.factor(iris[,5])
#' lrnd=sample(1:150)[1:50]
#' cl[lrnd]=sample(cl[lrnd])  ## add a bit of misclassification for fun
#' pars   <- valipars(sampling = "cv",niter = 10, nreps=4)
#' dat1=dat.sel1(dat,cl,pwise="virginica",mclass=NULL,pars=pars)
#' 
#' res1=lapply(dat1,function(x) accest(x,clmeth="lda"))
#' res2=lapply(dat1,function(x) accest(x,clmeth="randomForest",ntree=50))
#' 
#' ## Aggregate res1 and res2
#' mc=mc.agg(res1,res2)
#' 
#' ## AUC in each model
#' auc.iter<-mc.meas.iter(mc,type="auc",nam=c("DisId","Alg"))
#' ## Plot them
#' boxplot(auc.iter)
#' ## Print on the screen
#' print(auc.iter)
#' 
mc.meas.iter<-function(mc.obj,lmod=NULL,type="acc",nam="Model"){

if(!inherits(mc.obj, "mc.agg"))
  stop("method is only valid for mc.agg objects")

if(is.null(lmod))
 lmod=1:nrow(mc.obj$cldef)

lnam<-c("Model","Alg","Arg","Arg","Dis","AlgId","DisId")
rep<-match(nam,lnam)
rep=rep[!is.na(rep)]
if(length(rep)>0){
  nam2<-mc.obj$cldef[lmod,rep[1]]
  if(length(rep)>1)
    for(i in rep[-1]){
    nam2=paste(nam2,mc.obj$cldef[lmod,i],sep="-")
    }
  nam<-nam2
}

tmp<-lapply(lmod,function(j)
    eval(parse(text=paste("vect=mc.obj$clas[[j]]$",type,".iter",sep=""))))
res<-do.call("cbind",tmp)
dimnames(res)=list(1:nrow(res),nam2)
as.data.frame(res)
}


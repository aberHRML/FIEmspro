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

mc.roc <- function(mc.obj, lmod = 1, method = "all") UseMethod("mc.roc")

#############
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


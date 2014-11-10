batchcorr<-function(mat,batch,type="median",mate=NULL,batchte=NULL){
ubatch <- unique(batch)
mmat<-NULL
for(k in ubatch){
mmat<-rbind(mmat,apply(mat[batch==k,],2,type))
}
meanmmat=apply(mmat,2,type)
mmat2<-sweep(mmat,2,meanmmat,"-")
for(k in 1:length(ubatch)){
l=which(batch==ubatch[k])
mat[l,]<-sweep(mat[l,,drop=FALSE],2,mmat2[k,],"-")
if(!is.null(mate)){
 l=which(batchte==ubatch[k])
 if(length(l)>1)
   mate[l,]<-sweep(mate[l,,drop=FALSE],2,mmat2[k,],"-")
 if(length(l)==1)
   mate[l,]<-mate[l,]-mmat2[k,]
}

}
if(!is.null(mate))
 return(list(tr=mat,te=mate))
else
 return(mat)

}

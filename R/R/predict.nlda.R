`predict.nlda` <-
function(object, newdata, dim2use=NULL,...)
{
  if(!inherits(object, "nlda")) stop("object not of class \"nlda\"")
  if (missing(newdata)) {
    return(list(class = object$pred, x = object$x, xmeans=object$xmeans,
                conf = object$conf, acc = object$acc))
  }
  if(is.null(dim(newdata)))
    dim(newdata) <- c(1, length(newdata))  ## a row vector
  newdata <- as.matrix(newdata)		   
  if(ncol(newdata) != nrow(object$loadings)) stop("wrong number of variables")

  g <- length(object$lev)
  
  ## rotated data (projection)
  x <- sweep(newdata, 2, object$means) %*% object$loadings

  if(is.null(dim2use))
    dim2use <- ncol(x)

#if(type==1){

#  mdist=as.matrix(dist(rbind(object$xmeans[,1:dim2use,drop=F],x[,1:dim2use,drop=F])))
#  mdist=mdist[1:g,(g+1):ncol(mdist)]
#  prob=(1-t(sweep(mdist,2,apply(mdist,2,sum),"/")))/g
#  pred = apply(prob,1,which.max)
#  pred <- factor(dimnames(prob)[[2]][pred], levels = object$lev)
#}
#else{

  nbmod=naiveBayes(data.frame(object$x[,1:dim2use,drop=F]),object$cl)
  prob=predict(nbmod,data.frame(x[,1:dim2use,drop=F]),type="raw")
  pred = apply(prob,1,which.max)
  pred <- factor(levels(object$cl)[pred], levels = object$lev)
#}

  ##  list(class = pred, prob = prob , x = x,  xmeans = object$xmeans, dim2use=dim2use)
  list(class = pred, prob = prob,posterior = prob , x = x,  xmeans = object$xmeans, dim2use=dim2use)

}


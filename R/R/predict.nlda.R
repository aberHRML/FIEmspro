#' Classify Multivariate Observations by 'nlda'
#' 
#' Classify multivariate observations in conjunction with \code{nlda}, and also
#' project data onto the linear discriminants.
#' 
#' This function is a method for the generic function \code{predict()} for
#' class \code{nlda}. If \code{newdata} is omitted, the results of training
#' data in \code{nlda} object will be returned.
#' 
#' @usage \method{predictnlda}(object, newdata, dim2use = NULL, \dots{})
#' @param object Object of class \code{nlda}.
#' @param newdata A matrix or data frame of cases to be classified.
#' @param dim2use The dimension of rotated data set to be used in prediction.
#' @param \dots Arguments passed to or from other methods.
#' @return A list with components: \item{class}{ The predicted class (a
#' factor).  } \item{x}{ The projections of test data on discriminant
#' variables.  } \item{prob}{ The posterior probabilities for the predicted
#' classes.  } \item{xmeans}{ The group means obtained from training.  }
#' \item{dim2use}{ The dimension of rotated data set to be used in prediction.
#' }
#' @author David Enot \email{dle@@aber.ac.uk} and Wanchang Lin
#' \email{wll@@aber.ac.uk}.
#' @seealso \code{\link{nlda}}, \code{\link{plot.nlda}}
#' @keywords classif
#' @examples
#' 
#' 
#' data(abr1)
#' cl   <- factor(abr1$fact$class)
#' dat  <- abr1$pos
#' 
#' ## divide data as training and test data
#' idx <- sample(1:nrow(dat), round((2/3)*nrow(dat)), replace=FALSE) 
#' 
#' ## constrcuct train and test data 
#' train.dat  <- dat[idx,]
#' train.t    <- cl[idx]
#' test.dat   <- dat[-idx,]        
#' test.t     <- cl[-idx] 
#' 
#' ## apply NLDA
#' model    <- nlda(train.dat,train.t)
#' pred.te  <- predict(model, test.dat)
#' 
#' ## confusion matrix
#' table(test.t,pred.te$class)
#' 
#' 
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


## wll-29-08-2007: deal with Character strings for title


#' Hierarchical Clustering for Class 'nlda'
#' 
#' Hierarchical clustering based on the Mahalanobis distances between group
#' centers for class \code{nlda}. Group centers coordinates of the training
#' data points can be calculated using all possible discriminant functions or a
#' selected number of discriminant functions.
#' 
#' 
#' @usage hca.nlda(x, method="complete",df2use=NULL, main="Aggregation of group
#' centres", ylab="Mahalanobis distance", xlab="", sub="",\dots{})
#' @param x An object of class \code{nlda}.
#' @param method Agglomeration method to be used. This should be an unambiguous
#' abbreviation of \code{"ward"}, \code{"single"}, \code{"complete"},
#' \code{"average"}, \code{"mcquitty"}, \code{"median"} or \code{"centroid"}.
#' @param df2use Discriminant functions to be included in the HCA (by default
#' all of DFs are considered if \code{df2use=NULL}).
#' @param main,sub,xlab,ylab Character strings for annotating the HCA plot.
#' For details, see \code{\link{plclust}}.
#' @param \dots Additional arguments to \code{plclust}. For details, see
#' \code{\link{plclust}}.
#' @author David Enot \email{dle@@aber.ac.uk} and Wanchang Lin
#' \email{wll@@aber.ac.uk}.
#' @seealso \code{\link{nlda}}, \code{\link{predict.nlda}}
#' @keywords hplot
#' @examples
#' 
#' ## load abr1
#' data(abr1)
#' cl   <- factor(abr1$fact$class)
#' dat <- preproc(abr1$pos , y=cl, method=c("log10","TICnorm"),add=1)[,110:500]  
#' 
#' ## build nlda model
#' model    <- nlda(dat,cl)
#' 
#' ## HCA using all DFs
#' hca.nlda(model)
#' 
#' ## or only using the first 2 DFs
#' hca.nlda(model,df2use=1:2)
#' 
#' 
`hca.nlda` <-  function(x, method="complete",df2use=NULL, 
                        main="Aggregation of group centres",
                        ylab="Mahalanobis distance",xlab="",sub="",...)
{
  if(length(x$prior)<3){stop('Needs more than 2 classes')}
  if(is.null(df2use)){df2use=1:(length(x$prior)-1)}
  mdist <- dist(x$xmeans[,df2use])
  hc    <- hclust(mdist,method)
  plot(hc,main=main,ylab=ylab,xlab=xlab,sub=sub,...)
}


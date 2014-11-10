#' Scatter Plot by Group
#' 
#' Create a scatter plot by group.
#' 
#' The scatter plot is by columns of \code{dat}. If \code{legend.loc} is
#' \code{mousepoint}, legend will be located by the mouse point.
#' 
#' @usage grpplot(dat, group, col = NULL, pch = NULL, cex = 1, legend.loc =
#' "rightside",...)
#' @param dat A matrix or data frame to be plotted.
#' @param group A factor or vector giving group information of columns of
#' \code{dat}.
#' @param col A strings of colors recognized by the \code{plot} function. Or a
#' vector of color values. For more details, see \code{?colors}.
#' @param pch A vector of symbol values recognized by the \code{plot} function.
#' @param cex Size of symbols.
#' @param legend.loc Position of legend. The location should be one of
#' \code{bottomright}, \code{bottom}, \code{bottomleft}, \code{left},
#' \code{topleft}, \code{top}, \code{topright}, \code{right},
#' \code{center},\code{mousepoint} and \code{rightside}.
#' @param \dots Additional graphics parameters passed to \code{plot}, such as
#' \code{main}, \code{xlab} and \code{ylab}.
#' @author Wanchang Lin \email{wll@@aber.ac.uk}
#' @keywords hplot
#' @examples
#' 
#' data(iris)
#' grpplot(iris[,1:2], iris[,5],main="IRIS DATA",legend.loc="topleft")
#' 
#' ## color values for the group and symbol size
#' grpplot(iris[,1:2], iris[,5],main="IRIS DATA",col=c(4,5,6),cex=1.2,
#'         legend.loc="topright") 
#' 
#' ## color and symbols for the group. legend location
#' grpplot(iris[,c(1,3)], iris[,5], col=c("red", "green3", "blue"),
#'         pch=c(15,16,17), main="IRIS DATA",legend.loc="topleft")
#'       
#' ## plot vector
#' grpplot(iris[,3], iris[,5],main="IRIS DATA",legend.loc="top")
#' 
#' ## pairs plot. 
#' grpplot(iris, iris[,5],main="IRIS DATA")
#' 
#' 
`grpplot` <-
function(dat, group,col=NULL,pch=NULL,cex=1,legend.loc="rightside",...)
{
  ## validity checking should go here.
  if (is.vector(dat)|| ncol(dat)==1) {
    dat <- as.matrix(dat)
  } else {  
    dat <- as.data.frame(dat)
  }

  legend.loc <- match.arg(legend.loc, c("bottomright", "bottom", "bottomleft",
                                        "left", "topleft", "top", "topright",
                                        "right", "center","mousepoint", "rightside"))
  
  group <- factor(group)
  n     <- nlevels(group)
  
  col <- if (is.null(col)) unclass(group) else col[group]
  pch <- if (is.null(pch)) unclass(group) else pch[group]
  
  if (ncol(dat) <= 2) {
    if (legend.loc == "rightside") {
      oldpar <- par(mar = par()$mar+c(0,0,0,4),xpd=TRUE)
      plot(dat, col=col,pch=pch,cex=cex,...)
      legend(par("usr")[2] + 1.5,par("usr")[4],legend=sort(unique(levels(group))),cex=cex, 
             col = sort(as.vector(unique(col))), pch = sort(as.vector(unique(pch))) )
      par(oldpar)
    } else if (legend.loc == "mousepoint"){     ## use mouse point to locate legend. 
      plot(dat, col=col,pch=pch,cex=cex,...)
      legend(locator(1), legend=sort(unique(levels(group))), cex=cex,
             col = sort(as.vector(unique(col))), pch = sort(as.vector(unique(pch))) )
    } else {
      plot(dat, col=col,pch=pch,cex=cex,...)
      legend(legend.loc, inset=0.02,legend=sort(unique(levels(group))),cex=cex, 
             col = sort(as.vector(unique(col))), pch = sort(as.vector(unique(pch))) )
    }
  } else {
    plot(dat, col=col,pch=pch,cex=cex,...)
  }
}


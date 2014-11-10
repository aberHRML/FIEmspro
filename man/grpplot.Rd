% 26-02-2-007:
%
\name{grpplot}
\alias{grpplot}
\title{Scatter Plot by Group}
\description{Create a scatter plot by group.}

\usage{
  grpplot(dat, group, col = NULL, pch = NULL, cex = 1, 
          legend.loc = "rightside",...)
}

\arguments{
  \item{dat}{ A matrix or data frame to be plotted. }
  \item{group}{ A factor or vector giving group information of columns of 
    \code{dat}.}
  \item{col}{ A strings of colors recognized by the \code{plot} function. Or a 
    vector of color values. For more details, see \code{?colors}. }
  \item{pch}{ A vector of symbol values recognized by the \code{plot} function. }
  \item{cex}{ Size of symbols. }
  \item{legend.loc}{ Position of legend. The location  should be one of \code{bottomright}, \code{bottom}, \code{bottomleft}, \code{left}, 
   \code{topleft}, \code{top}, \code{topright}, \code{right}, 
   \code{center},\code{mousepoint} and \code{rightside}.   
  }
  \item{\dots}{ Additional graphics parameters passed to \code{plot}, such as 
    \code{main}, \code{xlab} and \code{ylab}. }
}

\details{
  The scatter plot is by columns of \code{dat}. If \code{legend.loc} is \code{mousepoint}, 
  legend will be located by the mouse point.  
}

\author{Wanchang Lin \email{wll@aber.ac.uk}}

\examples{
data(iris)
grpplot(iris[,1:2], iris[,5],main="IRIS DATA",legend.loc="topleft")

## color values for the group and symbol size
grpplot(iris[,1:2], iris[,5],main="IRIS DATA",col=c(4,5,6),cex=1.2,
        legend.loc="topright") 

## color and symbols for the group. legend location
grpplot(iris[,c(1,3)], iris[,5], col=c("red", "green3", "blue"),
        pch=c(15,16,17), main="IRIS DATA",legend.loc="topleft")
      
## plot vector
grpplot(iris[,3], iris[,5],main="IRIS DATA",legend.loc="top")

## pairs plot. 
grpplot(iris, iris[,5],main="IRIS DATA")

}

\keyword{hplot}

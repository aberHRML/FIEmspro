%
\name{hca.nlda}
\alias{hca.nlda}
\title{
  Hierarchical Clustering for Class 'nlda'
}
\description{
   Hierarchical clustering based on the Mahalanobis distances between group 
   centers for class \code{nlda}. Group centers coordinates of the training data 
   points can be calculated using all possible discriminant functions or a 
   selected number of discriminant functions.
}

\usage{
  hca.nlda(x, method="complete",df2use=NULL,
           main="Aggregation of group centres",
           ylab="Mahalanobis distance",
           xlab="", sub="",\dots)

}

% ----------------------------------------------------------------------------
\arguments{
  \item{x}{
  An object of class \code{nlda}.
  }
  \item{method}{Agglomeration method to be used. This should
    be an unambiguous abbreviation of
    \code{"ward"}, \code{"single"}, \code{"complete"},
    \code{"average"}, \code{"mcquitty"}, \code{"median"} or
    \code{"centroid"}.}
  \item{df2use}{
    Discriminant functions to be included in the HCA (by default all of DFs are
    considered if \code{df2use=NULL}).
  }
  \item{main, sub, xlab, ylab}{Character strings for annotating the HCA plot. 
    For details, see \code{\link{plclust}}.
  }
  \item{\dots}{
  Additional arguments to \code{plclust}. For details, see \code{\link{plclust}}.
  }
}

% ----------------------------------------------------------------------------
\author{
  David Enot \email{dle@aber.ac.uk} and Wanchang Lin \email{wll@aber.ac.uk}.
}


\seealso{
  \code{\link{nlda}}, \code{\link{predict.nlda}}
}

\examples{
## load abr1
data(abr1)
cl   <- factor(abr1$fact$class)
dat <- preproc(abr1$pos , y=cl, method=c("log10","TICnorm"),add=1)[,110:500]  

## build nlda model
model    <- nlda(dat,cl)

## HCA using all DFs
hca.nlda(model)

## or only using the first 2 DFs
hca.nlda(model,df2use=1:2)

}

\keyword{hplot}

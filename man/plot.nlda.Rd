% wll-14-06-2006: 
%
\name{plot.nlda}
\alias{plot.nlda}
\title{
Plot Method for Class 'nlda'
}
\description{
Plots a set of data on one, two or more linear discriminants.
}

\usage{
\method{plot}{nlda}(x, panel = panel.nlda, cex = 0.7, dimen,
     abbrev = FALSE, \dots)
}

% ----------------------------------------------------------------------------
\arguments{
  \item{x}{
  An object of class \code{nlda}.
  }
  \item{panel}{
  The panel function used to plot the data.
  }
  \item{cex}{
  Graphics parameter \code{cex} for labels on plots.
  }
  \item{dimen}{
  The index of linear discriminants to be used for the plot.
  }
  \item{abbrev}{
  Whether the group labels are abbreviated on the plots. If \code{abbrev > 0}
  this gives \code{minlength} in the call to \code{abbreviate}.
  }
  \item{\dots}{
  Additional arguments to \code{plot}.
  }
}

% ----------------------------------------------------------------------------
\details{
This function is a method for the generic function
\code{plot()} for class \code{nlda}. The behaviour is determined by the value 
of \code{dimen}. For the length of \code{dimen} is greater than 2, a 
\code{pairs} plot is used. For the length of \code{dimen} is equal to 2, a 
scatter plot is drawn. Otherwise, a set of histograms or density plots are drawn.
}

% ----------------------------------------------------------------------------
\author{
  Wanchang Lin \email{wll@aber.ac.uk} and David Enot \email{dle@aber.ac.uk}.
}


\seealso{
  \code{\link{nlda}}, \code{\link{predict.nlda}}, \code{\link{hca.nlda}}
}

\examples{
## load abr1
data(abr1)
cl   <- factor(abr1$fact$class)
dat <- preproc(abr1$pos , y=cl, method=c("log10","TICnorm"),add=1)[,110:500]  

## build model on all the data available
model    <- nlda(dat,cl)

## Plot second component versus first
plot(model,dimen=c(1,2),main = "Training data",abbrev = TRUE)

## Pairwise scatterplots of several components 
plot(model,main = "Training data",abbrev = TRUE)

}

\keyword{hplot}

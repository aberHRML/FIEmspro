% wll-26-02-2007
% dle-14-09-2007

\name{onebc}
\alias{onebc}

\title{
  Metabolomics Fingerprint Baseline Correction
}

\description{
  Core function to perform baseline correction on one metabolomics fingerprint.
}

\usage{
  onebc(x,wsize=50,qtl=0.1,maxy=1000,plotting=TRUE,title=TRUE,
        sampid=NULL)
}

\arguments{
  \item{x}{
    A numeric vector to be processed. 
  }
  \item{wsize}{Window size.}
  \item{qtl}{ 
   A numeric value of for lower quantile probability. 
  }
  \item{maxy}{
  A numeric value specifying y axis maximal value to be plotted.
  }
  \item{plotting}{
    A logical value indicating whether or not plotting.
  }
  \item{title}{
   A logical value indicating whether or not to show plot title.
  }
  \item{sampid}{
   Sample ID to be written in the title if both plotting and title are TRUE 
   (useful when \code{onebc} called from \code{multibc}).
  }
}

\value{
  A list containing the following components:
  \item{x}{A numeric vector of the resulting fingerprint after baseline correction. 
  }
  \item{bsl}{A numeric vector of the baseline intensities. 
  }
}

\details{
  The purpose of the baseline correction is to remove undesirable effects due 
  excess chemical noise. Possible consequences of baseline drift include the 
  possibility that the baseline may be discriminatory and that important 
  information may be obscured in areas affected by baseline problems. A simple 
  consensual approach consists in fitting a monotone local minimum curve to 
  each fingerprint. Basically, the fingerprint is divided into equally 
  spaced m/z intervals and a local minimum intensity value is returned as the 
  baseline estimate for this region. Finally, the whole fingerprint baseline is 
  computed by linear interpolation based on pairs made of the centre of the 
  interval and its corresponding local minima. Intervals (argument \code{wsize}) 
  are in the order of 30-70 amu as a trade off between the removal of relevant 
  chemical (small interval) or estimation bias due to use of a larger interval. 
  Rather than using the minimum value of an interval, it is also judicious to 
  use the value corresponding to a low quantile (argument \code{qtl}) to avoid 
  any spurious estimates due to zeros or abnormally low signals.
}

\author{
  David Enot \email{dle@aber.ac.uk} and Wanchang Lin \email{wll@aber.ac.uk}.
}

% ----------------------------------------------------------------------------
\seealso{
  \code{\link{multibc}}
}

\examples{
  data(abr1)
  cl  <- factor(abr1$fact$class)
  mat <- abr1$pos

  ## baseline correction
  res <- onebc(mat[1,110:2000], qtl=0.8, sampid="1")
  
}

\keyword{manip}

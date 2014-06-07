% wll-26-02-2007
%
\name{multibc}
\alias{multibc}
\title{
  Multiple Metabolomics Fingerprint Baseline Correction
}
\description{
  Wrapper for function \code{onebc} for several  
  samples. An additional plotting parameter for visualizing the  
  baseline correction process is added.
}
\usage{
  multibc(x,wsize=50,qtl=0.1,maxy=1000,plotting=FALSE,pause=0)
}
\arguments{
  \item{x}{
    A data frame or matrix to be processed. 
  }
  \item{wsize}{Window size.}
  \item{qtl}{ 
   A numeric value of probability with values in [0,1]. 
  }
  \item{maxy}{
  A numeric value which specifies maximal intensity to be plotted on y axis.
  }
  \item{plotting}{
    A logical value indicating whether or not plotting.
  }
  \item{pause}{Time interval defining pause between each baseline correction
  in order to visualise each individual plot during the BC process if 
  \code{plotting} is \code{TRUE}.}
}

\value{
  A list containing the following components:
  \item{x}{Sample matrix after baseline subtraction. 
  }
  \item{bsl}{Baseline matrix. 
  }
}

\author{
  David Enot \email{dle@aber.ac.uk}
}

% ----------------------------------------------------------------------------
\seealso{
  \code{\link{onebc}}
}

\examples{
  ## load abr1
  data(abr1)
  mat <- abr1$pos[1:5,110:2000]

  ## baseline correction
  res <- multibc(mat,plotting=TRUE,pause=0.5)
  
}

\keyword{manip}

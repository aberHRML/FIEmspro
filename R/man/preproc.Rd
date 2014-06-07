% wll-01-12-2006:
%
\name{preproc}
\alias{preproc}
\title{
  Data Tranformation Wrapper
}
\description{
  Wrapper for several techniques to perform data transformation of the original 
  fingerprint matrix.
}
\usage{
  preproc (x, y=NULL,method="log",add=1)
}
\arguments{
  \item{x}{
    A numeric data frame or matrix to be pre-processed. 
  }
  \item{method}{
    A method used to pre-process the data set. The following methods are supported:
    \itemize{
      \item \code{center:} Centering
      \item \code{auto:} Auto scaling
      \item \code{range:} Range scaling
      \item \code{pareto:} Pareto scaling
      \item \code{vast:} Vast scaling
      \item \code{level:} Level scaling
      \item \code{log:} Log transformation (default)
      \item \code{log10:} Log 10 transformation
      \item \code{sqrt:} Square root transformation
      \item \code{asinh:} Inverse hyperbolic sine transformation
      \item \code{TICnorm:} TIC normalisation
    }
  }
  \item{add}{
  A numeric value for addition used in the logarithmic transformations \code{log}
  and \code{log10}.
  }
  \item{y}{
    A factor specifying the class for each observation. It is only used by the 
    method \code{TICnorm}.
  }
}

\details{
  Purpose of normalisation is to remove inter-spectrum sources of variability 
  that come mainly from different sample concentration, loss of sensitivity of 
  the detector over time or degradation of certain samples. Global normalisation 
  by rescaling each measurement within a spectrum by a constant factor, such as 
  the sum of all the spectra intensities (\code{TICnorm} method). However, the 
  default use of TIC normalisation can lead to the generation of spurious 
  knowledge if the overall sample intensity is class/factor dependent. If this 
  is the case, a straightforward approach is to remove the difference, between 
  the average scale of the corresponding class and the average scale, from the 
  scale factor by providing the class/factor of interest (argument \code{y}) 
  while calling \code{preproc}.

}


\value{
  Transformed sample matrix.
}

\author{
  Wanchang Lin \email{wll@aber.ac.uk} and David Enot \email{dle@aber.ac.uk}.
}

% ----------------------------------------------------------------------
\references{
Berg, R., Hoefsloot, H., Westerhuis, J., Smilde, A. and Werf, M. (2006),
Centering, scaling, and transformations: improving the biological
information content of metabolomics data, \emph{BMC Genomics}, 7:142
}

\examples{
data(abr1)
cl   <- factor(abr1$fact$class)
dat  <- abr1$pos[,110:2000]

## normalise data set using class dependent "TICnorm"
z.1  <- preproc(dat, y=cl, method="TICnorm")

## scale data set using "log10"
z.2 <- preproc(dat,method="log10", add=1)

## or scale data set using "log" and "TICnorm" sequentially
z.3 <- preproc(dat,method=c("log","TICnorm"), add=0.1)

}

\keyword{manip}

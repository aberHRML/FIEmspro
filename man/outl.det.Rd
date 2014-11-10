% wll-14-03-2007
% dle-14-09-2007

\name{outl.det}
\alias{outl.det}
\title{
 Detection and Display Outliers
}
\description{
 Wrapper for the detection of sample outliers by computation of Mahalanobis distances using 
 \code{cov.rob} from package \pkg{MASS}. The (robust) square root distance from 
 the center is displayed alongside a 2D mapping of the data and its confidence 
 ellipse.
}
\usage{
outl.det(x, method = "classical", conf.level = 0.975, 
            dimen=c(1,2), tol = 1e-7, plotting = TRUE)
}

% --------------------------------------------------------------------
\arguments{
  \item{x}{
  A data.frame or matrix. 
  }
  \item{method}{
  The method to be used: 
    \itemize{
      \item \code{mve}. Minimum volume ellipsoid.
      \item \code{mcd}. Minimum covariance determinant.
      \item \code{classical}. Classical product-moment.
    }
  For details, see \code{cov.rob} in package \pkg{MASS}.  
  }
  \item{conf.level}{
  The confidence level for controlling the cutoff of Mahalanobis distances.
  }
  \item{dimen}{Dimensions used to plot tolerance ellipse and the data points 
  alongside these two dimensions. 
  
  }
  \item{tol}{
   The tolerance to be used for computing Mahalanobis distances 
   (see \code{cov.rob} in package \pkg{MASS})
  }
  \item{plotting}{
  A logical value. If \code{TRUE}, The Mahalanobis distances against the index 
  of data samples and the tolerance ellipse of the data samples are plotted. 
  }
}


% ----------------------------------------------------------------------------
\value{
  A list with components:
  \item{outlier}{List of outliers detected.}
  \item{conf.level}{Confidence level used.}
  \item{mah.dist}{Mahalanobis distances of each data sample.}
  \item{cutoff}{ Cutoff of Mahalanobis distances for outliers detection.}
}

\details{
  If the number of samples is \code{n} and number of variables in a sample is 
  \code{p}, the data set must be \code{n > p + 1}. In this case, PCA can be 
  used to produce fewer directions of uncorrelated dimensions that explain 
  different dimensions in the data. Due to the inherent difficulties in 
  defining outliers, inclusion of the first few dimensions only is almost 
  always sufficient to compute Mahalanobis distances. However in more complex 
  designs implicating various factors and/or multiple levels, different 
  contributions to the overall variation modelled by PCA may be confounded in 
  such a reduced space. In such situation, the initial dataset must be 
  decomposed into smaller problems to relate potential outlying
  behaviour. 
}

% ----------------------------------------------------------------------------
\author{
  Wanchang Lin \email{wll@aber.ac.uk} and David Enot \email{dle@aber.ac.uk}
}

% ----------------------------------------------------------------------
\examples{
## load abr1
data(abr1)
y   <- factor(abr1$fact$class)
x <- preproc(abr1$pos , y=y, method=c("log10","TICnorm"),add=1)[,110:1000]  
## Select classes 1 and 2
tmp <- dat.sel(x, y, choices=c("1","2"))
dat <- tmp$dat[[1]]
ind <- tmp$cl[[1]]


## dimension reduction by PCA
x   <- prcomp(dat,scale=FALSE)$x

## perform and plot outlier detection using classical Mahalanobis distance
## on the first 2 PCA dimensions
res <- outl.det(x[,c(1,2)], method="classical",dimen=c(1,2),
                    conf.level = 0.975)

}

\keyword{classif}

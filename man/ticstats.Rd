% wll-26-02-2007
% dle-14-09-2007
%
\name{ticstats}
\alias{ticstats}
\title{
  Compute and Display Total Ion Count (TIC) statistics
}


\description{
  By definition the total ion count of a spectrum is the sum of all the m/z 
  signal intensities (sum over the columns).  A robust regression can be built 
  to model the effect of the injection order on the TIC of each sample. The 
  fitting residuals are used to evaluate the median of the absolute deviations 
  (MAD) from the linear equation. Sample TIC and linear model are then plotted 
  according to the injection order to identify potential outliers and or structure 
  in the data.
}

\usage{
  ticstats(x,injorder=NULL,thres=3)
}
\arguments{
  \item{x}{
    A numeric data frame or matrix to be processed. 
  }
  \item{injorder}{
    A numeric vector corresponding to the injection order of each sample.
  }
  \item{thres}{
    A numeric value of threshold for detecting outlier. 
  }
}

\value{
  A list containing the following components:
  \item{resid}{Sample residuals.}
  \item{mod}{Robust linear model.}
  \item{loutl}{List of outlying samples as defined by the 
       \code{thres} argument.}

}

\details{
  As an easy diagnostic measure, the TIC can provide an estimation of factors 
  that may affect the overall intensity of the run such as gradual instrument 
  drift (e.g. resulting from loss of sensitivity of the ion source), or step 
  changes in instrument characteristics after maintenance.  Also, an examination 
  of the TIC can reveal suspicious samples where unusually low or especially 
  high signal intensities in some runs may be due to contamination or poorly 
  extracted samples. A regression can be built to model the effect of the 
  injection order on the TIC of each sample. As a conservative rule, any sample 
  that deviates more than 2/3 (argument \code{thres}) times from the MAD must be 
  examined manually to identify the origin of the different intensity behaviour 
  and then removed before further statistical analysis if corrective measures do 
  not improve the individual fingerprint. Further assessment of outlying samples 
  is discussed later. In any case where a linear relationship (i.e. gradually 
  changing TIC in sample set) is observed between the injection order and sample 
  TIC, this dependency will be removed by TIC normalisation. If other structure 
  related to the order of injection is noticed, for example an analytical batch 
  effect (i.e. a step change in TIC at beginning or in middle of an injection 
  series), the user must identify its potential origin (e.g. changes in machine 
  calibration or mobile phase) and possibly create a new experimental factor 
  (batch) where each step change in level corresponds to the start of a new batch.
}

\author{
  David Enot and Wanchang Lin \email{dle,wll@aber.ac.uk}
}

\seealso{
  \code{\link[MASS]{rlm}}.
} 

\examples{
  data(abr1)
  dat <- abr1$pos
  res <- ticstats(dat,injorder=NULL)

}

\keyword{manip}

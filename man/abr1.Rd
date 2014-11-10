% wll-12-10-2006: First draft
% dle-drp-15-09-2007: Major changes

\name{abr1}
\docType{data}
\alias{abr1}
\alias{FIEmspro}       % dirty trick for luch help by typing ?FIEmspro

\title{abr1 dataset}
\usage{
data(abr1)
}

\description{
  Real world FIE-MS dataset.
}

\value{
  A list with the following elements:
  \item{fact}{A data frame containing experimental meta-data.}
  \item{pos}{A data frame for positive data with 120 observations and 2000 variables.}
  \item{neg}{A data frame for negative data with 120 observations and 2000 variables.}
}

\details{
  FIE-MS data matrices developed from analysis of samples representing a time 
  course of pathogen attack in a model plant species (Brachypodium distachyon). 
  The data was developed in a single batch with all samples randomised using a 
  Thermo LTQ linear ion trap processed using \code{fiems_ltq_main}. Both positive 
  and negative ion mode are given (\code{abr1$pos} and \code{abr1$neg}). To avoid 
  confusions, variable names are given with a letter corresponding to the ionisation 
  mode followed by the actual nominal mass value (e.g. P130 corresponds to the 
  nominal mass 130 in the positive mode). 
  
  Experimental factors are given in the
   \code{abr1$fact} data frame:
  \itemize{
    \item \code{injorder:} sample injection order
    \item \code{name:} sample name
    \item \code{rep:} biological replicate for a given class
    \item \code{day:} number of days following infection after which the sample 
          has been harvested - Level H corresponds to an healthy plant.
    \item \code{class:} identical to day except that \code{class=6} when \code{day=H}
    \item \code{pathcdf,filecdf,name.org,remark:} are generated from profile 
          processing and are kept for  traceability purposes.
  }
 Factor of interest for classification are contained in \code{abr1$fact$day}.
  There are 20 biological replicates in each class has 

}

\author{
  Manfred Beckmann, David Enot and Wanchang Lin \email{meb,dle, wll@aber.ac.uk}
}


\examples{
# Load data set
data(abr1)

# Select data set
dat <- abr1$neg

# number of observations and variables in the negative mode matrix
dim(dat)

# names of the variables
dimnames(dat)[[2]]

# print out the experimental factors
print(abr1$fact)

# check out the repartition of class
table(abr1$fact$class)
}    
\keyword{datasets}

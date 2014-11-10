% lwc-21-03-2007

\name{fs.techniques}
\alias{fs.techniques}
\alias{fs.anova}
\alias{fs.auc}
\alias{fs.bw}
\alias{fs.kruskal}
\alias{fs.mi}
\alias{fs.relief}
\alias{fs.rf}
\alias{fs.snr}
\alias{fs.welch}

\title{
  Implementation of Feature Ranking Techniques 
}
\description{
  Implementation of feature ranking techniques. 
}
\usage{
  fs.anova(x,y,\dots)
  fs.auc(x,y)
  fs.bw(x,y)
  fs.kruskal(x,y,\dots)
  fs.mi(x,y)
  fs.relief(x,y)
  fs.rf(x,y,\dots)
  fs.snr(x,y)
  fs.welch(x,y,\dots)
}

% --------------------------------------------------------------------
\arguments{
  \item{x}{
  A data frame or matrix of data set. 
  }
  \item{y}{
  A factor or vector of class.
  }
  \item{\dots}{
  Optional arguments to be passed to the feature ranking method.
  }
}

\value{
  A list with components:
  \item{fs.rank}{A vector of feature ranks.}
  \item{fs.order}{A vector of feature ids in decreasing order of saliency.}
  \item{stats}{A vector of the original statistic/quantity describing feature saliency.}
  \item{pval}{A vector of p values if calculated by the feature ranking method.}
}


\details{
Several techniques are implemented in the current packages:
\describe{
    \item{\bold{fs.anova}:}{Wrapper for function \code{\link{oneway.test}}.
     Performs an analysis 
      of variance to test whether means from normal distributions are 
      identical. It assumes that group variances are not necessarily equal. 
      The F value is used to compute feature ranks
       - Two and multiple class problems both allowed.}
    \item{\bold{fs.auc}:}{Compute the area under the simple ROC curve (x axis: 
      false positive, y-axis: true positive rate) for each individual feature. 
      The actual value of the AUC (if class 1 > class 2) or its complement 
      (if class 1 < class 2) is used to get the feature ranking - Two class problems only.}
    \item{\bold{fs.bw}:}{Compute the ratio of between-group to within-group sums of 
      squares for each feature without assuming any particular data 
      distributions - Two and multiple class problems both allowed.} 
    \item{\bold{fs.kruskal}:}{Wrapper for function \code{\link{kruskal.test}} - Non parametric 
      alternative that handles two and multiple class problems.} 
    \item{\bold{fs.mi}:}{Compute the mutual information between the two classes - Two 
      class problems only.}
    \item{\bold{fs.relief}:}{Implementation of the RELIEF algorithm to calculate 
      relevance scores in a multivariate fashion - Two and multiple class
      problems both allowed.}
    \item{\bold{fs.rf}:}{Wrapper for randomForest function to compute importance 
      scores in a multivariate fashion. The mean decrease in accuracy is used 
      to calculate feature scores. Further arguments related to the random 
      forests algorithm can also be passed - Two and multiple class problems both 
      allowed.}
    \item{\bold{fs.snr}:}{Compute the signal to noise ratio for each feature. The 
      absolute value of the SNR is reported and used for accessing feature 
      ranks - Two class problems only.}
    \item{\bold{fs.welch}:}{Performs a univariate t-test to test whether group means 
      from normal distributions are identical assuming that group 
      variances may not be necessarily equal. The absolute value of the t-test 
      statistics is returned and used to compute feature ranks - Two classes 
      problems only.}
}
}

% ----------------------------------------------------------------------------
\author{
  David Enot \email{dle@aber.ac.uk} and Wanchang Lin \email{wll@aber.ac.uk}.
}

\references{
  Dudoit, S., Fridlyand, J. and Speed, T.P. (2002). Comparison of discrimination methods 
  for classification of tumors using gene expression data. 
  \emph{Journal of the American Statistical Association}. Vol.97, No.457, 77-87.

  Kira, K. and Rendel, L. (1992). 
  The Feature Selection Problem: Traditional Methods and a new algorithm. 
  \emph{Proc. Tenth National Conference on Artificial Intelligence}, MIT Press, 
  129 - 134. 
  
  Kononenko, I., Simes, E., and Robnik-Sikonja, M. (1997). 
  Overcoming the myopia of induction learning algorithms with RELIEFF. 
  \emph{Applied Intelligence}, Vol.7, 1, 39-55. 

  Jeffery, I. B., Higgins,D. G. and Culhane,A. C. (2006).  Comparison and 
  evaluation of methods for generating differentially expressed gene lists 
  from microarray data. \emph{BMC Bioinformatics}, 7:359.
  
  Chen, D.,Liu, Z., Ma, X. and Hua,D. (2005). Selecting Genes by Test Statistics.
  \emph{Journal of Biomedicine and Biotechnology}. 2005:2, 132 - 138.
  
  Golub, T. R., et al., (1999). Molecular classification of cancer: class 
  discovery and class prediction by gene expression monitoring. 
  \emph{Science}, 286:531-537.
}


\seealso{
  \code{\link{oneway.test}}, \code{\link{kruskal.test}}, \code{\link[randomForest]{randomForest}},
  \code{\link{feat.rank.re}}.
} 


% ----------------------------------------------------------------------
\examples{
## prepare data set
data(abr1)
y   <- factor(abr1$fact$class)
x <- preproc(abr1$pos , y=y, method=c("log10","TICnorm"),add=1)[,110:500]  
## Only test for class 1 and 2
dat <- dat.sel(x, y, choices=c("1","2"))
mat <- dat$dat[[1]]
cl <- dat$cl[[1]]

## apply SNR method for feature ranking
res <- fs.snr(mat,cl)
names(res)


## Template R function for a user defined feature ranking function, 
## which can be used in re-sampling based feature selection 
## function: feat.rank.re.
fs.custom <- function(x, y)
{
### -------- user defined feature selection method goes here ----------
## As an example, generate random importance score
  stats        <- abs(rnorm(ncol(x)))
  names(stats) <- names(x)
### --------------------------------------------------------------------

  ### Generate rank and order
  ### Here the importance score is in decreasing order
  fs.rank <- rank(-stats, na.last = TRUE, ties.method = "random")
  fs.order <- order(fs.rank, na.last = TRUE)
  names(fs.rank) <- names(stats)
  nam <- names(stats[fs.order])
  ### return results
  list(fs.rank = fs.rank, fs.order = fs.order, stats = stats)
}

## apply fs.custom for feature ranking
res <- fs.custom(mat,cl)
names(res)


}

\keyword{classif}

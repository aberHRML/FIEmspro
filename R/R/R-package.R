

#' abr1 dataset
#' 
#' Real world FIE-MS dataset.
#' 
#' FIE-MS data matrices developed from analysis of samples representing a time
#' course of pathogen attack in a model plant species (Brachypodium
#' distachyon).  The data was developed in a single batch with all samples
#' randomised using a Thermo LTQ linear ion trap processed using
#' \code{fiems_ltq_main}. Both positive and negative ion mode are given
#' (\code{abr1$pos} and \code{abr1$neg}). To avoid confusions, variable names
#' are given with a letter corresponding to the ionisation mode followed by the
#' actual nominal mass value (e.g. P130 corresponds to the nominal mass 130 in
#' the positive mode).
#' 
#' Experimental factors are given in the \code{abr1$fact} data frame: \itemize{
#' \item \code{injorder:} sample injection order \item \code{name:} sample name
#' \item \code{rep:} biological replicate for a given class \item \code{day:}
#' number of days following infection after which the sample has been harvested
#' - Level H corresponds to an healthy plant.  \item \code{class:} identical to
#' day except that \code{class=6} when \code{day=H} \item
#' \code{pathcdf,filecdf,name.org,remark:} are generated from profile
#' processing and are kept for traceability purposes.  } Factor of interest for
#' classification are contained in \code{abr1$fact$day}.  There are 20
#' biological replicates in each class has
#' 
#' @name abr1
#' @aliases abr1 FIEmspro
#' @docType data
#' @usage data(abr1)
#' @return A list with the following elements: \item{fact}{A data frame
#' containing experimental meta-data.} \item{pos}{A data frame for positive
#' data with 120 observations and 2000 variables.} \item{neg}{A data frame for
#' negative data with 120 observations and 2000 variables.}
#' @author Manfred Beckmann, David Enot and Wanchang Lin \email{meb,dle,
#' wll@@aber.ac.uk}
#' @keywords datasets
#' @examples
#' 
#' # Load data set
#' data(abr1)
#' 
#' # Select data set
#' dat <- abr1$neg
#' 
#' # number of observations and variables in the negative mode matrix
#' dim(dat)
#' 
#' # names of the variables
#' dimnames(dat)[[2]]
#' 
#' # print out the experimental factors
#' print(abr1$fact)
#' 
#' # check out the repartition of class
#' table(abr1$fact$class)
#' 
NULL





#' Implementation of Feature Ranking Techniques
#' 
#' Implementation of feature ranking techniques.
#' 
#' Several techniques are implemented in the current packages: \describe{
#' \item{list("fs.anova")}{Wrapper for function \code{\link{oneway.test}}.
#' Performs an analysis of variance to test whether means from normal
#' distributions are identical. It assumes that group variances are not
#' necessarily equal.  The F value is used to compute feature ranks - Two and
#' multiple class problems both allowed.}\item{:}{Wrapper for function
#' \code{\link{oneway.test}}.  Performs an analysis of variance to test whether
#' means from normal distributions are identical. It assumes that group
#' variances are not necessarily equal.  The F value is used to compute feature
#' ranks - Two and multiple class problems both allowed.}
#' \item{list("fs.auc")}{Compute the area under the simple ROC curve (x axis:
#' false positive, y-axis: true positive rate) for each individual feature.
#' The actual value of the AUC (if class 1 > class 2) or its complement (if
#' class 1 < class 2) is used to get the feature ranking - Two class problems
#' only.}\item{:}{Compute the area under the simple ROC curve (x axis: false
#' positive, y-axis: true positive rate) for each individual feature.  The
#' actual value of the AUC (if class 1 > class 2) or its complement (if class 1
#' < class 2) is used to get the feature ranking - Two class problems only.}
#' \item{list("fs.bw")}{Compute the ratio of between-group to within-group sums
#' of squares for each feature without assuming any particular data
#' distributions - Two and multiple class problems both
#' allowed.}\item{:}{Compute the ratio of between-group to within-group sums of
#' squares for each feature without assuming any particular data distributions
#' - Two and multiple class problems both allowed.}
#' \item{list("fs.kruskal")}{Wrapper for function \code{\link{kruskal.test}} -
#' Non parametric alternative that handles two and multiple class
#' problems.}\item{:}{Wrapper for function \code{\link{kruskal.test}} - Non
#' parametric alternative that handles two and multiple class problems.}
#' \item{list("fs.mi")}{Compute the mutual information between the two classes
#' - Two class problems only.}\item{:}{Compute the mutual information between
#' the two classes - Two class problems only.}
#' \item{list("fs.relief")}{Implementation of the RELIEF algorithm to calculate
#' relevance scores in a multivariate fashion - Two and multiple class problems
#' both allowed.}\item{:}{Implementation of the RELIEF algorithm to calculate
#' relevance scores in a multivariate fashion - Two and multiple class problems
#' both allowed.} \item{list("fs.rf")}{Wrapper for randomForest function to
#' compute importance scores in a multivariate fashion. The mean decrease in
#' accuracy is used to calculate feature scores. Further arguments related to
#' the random forests algorithm can also be passed - Two and multiple class
#' problems both allowed.}\item{:}{Wrapper for randomForest function to compute
#' importance scores in a multivariate fashion. The mean decrease in accuracy
#' is used to calculate feature scores. Further arguments related to the random
#' forests algorithm can also be passed - Two and multiple class problems both
#' allowed.} \item{list("fs.snr")}{Compute the signal to noise ratio for each
#' feature. The absolute value of the SNR is reported and used for accessing
#' feature ranks - Two class problems only.}\item{:}{Compute the signal to
#' noise ratio for each feature. The absolute value of the SNR is reported and
#' used for accessing feature ranks - Two class problems only.}
#' \item{list("fs.welch")}{Performs a univariate t-test to test whether group
#' means from normal distributions are identical assuming that group variances
#' may not be necessarily equal. The absolute value of the t-test statistics is
#' returned and used to compute feature ranks - Two classes problems
#' only.}\item{:}{Performs a univariate t-test to test whether group means from
#' normal distributions are identical assuming that group variances may not be
#' necessarily equal. The absolute value of the t-test statistics is returned
#' and used to compute feature ranks - Two classes problems only.} }
#' 
#' @aliases fs.techniques fs.anova fs.auc fs.bw fs.kruskal fs.mi fs.relief
#' fs.rf fs.snr fs.welch
#' @usage fs.anova(x,y,\dots{}) fs.auc(x,y) fs.bw(x,y) fs.kruskal(x,y,\dots{})
#' fs.mi(x,y) fs.relief(x,y) fs.rf(x,y,\dots{}) fs.snr(x,y)
#' fs.welch(x,y,\dots{})
#' @param x A data frame or matrix of data set.
#' @param y A factor or vector of class.
#' @param \dots Optional arguments to be passed to the feature ranking method.
#' @return A list with components: \item{fs.rank}{A vector of feature ranks.}
#' \item{fs.order}{A vector of feature ids in decreasing order of saliency.}
#' \item{stats}{A vector of the original statistic/quantity describing feature
#' saliency.} \item{pval}{A vector of p values if calculated by the feature
#' ranking method.}
#' @author David Enot \email{dle@@aber.ac.uk} and Wanchang Lin
#' \email{wll@@aber.ac.uk}.
#' @seealso \code{\link{oneway.test}}, \code{\link{kruskal.test}},
#' \code{\link[randomForest]{randomForest}}, \code{\link{feat.rank.re}}.
#' @references Dudoit, S., Fridlyand, J. and Speed, T.P. (2002). Comparison of
#' discrimination methods for classification of tumors using gene expression
#' data.  \emph{Journal of the American Statistical Association}. Vol.97,
#' No.457, 77-87.
#' 
#' Kira, K. and Rendel, L. (1992).  The Feature Selection Problem: Traditional
#' Methods and a new algorithm.  \emph{Proc. Tenth National Conference on
#' Artificial Intelligence}, MIT Press, 129 - 134.
#' 
#' Kononenko, I., Simes, E., and Robnik-Sikonja, M. (1997).  Overcoming the
#' myopia of induction learning algorithms with RELIEFF.  \emph{Applied
#' Intelligence}, Vol.7, 1, 39-55.
#' 
#' Jeffery, I. B., Higgins,D. G. and Culhane,A. C. (2006).  Comparison and
#' evaluation of methods for generating differentially expressed gene lists
#' from microarray data. \emph{BMC Bioinformatics}, 7:359.
#' 
#' Chen, D.,Liu, Z., Ma, X. and Hua,D. (2005). Selecting Genes by Test
#' Statistics.  \emph{Journal of Biomedicine and Biotechnology}. 2005:2, 132 -
#' 138.
#' 
#' Golub, T. R., et al., (1999). Molecular classification of cancer: class
#' discovery and class prediction by gene expression monitoring.
#' \emph{Science}, 286:531-537.
#' @keywords classif
#' @examples
#' 
#' ## prepare data set
#' data(abr1)
#' y   <- factor(abr1$fact$class)
#' x <- preproc(abr1$pos , y=y, method=c("log10","TICnorm"),add=1)[,110:500]  
#' ## Only test for class 1 and 2
#' dat <- dat.sel(x, y, choices=c("1","2"))
#' mat <- dat$dat[[1]]
#' cl <- dat$cl[[1]]
#' 
#' ## apply SNR method for feature ranking
#' res <- fs.snr(mat,cl)
#' names(res)
#' 
#' 
#' ## Template R function for a user defined feature ranking function, 
#' ## which can be used in re-sampling based feature selection 
#' ## function: feat.rank.re.
#' fs.custom <- function(x, y)
#' {
#' ### -------- user defined feature selection method goes here ----------
#' ## As an example, generate random importance score
#'   stats        <- abs(rnorm(ncol(x)))
#'   names(stats) <- names(x)
#' ### --------------------------------------------------------------------
#' 
#'   ### Generate rank and order
#'   ### Here the importance score is in decreasing order
#'   fs.rank <- rank(-stats, na.last = TRUE, ties.method = "random")
#'   fs.order <- order(fs.rank, na.last = TRUE)
#'   names(fs.rank) <- names(stats)
#'   nam <- names(stats[fs.order])
#'   ### return results
#'   list(fs.rank = fs.rank, fs.order = fs.order, stats = stats)
#' }
#' 
#' ## apply fs.custom for feature ranking
#' res <- fs.custom(mat,cl)
#' names(res)
#' 
#' 
#' 
NULL




#' Detection and Display Outliers
#' 
#' Wrapper for the detection of sample outliers by computation of Mahalanobis
#' distances using \code{cov.rob} from package \pkg{MASS}. The (robust) square
#' root distance from the center is displayed alongside a 2D mapping of the
#' data and its confidence ellipse.
#' 
#' If the number of samples is \code{n} and number of variables in a sample is
#' \code{p}, the data set must be \code{n > p + 1}. In this case, PCA can be
#' used to produce fewer directions of uncorrelated dimensions that explain
#' different dimensions in the data. Due to the inherent difficulties in
#' defining outliers, inclusion of the first few dimensions only is almost
#' always sufficient to compute Mahalanobis distances. However in more complex
#' designs implicating various factors and/or multiple levels, different
#' contributions to the overall variation modelled by PCA may be confounded in
#' such a reduced space. In such situation, the initial dataset must be
#' decomposed into smaller problems to relate potential outlying behaviour.
#' 
#' @usage outl.det(x, method = "classical", conf.level = 0.975, dimen=c(1,2),
#' tol = 1e-7, plotting = TRUE)
#' @param x A data.frame or matrix.
#' @param method The method to be used: \itemize{ \item \code{mve}. Minimum
#' volume ellipsoid.  \item \code{mcd}. Minimum covariance determinant.  \item
#' \code{classical}. Classical product-moment.  } For details, see
#' \code{cov.rob} in package \pkg{MASS}.
#' @param conf.level The confidence level for controlling the cutoff of
#' Mahalanobis distances.
#' @param dimen Dimensions used to plot tolerance ellipse and the data points
#' alongside these two dimensions.
#' 
#' @param tol The tolerance to be used for computing Mahalanobis distances (see
#' \code{cov.rob} in package \pkg{MASS})
#' @param plotting A logical value. If \code{TRUE}, The Mahalanobis distances
#' against the index of data samples and the tolerance ellipse of the data
#' samples are plotted.
#' @return A list with components: \item{outlier}{List of outliers detected.}
#' \item{conf.level}{Confidence level used.} \item{mah.dist}{Mahalanobis
#' distances of each data sample.} \item{cutoff}{ Cutoff of Mahalanobis
#' distances for outliers detection.}
#' @author Wanchang Lin \email{wll@@aber.ac.uk} and David Enot
#' \email{dle@@aber.ac.uk}
#' @keywords classif
#' @examples
#' 
#' ## load abr1
#' data(abr1)
#' y   <- factor(abr1$fact$class)
#' x <- preproc(abr1$pos , y=y, method=c("log10","TICnorm"),add=1)[,110:1000]  
#' ## Select classes 1 and 2
#' tmp <- dat.sel(x, y, choices=c("1","2"))
#' dat <- tmp$dat[[1]]
#' ind <- tmp$cl[[1]]
#' 
#' 
#' ## dimension reduction by PCA
#' x   <- prcomp(dat,scale=FALSE)$x
#' 
#' ## perform and plot outlier detection using classical Mahalanobis distance
#' ## on the first 2 PCA dimensions
#' res <- outl.det(x[,c(1,2)], method="classical",dimen=c(1,2),
#'                     conf.level = 0.975)
#' 
#' 
`outl.det` <-
function(x, method = "classical", conf.level = 0.975, dimen=c(1,2), 
                        tol = 1e-7, plotting = TRUE){

  ## -------------------------------------------------------------
  ## Plot Mahalanobis distances against the index of data set.   
  plot.dist <- function(dis, cutoff){
    ylab <- "Distance Square Root"
    plot(dis, ylab=ylab, xlab="Index", type="p")
    ## label outliers
    x <- cbind(c(1:length(dis)),dis)
    outlier.label(x, dis, cutoff)    
    ## Use cutoff to draw a horisontal line.
    abline(h=cutoff, col="red")        
    title(main="Distance Plot")
  }

  ## -------------------------------------------------------------------
  ## Plot the tolerance ellipse of the bivariate data set. 
  plot.elli <- function(x, cov,center,dimen,conf.level=0.95, dis, cutoff){
    ## calculate ellipse values
    x <- x[,dimen]
    z <- elli(x=cov[dimen,dimen], center = center[dimen], conf.level=conf.level)
    ## or call ellipse in package 'ellipse'.
    ## z <- ellipse::ellipse(x=cov,centre=center, level=conf.level)
    
    x1 <- c(min(x[, 1], z[, 1]), max(x[,1],z[,1]))
    y1 <- c(min(x[, 2], z[, 2]), max(x[,2],z[,2]))
    
    plot(x[, 1], x[, 2], xlim = x1, ylim = y1, xlab = paste("Dimension",dimen[1]), 
        ylab = paste("Dimension",dimen[2]), type = "p")
    box()
    outlier.label(x,dis,cutoff)   ## label outliers
    points(z[, 1], z[, 2], type = "l", lty="solid", col="red")
    main = paste("Tolerance Ellipse (", 100 * conf.level, "%)",sep = "")
    title(main)
  }

  ## -------------------------------------------------------------------
  ## Calculates a ellipsoid with confidence level
  ## Modified from package 'rrcov' 
  elli <- function(x, center,conf.level=0.95) {
    dist    <- sqrt(qchisq(conf.level, 2))
    A       <- solve(x)
    lambda1 <- max(eigen(A)$values)
    lambda2 <- min(eigen(A)$values)
    eigvect <- eigen(A)$vectors[, order(eigen(A)$values)[2]]
    z       <- seq(0, 2 * pi, len=100)
    ## z       <- seq(0, 2 * pi, 0.01)
    z1      <- dist/sqrt(lambda1) * cos(z)
    z2      <- dist/sqrt(lambda2) * sin(z)
    alfa    <- atan(eigvect[2]/eigvect[1])
    r       <- matrix(c(cos(alfa),  - sin(alfa), sin(alfa), cos(alfa)), ncol = 2)
    z       <- t(t(cbind(z1, z2) %*% r) + center)

    return(z)
  }

  ## -------------------------------------------------------------------
  ## Label the outliers which robust distance is larger tan cutoff.
  outlier.label <- function(x, dis, cutoff){
    outlier.n <- length(which(dis > cutoff))          
    if(outlier.n > 0) {
      n    <- length(dis)
      ind  <- sort(dis, index.return=TRUE)$ix
      ind  <- ind[(n-outlier.n+1):n]
      
      xrange <- par("usr")
      xrange <- xrange[2] - xrange[1]
      ## text(x[ind,1] + xrange/50, x[ind,2], ind)

      ## -- display names -----
      txt <- names(dis[ind])
      if (is.null(txt)) txt <- ind
      ## text(x[ind,1] + xrange/50, x[ind,2], txt)
      text(x[ind,1] - xrange/20, x[ind,2], txt, col="blue")  ## wll-21-08-2007
    }
  }
  ## -------------------------------------------------------------

  method  <- match.arg(method, c("mve", "mcd", "classical"))
  x       <- as.matrix(x)
  rob     <- cov.rob(x, method=method)
  ## Mahalanobis distances
  dis     <- sqrt(mahalanobis(x, rob$center, rob$cov, tol=tol))
  cutoff  <- sqrt(qchisq(conf.level, ncol(x)))
  outlier <- which(dis > cutoff)

  ## plot robust mahalanobis distances
  if (plotting){
    par(mfrow=c(1,2), pty="m")
    plot.dist(dis, cutoff)                  
    plot.elli(x, rob$cov, rob$center,dimen=dimen,conf.level=conf.level, dis, cutoff)
  }

  if (!is.null(names(outlier)))
     outlier <- names(outlier)

  ret <- list(outlier=outlier, conf.level = conf.level,mah.dist=dis,cutoff=cutoff)
  return(ret)
}


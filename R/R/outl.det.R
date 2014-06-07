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


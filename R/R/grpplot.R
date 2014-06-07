`grpplot` <-
function(dat, group,col=NULL,pch=NULL,cex=1,legend.loc="rightside",...)
{
  ## validity checking should go here.
  if (is.vector(dat)|| ncol(dat)==1) {
    dat <- as.matrix(dat)
  } else {  
    dat <- as.data.frame(dat)
  }

  legend.loc <- match.arg(legend.loc, c("bottomright", "bottom", "bottomleft",
                                        "left", "topleft", "top", "topright",
                                        "right", "center","mousepoint", "rightside"))
  
  group <- factor(group)
  n     <- nlevels(group)
  
  col <- if (is.null(col)) unclass(group) else col[group]
  pch <- if (is.null(pch)) unclass(group) else pch[group]
  
  if (ncol(dat) <= 2) {
    if (legend.loc == "rightside") {
      oldpar <- par(mar = par()$mar+c(0,0,0,4),xpd=TRUE)
      plot(dat, col=col,pch=pch,cex=cex,...)
      legend(par("usr")[2] + 1.5,par("usr")[4],legend=sort(unique(levels(group))),cex=cex, 
             col = sort(as.vector(unique(col))), pch = sort(as.vector(unique(pch))) )
      par(oldpar)
    } else if (legend.loc == "mousepoint"){     ## use mouse point to locate legend. 
      plot(dat, col=col,pch=pch,cex=cex,...)
      legend(locator(1), legend=sort(unique(levels(group))), cex=cex,
             col = sort(as.vector(unique(col))), pch = sort(as.vector(unique(pch))) )
    } else {
      plot(dat, col=col,pch=pch,cex=cex,...)
      legend(legend.loc, inset=0.02,legend=sort(unique(levels(group))),cex=cex, 
             col = sort(as.vector(unique(col))), pch = sort(as.vector(unique(pch))) )
    }
  } else {
    plot(dat, col=col,pch=pch,cex=cex,...)
  }
}


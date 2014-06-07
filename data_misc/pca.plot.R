pca.plot <- function(x, y, scale=FALSE, abbrev = FALSE, ...)
{
  pca    <- prcomp(x, scale.= scale)
  vars   <- pca$sdev^2
  vars.p <- round((vars/sum(vars)) * 100, 2)
  names(vars.p) <- colnames(pca$rotation)

  if(abbrev) levels(y) <- abbreviate(levels(y), abbrev)
  xval   <- pca$x[,1:2]

  xlab <- paste("PC1", " (", vars.p[1],"%)", sep = "")
  ylab <- paste("PC2", " (", vars.p[2],"%)", sep = "")
  plot(xval,type="n", cex=1.0,cex.lab=1.0,cex.axis=1.0,cex.main=1.0,
       ylab=paste("PC2", " (", vars.p[2],"%)", sep = ""),
       xlab=paste("PC1", " (", vars.p[1],"%)", sep = ""),...)
  text(xval[, 1], xval[, 2], as.character(y), cex=0.7, col=unclass(y),...)
  invisible(NULL)
}
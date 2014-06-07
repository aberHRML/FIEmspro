aam.plot <- function(aam, cls, DF,...){
  z.1 <- lapply(aam, function(y){
    z    <- lapply(y, my.melt)
    z    <- do.call(rbind,z)
    dfn  <- sub("(^.*?)\\.\\d+$", "\\1", rownames(z), perl=TRUE)
    z    <- cbind(z,dfn)
    rownames(z) <- 1: nrow(z)
    z
  })
  z.1    <- do.call(rbind,z.1)
  dfn.1  <- sub("(^.*?)\\.\\d+$", "\\1", rownames(z.1), perl=TRUE)
  z.1    <- cbind(z.1,dfn.1)
  rownames(z.1) <- 1: nrow(z.1)
  names(z.1)    <- c("value", "classifier", "assessment", "pairwise","data")
  z.1$data      <- paste(z.1$data, z.1$pairwise, sep="~")

  aam.p <-
    dotplot(factor(data, levels=rev(unique.default(data))) ~
            value | assessment,
            data=z.1, groups = classifier, as.table=T, layout = c(3,1),
            par.settings = list(superpose.line = list(lty=c(1:7)),
                                superpose.symbol=list(pch=rep(1:25))),
            type="o",
            auto.key = list(lines=TRUE,space="bottom",columns=length(cl.method)),
            xlab="", main=DF)

}
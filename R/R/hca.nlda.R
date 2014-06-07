## wll-29-08-2007: deal with Character strings for title
`hca.nlda` <-  function(x, method="complete",df2use=NULL, 
                        main="Aggregation of group centres",
                        ylab="Mahalanobis distance",xlab="",sub="",...)
{
  if(length(x$prior)<3){stop('Needs more than 2 classes')}
  if(is.null(df2use)){df2use=1:(length(x$prior)-1)}
  mdist <- dist(x$xmeans[,df2use])
  hc    <- hclust(mdist,method)
  plot(hc,main=main,ylab=ylab,xlab=xlab,sub=sub,...)
}


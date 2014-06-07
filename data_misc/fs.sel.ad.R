fs.sel.ad <- function(x,y,method,thr,...){
  ordering <- if (method == "fs.auc.ad") "greater" else "less"  
  
  method <- if (is.function(method)) method
            else if (is.character(method)) get(method)
            else eval(method)

  fs.res <- as.data.frame(method(x,y,...))
  fs.val <- fs.res[,grep('ad',names(fs.res)),drop=F]
  fs.num <- fs.n(fs.val,thr,ordering,...)     
  fs <- as.character(fs.res$fs.order)
  fs.order <- lapply(1:nrow(fs.num), function(i){
    len    <- fs.num[i,2]
    fs.tmp <- if (len==0) NA else fs[1:len]
  })
  names(fs.order) <- paste("thr",thr, sep="_")
  len      <- max(sapply(fs.order,length))
  fs.order <- sapply(fs.order, function(x) c(x,rep(NA,len - length(x))))
  if (is.vector(fs.order)) fs.order <- t(fs.order)
  
  res <- list(fs.num=fs.num, fs.order=fs.order, fs=fs.res)
}
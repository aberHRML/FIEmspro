my.save.tab <- function(x, filename="temp.csv", firstline="\n"){
  options(warn=-1)
  write(firstline, file=filename)
  if (is.list(x)) {
    for(i in names(x)){
      write(paste('\n', i, sep=""), file=filename, sep=',', append=T)
      write.table(x[[i]], file=filename, append=T, sep=",", na = "",
                  quote=F,row.names=T,col.names=NA)
    }
  } else {
    write(paste('\n', sep=""), file=filename, sep=',', append=T)
    write.table(x, file=filename, append=T, sep=",", na = "",
                quote=F,row.names=T,col.names=NA)
  }
  options(warn=0)
  invisible()
}
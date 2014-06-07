## ==============================================
trainind.cv <- function(cv)
{
  tmp <- NULL
  k = 1
  for(i in unique(cv)){
    tmp[[k]]=which(cv!=i)
    k = k+1
  }
  list(Iter_1=tmp)
}

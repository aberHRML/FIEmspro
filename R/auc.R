auc <- function(stat,label)
{
  if (missing(label) || missing(stat)) stop("arguments miss")
  if(length(label) != length(stat)) stop("lengths differ")
  if (is.factor(label))
    label <- as.numeric(levels(label))[as.integer(label)]
  if(!all(sort(unique(label)) == c(0, 1)))
      stop("'label' must take values 0 or 1")

  label <- label[order(stat, decreasing=T)]
  tmp <- cumsum(label)/sum(label)
	auc <- mean(tmp[label==0])
  return(auc)
}


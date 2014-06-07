p.diff <- function(x){
  n <- length(x)
  if (is.null(names(x))) names(x) <- 1:n
  
  prs <- cbind(rep(1:n, each = n), 1:n)
  com <- prs[prs[, 1] < prs[, 2], , drop = FALSE]
  res <- x[com[, 2]] - x[com[, 1]]
  names(res) <- paste(names(x)[com[, 2]], names(x)[com[, 1]], sep = "~")
  res
}
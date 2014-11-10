nlda.formula <-
  function (formula, data = NULL, ..., subset, na.action = na.omit)
{
  call <- match.call()
  if (!inherits(formula, "formula"))
    stop("method is only for formula objects")
  m <- match.call(expand.dots = FALSE)
  if (identical(class(eval.parent(m$data)), "matrix"))
    m$data <- as.data.frame(eval.parent(m$data))
  m$... <- NULL
  m[[1]] <- as.name("model.frame")
  m$na.action <- na.action
  m <- eval(m, parent.frame())
  Terms <- attr(m, "terms")
  attr(Terms, "intercept") <- 0
  x <- model.matrix(Terms, m)
  y <- model.extract(m, "response")
  attr(x, "na.action") <- attr(y, "na.action") <- attr(m, "na.action")

  ret <- nlda.default (x, y, ..., na.action = na.action)

  ret$call <- call
  ret$call[[1]] <- as.name("nlda")
  ret$terms <- Terms
  if (!is.null(attr(m, "na.action")))
    ret$na.action <- attr(m, "na.action")
  class(ret) <- c("nlda.formula", class(ret))
  return (ret)
}

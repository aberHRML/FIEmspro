roc <- function (stat, label, plotting=TRUE,...)
{
  rule <- function (x, thresh) ifelse(x > thresh, 1, 0)

  if (missing(label) || missing(stat)) stop("arguments miss")
  if(length(label) != length(stat)) stop("lengths differ")
  if (is.factor(label))
    label <- as.numeric(levels(label))[as.integer(label)]
  if(!all(sort(unique(label)) == c(0, 1)))
      stop("'label' must take values 0 or 1")

  ## cuts points
  udata <- unique(sort(stat))
  delta <- min(diff(udata))/2
  cutpts <- c(udata - delta, udata[length(udata)] + delta)

  np <- length(cutpts)
  sens <- rep(NA, np)
  spec <- rep(NA, np)
  for (i in 1:np) {
    pred <- rule(stat, cutpts[i])
    sens[i] = mean(pred[label == 1])
    spec[i] = mean(1 - pred[label == 0])
  }

  auc <- auc(stat,label)

  if (plotting) {           ## plot ROC curve
    X11()
    fpr  <- 1 - spec        ## false positive rate (fpr)
    tpr  <- sens            ## true positive rate  (tpr)
    main <- "ROC Curve"
    xlab <- "False positive Rate"
    ylab <- "True positive Rate"
    plot(fpr, tpr, type = 'n', xlim = c(0,1), ylim = c(0,1),
         main = main,  xlab = xlab, ylab = ylab,...)
    points(fpr, tpr, col = "red", type="b",pch = 19)
    abline(h=seq(0,1,by=.1),v=seq(0,1,by=.1),lty=3, lwd = 0.5, col = "grey")
    abline(0,1)
  }

  ## ret <- list(spec = spec, sens = sens, cuts = cutpts, auc=auc)
  ret <- list(tpr = tpr, fpr = fpr, auc=auc)
  return(ret)
}

panel.elli <- function(x, y, ep=0,conf.level = 0.975,...) {
  plot.elli <- function(x,y,...){ 
    Var  <- var(cbind(x,y))
    Mean <- cbind(mean(x),mean(y))
    Elli <- ellipse(Var, centre = Mean, level = conf.level)
    panel.points(Elli[,1], Elli[,2],...)
  }

  panel.superpose(x,y,...)
  panel.abline(h=0, v=0,col=c("gray"), lty=2)
  if (ep == 1){
    plot.elli(x,y,type="l",col="red",lwd=2,...)
  } else if (ep == 2){
    panel.superpose(x,y,..., panel.groups = plot.elli, type="l", lty=2)
  }
}
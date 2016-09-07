hist.grouped <- function(x
                         ,interval.size = NA
                         ,width.consider = lolcat.default.width.consider
                         ,anchor.value = NA
                         ,col="lightblue"
                         ,main="Grouped Histogram"
                         ,right=F
                         ,include.lowest = T
                         ,...) {
  
  argext <- list(...)
  
  dist.grouped <- frequency.dist.grouped(x
                                         ,interval.size = interval.size
                                         ,width.consider = width.consider
                                         ,anchor.value = anchor.value
                                         ,na.rm = T)
  
  breaks.tentative <- dist.grouped$min
  resolution <- dist.grouped$min[2] - dist.grouped$min[1]
  
  breaks.tentative <- seq(min(breaks.tentative)-resolution, max(breaks.tentative)+resolution,resolution)
  
  labels.tentative <- dist.grouped$midpoint
  labels.tentative <-c(labels.tentative[1] - resolution, labels.tentative)
  labels.tentative <-c(labels.tentative, labels.tentative[length(labels.tentative)] + resolution)
  
  ret <- hist(x, col=col, breaks= breaks.tentative, main=main, xaxt="n", right=right, include.lowest = include.lowest, ...)
  
  if (length(argext[["xaxt"]]) && "n" == argext[["xaxt"]]) {
    
  } else {
    axis(1,at=breaks.tentative+.5*resolution, labels = labels.tentative)
  }
  
  invisible(ret)
}
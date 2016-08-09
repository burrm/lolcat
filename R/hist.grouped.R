hist.grouped <- function(x
                         ,interval.size = NA
                         ,width.consider = lolcat.default.width.consider 
                         ,col="lightblue"
                         ,main="Grouped Histogram"
                         ,...) {
  
  argext <- list(...)
  
  dist.grouped <- frequency.dist.grouped(x
                                         ,interval.size = interval.size
                                         ,width.consider = width.consider
                                         ,na.rm = T)
  
  breaks.tentative <- dist.grouped$min
  resolution <- dist.grouped$min[2] - dist.grouped$min[1]
  
  breaks.tentative <- seq(min(breaks.tentative)-resolution, max(breaks.tentative)+resolution,resolution)
  
  labels.tentative <- dist.grouped$midpoint
  labels.tentative <-c(labels.tentative[1] - resolution, labels.tentative)
  labels.tentative <-c(labels.tentative, labels.tentative[length(labels.tentative)] + resolution)
  
  ret <- hist(x, col=col, breaks= breaks.tentative, main=main, xaxt="n", ...)
  
  if (length(argext[["xaxt"]]) && "n" == argext[["xaxt"]]) {
    
  } else {
    axis(1,at=breaks.tentative-.5*resolution, labels = labels.tentative)
  }
  
  invisible(ret)
}
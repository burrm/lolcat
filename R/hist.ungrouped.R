hist.ungrouped <- function(x
                           ,col="lightblue"
                           ,main="Ungrouped Histogram"
                           ,...) {
  
  argext <- list(...)
  
  dist.ungrouped <- frequency.dist.ungrouped(x, na.rm = T)
  
  breaks.tentative <- as.numeric(levels(dist.ungrouped$value))
  resolution <- max(min(diff(breaks.tentative)),1e-7)
  
  breaks.tentative <- seq(min(breaks.tentative)-resolution, max(breaks.tentative)+resolution,resolution)
    
  ret <- hist(x, col=col, breaks= breaks.tentative, main=main, xaxt="n", ...)
  
  if ("n" == argext[["xaxt"]]) {
    
  } else {
  axis(1,at=breaks.tentative-.5*resolution, labels = breaks.tentative)
  }
  
  invisible(ret)
}
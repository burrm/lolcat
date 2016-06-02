hist.ungrouped <- function(x
                           ,col="lightblue"
                           ,main="Ungrouped Histogram"
                           ,...) {
  dist.ungrouped <- frequency.dist.ungrouped(x, na.rm = T)

  breaks.tentative <- as.numeric(levels(dist.ungrouped$value))
  resolution <- min(diff(breaks.tentative))
  
  breaks.tentative <- seq(min(breaks.tentative)-resolution, max(breaks.tentative),resolution)
    
  hist(x, col=col, breaks= breaks.tentative, main=main, xaxt="n", ...)
  axis(1,at=breaks.tentative-.5*resolution, labels = breaks.tentative)
}
frequency.polygon.ungrouped <- function (
  x
  ,col="blue"
  ,main="Ungrouped Frequency Polygon"
  ,ylab = "Frequency"
  ,...
){
  
  dist.grouped <- frequency.dist.grouped(x
                                         ,interval.size = interval.size
                                         ,width.consider = width.consider
                                         ,na.rm = T)
  
  resolution <- dist.grouped$min[2] - dist.grouped$min[1]
  
  x <- dist.grouped$midpoint
  x <- c(min(x)- resolution, x, max(x) + resolution)
  y <- c(0,dist.grouped$freq,0)
  
  plot(x,y, type="l", main = main, ylab = ylab, col = col, xaxt = "n", ...)
  axis(1, at=x, labels = x)
}

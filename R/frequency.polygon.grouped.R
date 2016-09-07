frequency.polygon.grouped <- function (
  x
  ,interval.size = NA
  ,width.consider = lolcat.default.width.consider
  ,anchor.value = NA
  ,col="blue"
  ,main="Grouped Frequency Polygon"
  ,ylab = "Frequency"
  ,right = F
  ,call.plot = T
  ,...
){

  dist.grouped <- frequency.dist.grouped(x
                                         ,interval.size = interval.size
                                         ,width.consider = width.consider
                                         ,right = right
                                         ,anchor.value = anchor.value
                                         ,na.rm = T)
  
  resolution <- dist.grouped$min[2] - dist.grouped$min[1]
  
  x <- dist.grouped$midpoint
  x <- c(min(x)- resolution, x, max(x) + resolution)
  y <- c(0,dist.grouped$freq,0)
  
  if (call.plot) {
    plot(x,y, type="l", main = main, ylab = ylab, col = col, xaxt = "n", ...)
    axis(1, at=x, labels = x)
  }
  
  invisible(list(x=x, y=y))
}
  
frequency.polygon.ungrouped <- function (
  x
  ,col="blue"
  ,main="Ungrouped Frequency Polygon"
  ,ylab = "Frequency"
  ,...
){
  
  dist.ungrouped <- frequency.dist.ungrouped(x
                                         #,interval.size = interval.size
                                         #,width.consider = width.consider
                                         ,na.rm = T)
  
  resolution <- min(diff(dist.ungrouped$value))
  
  x <- seq(min(dist.ungrouped$value)-resolution, max(dist.ungrouped$value) + resolution, resolution)
  y <- sapply(x, FUN = function(x) {
    idx <- which(dist.ungrouped$value == x)
    if (length(idx) > 0) {
      dist.ungrouped$freq[idx[1]]
    } else {
      0
    }
  })
  
  
  plot(x,y, type="l", main = main, ylab = ylab, col = col, xaxt = "n", ...)
  axis(1, at=x, labels = x)
}

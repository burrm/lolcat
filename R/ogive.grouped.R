ogive.grouped <- function(
  x
  ,interval.size = NA
  ,width.consider = lolcat.default.width.consider 
  ,anchor.value = NA
  ,col="blue"
  ,main="Grouped Ogive"
  ,ylab = "Cumulative Relative Frequency"
  ,xlab = "Midpoint"
  ,right = F
  ,call.plot = T
  ,...
) {
  dist.grouped <- frequency.dist.grouped(x
                                         ,interval.size = interval.size
                                         ,width.consider = width.consider
                                         ,anchor.value = anchor.value
                                         ,right = right
                                         ,na.rm = T)
  
  if (call.plot) {
    plot(dist.grouped$midpoint,dist.grouped$cum.up, type="b", main = main, ylab = ylab, xlab = xlab, col = col, xaxt = "n", ...)
    axis(1, at=dist.grouped$midpoint, labels = dist.grouped$midpoint)
  }
  
  invisible(list(x = dist.grouped$midpoint, y = dist.grouped$cum.up))
}
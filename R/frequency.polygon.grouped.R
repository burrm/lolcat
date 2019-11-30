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
  ,stat.lsl               = NA
  ,stat.lsl.label         = "LSL"
  ,stat.target            = NA
  ,stat.target.label      = "TGT"
  ,stat.usl               = NA
  ,stat.usl.label         = "USL"
  ,after.plot             = function (x, ...) {}
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
  
    if (!is.na(stat.lsl)) {
      hist.add.spec.line.simple(at = stat.lsl, label = stat.lsl.label)
    }

    if (!is.na(stat.target)) {
      hist.add.spec.line.simple(at = stat.target, label = stat.target.label)
    }

    if (!is.na(stat.usl)) {
      hist.add.spec.line.simple(at = stat.usl, label = stat.usl.label)
    }

    after.plot(x)
  }
  
  invisible(list(x=x, y=y))
}
  
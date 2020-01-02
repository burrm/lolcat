frequency.polygon.ungrouped <- function (
  x
  ,col="blue"
  ,main="Ungrouped Frequency Polygon"
  ,ylab = "Frequency"
  ,stat.lsl               = NA
  ,stat.lsl.label         = "LSL"
  ,stat.target            = NA
  ,stat.target.label      = "TGT"
  ,stat.usl               = NA
  ,stat.usl.label         = "USL"
  ,after.plot             = function (x, ...) {}
  ,...
){
  
  dist.ungrouped <- frequency.dist.ungrouped(x
                                         #,interval.size = interval.size
                                         #,width.consider = width.consider
                                         ,na.rm = T)
  
  resolution <- min(diff(unique(sort(dist.ungrouped$value))))
  
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

      if (!is.na(stat.lsl)) {
      hist.add.spec.line.simple(at = stat.lsl, label = stat.lsl.label)
    }

    if (!is.na(stat.target)) {
      hist.add.spec.line.simple(at = stat.target, label = stat.target.label)
    }

    if (!is.na(stat.usl)) {
      hist.add.spec.line.simple(at = stat.usl, label = stat.usl.label)
    }

    after.plot(x, freq = freq, resolution = resolution)
}

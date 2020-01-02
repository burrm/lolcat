hist.grouped <- function(
  x
  ,interval.size = NA
  ,width.consider = lolcat.default.width.consider
  ,anchor.value = NA
  ,col="lightblue"
  ,main="Grouped Histogram"
  ,right=F
  ,include.lowest = T
  ,stat.lsl               = NA
  ,stat.lsl.label         = "LSL"
  ,stat.target            = NA
  ,stat.target.label      = "TGT"
  ,stat.usl               = NA
  ,stat.usl.label         = "USL"
  ,after.plot             = function (x, ...) {}
  ,freq = T
  ,...
) {
  
  argext <- list(...)
  
  dist.grouped <- frequency.dist.grouped(x
                                         ,interval.size = interval.size
                                         ,width.consider = width.consider
                                         ,anchor.value = anchor.value
                                         ,right = right
                                         ,na.rm = T)
  
  breaks.tentative <- dist.grouped$max
  resolution <- if (nrow(dist.grouped) > 1) {
    dist.grouped$min[2] - dist.grouped$min[1]
  } else if (!is.na(interval.size)) {
    interval.size
  } else {
    1
  }
  
  breaks.tentative <- seq(min(breaks.tentative)-resolution, max(breaks.tentative)+resolution,resolution)
  
  labels.tentative <- dist.grouped$midpoint
  labels.tentative <-c(labels.tentative[1] - resolution, labels.tentative)
  labels.tentative <-c(labels.tentative, labels.tentative[length(labels.tentative)] + resolution)
  
  ret <- hist(x, col=col, breaks= breaks.tentative, main=main, xaxt="n", right=right, include.lowest = include.lowest, freq = freq, ...)
  
  if (length(argext[["xaxt"]]) && "n" == argext[["xaxt"]]) {
    
  } else {
    at <-#if (right) {
            breaks.tentative-.5*resolution
          #} else {
          #  breaks.tentative+.5*resolution
          #}

    axis(1
         ,at=at
         , labels = labels.tentative)
  }
  
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

  invisible(ret)
}
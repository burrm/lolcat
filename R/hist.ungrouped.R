hist.ungrouped <- function(
  x
  ,col="lightblue"
  ,main="Ungrouped Histogram"
  ,stat.lsl               = NA
  ,stat.lsl.label         = "LSL"
  ,stat.target            = NA
  ,stat.target.label      = "TGT"
  ,stat.usl               = NA
  ,stat.usl.label         = "USL"
  ,freq                   = T
  ,after.plot             = function (x, ...) {}
  ,...
) {
  
  argext <- list(...)
  
  dist.ungrouped <- frequency.dist.ungrouped(x, na.rm = T)
  
  breaks.tentative <- dist.ungrouped$value
  resolution <- max(min(diff(unique(sort(breaks.tentative)))),1e-7)
  
  breaks.tentative <- seq(min(breaks.tentative)-resolution, max(breaks.tentative)+resolution,resolution)
    
  ret <- hist(x, col=col, breaks= breaks.tentative, main=main, xaxt="n", freq = freq, ...)
  
  if (length(argext[["xaxt"]]) && "n" == argext[["xaxt"]]) {
    
  } else {
  axis(1,at=breaks.tentative-.5*resolution, labels = breaks.tentative)
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
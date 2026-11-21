#' Grouped Histogram 
#' 
#' Prepares a grouped histogram of a particular set of data. 
#'
#' @param x Vector/numeric - The data for the plot
#' @param interval.size Numeric - Bin size
#' @param width.consider Numeric - Bin sizes to consider when plotting histograms
#' @param anchor.value Numeric - One of the breakpoints
#' @param col Character - color of bars
#' @param main Character - Main chart title
#' @param right Logical - If TRUE, the histogram cells are right-closed (left open) intervals.
#' @param include.lowest Logical - If TRUE, an x[i] equal to the breaks value will be included in the first (or last, for right = FALSE) bar. This will be ignored (with a warning) unless breaks is a vector.
#' @param stat.lsl Numeric - Lower specification limit
#' @param stat.lsl.label Character - Lower specification limit label
#' @param stat.target Numeric - Process target
#' @param stat.target.label Character - Process target label
#' @param stat.usl Numeric - Upper specification limit
#' @param stat.usl.label Character - Upper specification limit label
#' @param after.plot Function - Function to perform after plotting, see hist.add.distribution.curve.normal
#' @param freq Logical - If TRUE, use frequency instead of probability densities.
#' @param xlim Numeric - Chart Domain, expressed as c(x.min,x.max)
#' @param ... Additional parameters - Additional parameters passed to hist() and after.plot()
#'
#' @return Return value of hist() 
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
  ,xlim = c(min(na.omit(c(x,stat.lsl,anchor.value))), max(na.omit(c(x,stat.usl,anchor.value))))
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
  
  breaks.tentative <- #seq(min(breaks.tentative)-resolution, max(breaks.tentative)+resolution,resolution)
                      c(dist.grouped$max[1]-resolution, dist.grouped$max, dist.grouped$max[length(dist.grouped$max)] + resolution)
    
  labels.tentative <- dist.grouped$midpoint
  labels.tentative <- c(labels.tentative[1] - resolution, labels.tentative)
  labels.tentative <- c(labels.tentative, labels.tentative[length(labels.tentative)] + resolution)
  
  
  #if (!is.na(stat.lsl)) {
    target.min <- xlim[1]
    current.min.bin <- min(breaks.tentative) 
    current.label   <- min(labels.tentative)
    
    while (target.min <= current.min.bin) {
      current.min.bin <- current.min.bin - resolution
      current.label <- current.label - resolution
      
      breaks.tentative <- c(current.min.bin, breaks.tentative)
      labels.tentative <- c(current.label, labels.tentative)
    }
  #}
  
  #if (!is.na(stat.usl)) {
    target.max <- xlim[2]
    current.max.bin <- max(breaks.tentative) 
    current.label   <- max(labels.tentative)
    
    while (target.max >= current.max.bin) {
      current.max.bin <- current.max.bin + resolution
      current.label   <- current.label + resolution
      
      breaks.tentative <- c(breaks.tentative, current.max.bin)
      labels.tentative <- c(labels.tentative, current.label)
    }
  #}
  
  
  ret <- hist(x, 
              col=col, 
              breaks= breaks.tentative, 
              main=main, xaxt="n", 
              right=right, 
              include.lowest = include.lowest, 
              freq = freq, 
              ...)
  
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
spc.controlviolation.run.above <- function(x, run.length=8, center.line = 0, ...) {
  warning("Deprecated - will be removed in future release.")
  ret <- logical(0)
  
  observed.run.lengths <- rle(x > center.line)
  
  for (i in 1:length(observed.run.lengths$lengths)) {
    is.run.violation <- observed.run.lengths$lengths[i] >= run.length & observed.run.lengths$values[i]
    ret <- c(ret, rep(is.run.violation, observed.run.lengths$lengths[i]))
  }
  
  ret  
}


spc.controlviolation.run.below <- function(x, run.length=8, center.line = 0, ...) {
  warning("Deprecated - will be removed in future release.")
  ret <- logical(0)
  
  observed.run.lengths <- rle(x < center.line)
  
  for (i in 1:length(observed.run.lengths$lengths)) {
    is.run.violation <- observed.run.lengths$lengths[i] >= run.length & observed.run.lengths$values[i]
    ret <- c(ret, rep(is.run.violation, observed.run.lengths$lengths[i]))
  }
  
  ret  
}


spc.controlviolation.runs <- function(x, run.length=8, center.line = 0, ...) {
  warning("Deprecated - will be removed in future release.")
  ret.1 <- spc.controlviolation.run.above(x = x, run.length = run.length, center.line = center.line)
  ret.2 <- spc.controlviolation.run.below(x = x, run.length = run.length, center.line = center.line)
  
  ret.1 | ret.2
}



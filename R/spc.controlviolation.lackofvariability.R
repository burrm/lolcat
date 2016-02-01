spc.controlviolation.lackofvariability <- function(x
                                                   ,lack.variation.count = 15
                                                   ,center.line
                                                   ,standard.error
                                                   ,lower.bound = center.line - standard.error
                                                   ,upper.bound = center.line + standard.error,
                                                   ...) {
  
  
  ret <- logical(0)
  
  observed.run.lengths <- rle(x >= lower.bound & x <= upper.bound)
  
  for (i in 1:length(observed.run.lengths$lengths)) {
    is.run.violation <- observed.run.lengths$lengths[i] >= lack.variation.count & observed.run.lengths$values[i]
    ret <- c(ret, rep(is.run.violation, observed.run.lengths$lengths[i]))
  }
  
  ret
  
}
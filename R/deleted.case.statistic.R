#' Deleted Case Statistic Calculation  
#' 
#' Calculates a summary statistic for each value of an input vector by calculating 
#' the value of the statistic when a particular case is deleted. 
#'
#' @param x Vector to compute deleted case statistics values for.
#' @param FUN Function used to compute summary statistic.
#'
#' @return Vector with same length as x. Each value i is the result of FUN(x) where x[i] is removed.
deleted.case.statistic <- function(x, FUN = mean) {
  ret <- x
  
  for (i in 1:length(x)) {
    this_x <- x[-i]
    ret[i] <- FUN(this_x)
  }
  
  ret
}
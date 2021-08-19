#' Control Chart Constant A3  
#' 
#' Calculate A3 control chart constant from Wheeler's SPC books.
#'
#' @param sample.size A scalar or vector of sample sizes
#' @param n.sigma How many standard errors to use (default 3 for most control charts)
#'
#' @return A scalar or vector with computed constants. 
spc.constant.calculation.A3 <- function(
  sample.size
  ,n.sigma = 3
) {
  sapply(sample.size, FUN = function(i) {
      n.sigma/(spc.constant.calculation.c4(i) * sqrt(i))
  })
}
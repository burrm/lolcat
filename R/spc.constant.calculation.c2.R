#' Control Chart Constant c2  
#' 
#' Calculate c2 control chart constant from Wheeler's SPC books.
#'
#' @param sample.size A scalar or vector of sample sizes
#'
#' @return A scalar or vector with computed constants. 
spc.constant.calculation.c2 <- function(
  sample.size
) {
  sapply(sample.size, FUN = function(i) {
    sqrt(2/i)*gamma(i/2)/gamma((i-1)/2)
  })
}
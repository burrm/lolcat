#' Control Chart Constant B4  
#' 
#' Calculate B4 control chart constant from Wheeler's SPC books.
#'
#' @param sample.size A scalar or vector of sample sizes
#' @param n.sigma How many standard errors to use (default 3 for most control charts)
#' @param exact Logical - Use exact calculation or approximate calculation (if FALSE)?
#'
#' @return A scalar or vector with computed constants. 
spc.constant.calculation.B4 <- function(
  sample.size, 
  n.sigma = 3, 
  exact = T
) {
  sapply(sample.size, FUN = function(i) {
    ret <- if (exact) {
      c2 <- spc.constant.calculation.c2(i)
      1 + (n.sigma / c2) * sqrt((i -1) / i - c2^2 ) 
    } else {
      1 + n.sigma / sqrt(2 * (i-1))
    }
    
    ret
  })
}
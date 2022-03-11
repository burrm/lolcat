#' Control Chart Constant B3  
#' 
#' Calculate B3 control chart constant from Wheeler's SPC books.
#'
#' @param sample.size A scalar or vector of sample sizes
#' @param n.sigma How many standard errors to use (default 3 for most control charts)
#' @param negative.as.NA Logical - return NA in the case a negative value is calculated
#' @param exact Logical - Use exact calculation or approximate calculation (if FALSE)?
#'
#' @return A scalar or vector with computed constants. 
spc.constant.calculation.B3 <- function(
  sample.size, 
  n.sigma = 3, 
  negative.as.NA = T, 
  exact = T
) {
  sapply(sample.size, FUN = function(i) {
    ret <- if (exact) {
      c2 <- spc.constant.calculation.c2(i)
      1 - (n.sigma / c2) * sqrt((i -1) / i - c2^2 ) 
    } else {
      1- n.sigma / sqrt(2 * (i-1))
    }

    if (ret < 0 & negative.as.NA) {
      ret <- NA
    }
    
    ret
  })

}
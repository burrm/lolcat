#' Control Chart Constant D3  
#' 
#' Calculate D3 control chart constant from Wheeler's SPC books.
#'
#' @param sample.size A scalar or vector of sample sizes
#' @param n.sigma Scalar - How many standard errors to use (default 3 for most control charts)
#' @param negative.as.NA Logical - negative values are possible from the calculation, but not useful. Set to false to obtain negative values instead of NA.
#'
#' @return A scalar or vector with computed constants. 
spc.constant.calculation.D3 <- function(
  sample.size, 
  n.sigma = 3, 
  negative.as.NA = T
) {
  sapply(sample.size, FUN = function(i) {
      ret <- 1 - n.sigma * spc.constant.calculation.d3(i) / spc.constant.calculation.d2(i)

      if (ret < 0 & negative.as.NA) {
        ret <- NA
      }
        
      ret
  })
  

}

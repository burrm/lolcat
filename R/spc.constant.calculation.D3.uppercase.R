#' Control Chart Constant d3 / D3  
#' 
#' Calculate d3 or D3 control chart constant from Wheeler's SPC books. These are in the same help file 
#' because of a quirk of how R generates help file names, but they are different constants with different uses.
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

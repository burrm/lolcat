#' Control Chart Constant D4  
#' 
#' Calculate D4 control chart constant from Wheeler's SPC books.
#'
#' @param sample.size A scalar or vector of sample sizes
#' @param n.sigma How many standard errors to use (default 3 for most control charts)
#'
#' @return A scalar or vector with computed constants. 

spc.constant.calculation.D4 <- function(
  sample.size
  ,n.sigma = 3
) {
  sapply(sample.size, FUN = function(i) {
    ret <- (1 + 
            n.sigma * 
            spc.constant.calculation.d3(i) / spc.constant.calculation.d2(i))
  
    ret

  })
}
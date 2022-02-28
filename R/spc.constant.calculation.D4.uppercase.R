#' Control Chart Constant d4 / D4  
#' 
#' Calculate D4 control chart constant from Wheeler's SPC books. These are in the same help file 
#' because of a quirk of how R generates help file names, but they are different constants with different uses.
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
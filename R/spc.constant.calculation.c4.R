#' Control Chart Constant c4  
#' 
#' Calculate c4 control chart constant from Wheeler's SPC books.
#'
#' @param sample.size A scalar or vector of sample sizes
#'
#' @return A scalar or vector with computed constants. 
spc.constant.calculation.c4 <- function(
  sample.size
) {
  sapply(sample.size, FUN = function(i) {
      spc.constant.calculation.c2(i) * sqrt(i/(i-1))
  })

}
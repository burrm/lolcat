#' Control Chart Constant D6  
#' 
#' Calculate D6 control chart constant from Wheeler's SPC books.
#'
#' @param sample.size A scalar or vector of sample sizes
#' @param n.sigma Scalar - How many standard errors to use (default 3 for most control charts)
#'
#' @return A scalar or vector with computed constants. 
spc.constant.calculation.D6 <- function(
  sample.size
  ,n.sigma = 3
) {
  sapply(sample.size, FUN = function(i) {
    ret <- NA
    if (i < 11 & n.sigma == 3) {
      ret <-  if (i == 2) {
        3.865
      }else if (i == 3) {
        2.745
      }else if (i == 4) {
        2.375
      }else if (i == 5) {
        2.179
      }else if (i == 6) {
        2.055
      }else if (i == 7) {
        1.967
      }else if (i == 8) {
        1.901
      }else if (i == 9) {
        1.850
      }else if (i == 10) {
        1.809
      } 
      
    } else { 
      ret <- 1 + n.sigma * (3.07/3) * spc.constant.calculation.d3(i) / spc.constant.calculation.d4(i)
    }
    
    ret
  })


}

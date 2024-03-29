#' Control Chart Constant D5  
#' 
#' Calculate D5 control chart constant from Wheeler's SPC books.
#'
#' @param sample.size A scalar or vector of sample sizes
#' @param n.sigma Scalar - How many standard errors to use (default 3 for most control charts)
#' @param negative.as.NA Logical - negative values are possible from the calculation, but not useful. Set to false to obtain negative values instead of NA.
#'
#' @return A scalar or vector with computed constants. 
spc.constant.calculation.D5 <- function(
  sample.size
  ,n.sigma = 3
  ,negative.as.NA = T
) {
  sapply(sample.size, FUN = function(i) {
      ret <- NA
  if (i < 11 & n.sigma == 3) {
    ret <-  if (i == 2) {
      NA
    }else if (i == 3) {
      NA
    }else if (i == 4) {
      NA
    }else if (i == 5) {
      NA
    }else if (i == 6) {
      NA
    }else if (i == 7) {
      0.078
    }else if (i == 8) {
      0.139
    }else if (i == 9) {
      0.187
    }else if (i == 10) {
      0.227
    } 
    
  } else {
  
  ret <- 1 - n.sigma * (3.07/3) * spc.constant.calculation.d3(i) / spc.constant.calculation.d4(i)
  
  }
  if (!is.na(ret)) {
    if (ret < 0 & negative.as.NA) {
      ret <- NA
    }
  }
  
  ret
  })

}

#' Control Chart Constant A2  
#' 
#' Calculate A2 control chart constant from Wheeler's SPC books.
#'
#' @param sample.size A scalar or vector of sample sizes
#' @param n.sigma How many standard errors to use (default 3 for most control charts)
#'
#' @return A scalar or vector with computed constants. 


# .spc.cached.constant.A2 <- data.frame(
#   sample.size = 2:15,
#   A2 = c(1.880, 1.023, 0.729, 0.577,
#          0.483, 0.419, 0.373, 0.337,
#          0.308, 0.285, 0.266, 0.249,
#          0.235, 0.223)
# )

spc.constant.calculation.A2 <- function(
  sample.size, 
  n.sigma = 3
) {
  # ret <- NA
  # 
  # idx <- which(.spc.cached.constant.A2$sample.size == sample.size)
  # if (length(idx) != 0) {
  #   ret <- .spc.cached.constant.A2$A2[idx]
  # } 
  # 
  # ret
  
  sapply(sample.size, FUN = function(i) {
    n.sigma/(spc.constant.calculation.d2(i) * sqrt(i))
  }) 
}


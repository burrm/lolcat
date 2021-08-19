#' Control Chart Constant d2  
#' 
#' Calculate d2 control chart constant from Wheeler's SPC books.
#'
#' @param sample.size A scalar or vector of sample sizes
#'
#' @return A scalar or vector with computed constants. 
# Integral of 1- F(x)^n - (1-F(x))^n
spc.constant.calculation.d2 <- function(
  sample.size
) {
  sapply(sample.size, FUN = function(i) {
    d2.f <- function(x) {
      1- pnorm(x)^i - (1-pnorm(x))^i
    }
    
    integrate(d2.f, -Inf, Inf)$value
  })
}
#' Control Chart Constant d3  
#' 
#' Calculate d3 control chart constant from Wheeler's SPC books.
#'
#' @param sample.size A scalar or vector of sample sizes
#'
#' @return A scalar or vector with computed constants. 

# d3 originally borrowed from SixSigma package
spc.constant.calculation.d3 <- function(
  sample.size
) {
  sapply(sample.size, FUN = function(i) {
    d3.f <- function(x) {
      x * (1 - ptukey(x, i, Inf))
    }
  
    d3.tmp <- integrate(d3.f, 0, Inf)$value*2
  
    #d3.tmp
 
    sqrt(d3.tmp-spc.constant.calculation.d2(i)^2)
  })


}



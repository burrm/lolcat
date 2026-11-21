#' Convert Kurtosis  
#' 
#' Convert between Fisher's and Pearson's calculations for kurtosis.
#'
#' @param x Scalar/Vector - One or more kurtosis values to convert.
#' @param sample.size Scalar/Vector - One or more sample sizes used in calculation of kurtosis.
#' @param from Scalar/Character - Identify which kurtosis calculation is used for input.
#' @param to Scalar/Character - Identify which kurtosis calculation is used for output.
#'
#' @return Scalar/Vector - Converted kurtosis values.
convert.kurtosis <- function(x
                             ,sample.size
                             ,from = c("fisher", "pearson")
                             ,to   = c("pearson", "fisher")
) {
  ret <- x
  n <- sample.size
  
  if (from[1] == "fisher" && to[1] == "pearson") {
    b2 <- ( (n-2)*(n-3) / ((n+1)*(n-1))  ) * x + 3*(n-1)/(n+1)
    ret <- b2
    
  } else if (from[1] == "pearson" && to[1] == "fisher") {
    g2 <- (x - 3*(n-1)/(n+1)) / ( (n-2)*(n-3) / ((n+1)*(n-1))  ) 
    ret <- g2
  }
  
  ret
}
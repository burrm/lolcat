#' Convert Skewness  
#' 
#' Convert between Fisher's and Pearson's calculations for skewness.
#'
#' @param x Scalar/Vector - One or more skewness values to convert.
#' @param sample.size Scalar/Vector - One or more sample sizes used in calculation of skewness.
#' @param from Scalar/Character - Identify which skewness calculation is used for input.
#' @param to Scalar/Character - Identify which skewness calculation is used for output.
#'
#' @return Scalar/Vector - Converted skewness values.
convert.skewness <- function(x
                             ,sample.size
                             ,from = c("fisher", "pearson")
                             ,to   = c("pearson", "fisher")
) {
  ret <- x
  n <- sample.size
  
  if (from[1] == "fisher" && to[1] == "pearson") {
    root.b1 <- x * ( (n-2)/sqrt(n*(n-1)) )
    ret <- root.b1
    
  } else if (from[1] == "pearson" && to[1] == "fisher") {
    g1 <- x / ( (n-2)/sqrt(n*(n-1)) )
    ret <- g1
  }
  
  ret
}
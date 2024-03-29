#' Skewness  
#' 
#' Calculate skewness for a variable.
#'
#' @param x A vector of values
#' @param method The method to use to compute skewness. Methods due to Pearson and Fisher are available.
#'
#' @return A scalar containing the skewness estimate. 

skewness <- function(x
                     ,method = c("fisher", "pearson")
                     ) {
  n  <- length(na.omit(x))
  m2 <- .moment.m2(x)
  m3 <- .moment.m3(x)
  
  ret <- if (method[1] == "fisher") {
    (m3 / m2^(3/2)) * sqrt(n*(n-1))/(n-2)
  } else if (method[1] == "pearson") {
    m3 / m2^(3/2)
  }

  ret
}

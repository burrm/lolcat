#' Kurtosis  
#' 
#' Calculate kurtosis for a variable.
#'
#' @param x A vector of values
#' @param method The method to use to compute kurtosis. Methods due to Pearson and Fisher are available.
#'
#' @return A scalar containing the kurtosis estimate. 
kurtosis <- function(x
                     ,method = c("fisher", "pearson")) {
  n  <- length(na.omit(x))
  m2 <- .moment.m2(x)
  m4 <- .moment.m4(x)
  
  ret <- if (method[1] == "fisher") {
    ((n-1)*(n+1)/((n-2)*(n-3)))*m4/m2^2 - 3*(n-1)^2/((n-2)*(n-3))
  } else if (method[1] == "pearson") {
    m4/m2^2
  }
  
  ret
}

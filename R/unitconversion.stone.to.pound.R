#' Unit Conversion - Mass/Weight - Stone to U.S. Pound  
#' 
#' Performs a conversion of weights from stones to pounds (avoirdupois). 
#'
#' @param x Vector - Values in units of stones
#'
#' @return x, but converted to pounds 
unitconversion.stone.to.pound <- function(
  x = 1
) {
  x * 14
}

#' @rdname unitconversion.stone.to.pound
unitconversion.st.to.lb <- unitconversion.stone.to.pound
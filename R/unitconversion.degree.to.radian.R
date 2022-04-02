#' Unit Conversion - Angles - Degrees to Radians  
#' 
#' Performs a conversion of angle measures from degrees to radians. 
#'
#' @param x Vector - Values in units of degrees
#'
#' @return x, but converted to radians 
unitconversion.degree.to.radian <- function(
  x = 1
) {
  x * (2*constants.pi)/360
}
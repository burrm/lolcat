#' Unit Conversion - Angles - Radians to Degrees 
#' 
#' Performs a conversion of angle measures from radians to degrees. 
#'
#' @param x Vector - Values in units of radians
#'
#' @return x, but converted to degrees 
unitconversion.radian.to.degree <- function(
  x = 1
) {
  x * 360 / (2*constants.pi)
}
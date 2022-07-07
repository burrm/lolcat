#' Unit Conversion - Mass/Weight - Grain to U.S. Pound  
#' 
#' Performs a conversion of weights from grains to pounds (avoirdupois). 
#'
#' @param x Vector - Values in units of grains
#'
#' @return x, but converted to pounds 
unitconversion.grain.to.pound <- function(
  x = 1
) {
  x / 7000
}

#' @rdname unitconversion.grain.to.pound
unitconversion.gr.to.lb <- unitconversion.grain.to.pound
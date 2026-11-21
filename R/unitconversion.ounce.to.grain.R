#' Unit Conversion - Mass/Weight - U.S. Ounce to Grain  
#' 
#' Performs a conversion of weights from ounces (avoirdupois) to grains. 
#'
#' @param x Vector - Values in units of ounces
#'
#' @return x, but converted to grains 
unitconversion.ounce.to.grain <- function(
  x = 1
) {
  x * 437.5
}

#' @rdname unitconversion.ounce.to.grain
unitconversion.oz.to.gr <- unitconversion.ounce.to.grain
#' Unit Conversion - Mass/Weight - Troy Ounce to U.S. Ounce  
#' 
#' Performs a conversion of weights from troy ounces to ounces (avoirdupois). 
#'
#' @param x Vector - Values in units of troy ounces
#'
#' @return x, but converted to ounces (avoirdupois) 
unitconversion.troy.ounce.to.ounce <- function(
  x = 1
) {
  x * (192/175)
}

#' @rdname unitconversion.troy.ounce.to.ounce
unitconversion.ozt.to.oz <- unitconversion.troy.ounce.to.ounce
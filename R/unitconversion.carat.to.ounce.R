#' Unit Conversion - Mass/Weight - Carat to Avoirdupois Ounce  
#' 
#' Performs a conversion of weights from carats to ounces (avoirdupois). 
#'
#' @param x Vector - Values in units of carats
#'
#' @return x, but converted to ounces 
unitconversion.carat.to.ounce <- function(
  x = 1
) {
  x * .00705
}

#' @rdname unitconversion.carat.to.ounce
unitconversion.ct.to.oz <- unitconversion.carat.to.ounce
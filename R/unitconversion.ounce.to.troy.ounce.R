#' Unit Conversion - Mass/Weight - U.S. Ounce to Troy Ounce  
#' 
#' Performs a conversion of weights from ounces (avoirdupois) to troy ounces. 
#'
#' @param x Vector - Values in units of ounces
#'
#' @return x, but converted to troy ounces 
unitconversion.ounce.to.troy.ounce <- function(
  x = 1
) {
  x * (175/192)
}

#' @rdname unitconversion.ounce.to.troy.ounce
unitconversion.oz.to.ozt <- unitconversion.ounce.to.troy.ounce
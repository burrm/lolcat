#' Unit Conversion - Mass/Weight - Grain to Avoirdupois Ounce  
#' 
#' Performs a conversion of weights from grains to ounces (avoirdupois). 
#'
#' @param x Vector - Values in units of grains
#'
#' @return x, but converted to ounces 
unitconversion.grain.to.ounce <- function(
  x = 1
) {
  x / 437.5
}

#' @rdname unitconversion.grain.to.ounce
unitconversion.gr.to.oz <- unitconversion.grain.to.ounce
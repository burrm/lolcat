#' Unit Conversion - Mass/Weight - Grain to Troy Ounce  
#' 
#' Performs a conversion of weights from grains to troy ounces. 
#'
#' @param x Vector - Values in units of grains
#'
#' @return x, but converted to Troy ounces 
unitconversion.grain.to.troy.ounce <- function(
  x = 1
) {
  x / 480
}

#' @rdname unitconversion.grain.to.troy.ounce
unitconversion.gr.to.ozt <- unitconversion.grain.to.troy.ounce
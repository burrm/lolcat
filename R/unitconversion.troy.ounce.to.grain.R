#' Unit Conversion - Mass/Weight - Troy Ounce to Grain  
#' 
#' Performs a conversion of weights from troy ounces to grains. 
#'
#' @param x Vector - Values in units of troy ounces
#'
#' @return x, but converted to grains 
unitconversion.troy.ounce.to.grain <- function(
  x = 1
) {
  x * 480
}

#' @rdname unitconversion.troy.ounce.to.grain
unitconversion.ozt.to.gr <- unitconversion.troy.ounce.to.grain
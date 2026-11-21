#' Unit Conversion - Mass/Weight - Troy Ounce to Troy Pound  
#' 
#' Performs a conversion of weights from troy ounces to troy pounds. 
#'
#' @param x Vector - Values in units of troy ounces
#'
#' @return x, but converted to troy pounds 
unitconversion.troy.ounce.to.troy.pound <- function(
  x = 1
) {
  x / 12
}

#' @rdname unitconversion.troy.ounce.to.troy.pound
unitconversion.ozt.to.lbt <- unitconversion.troy.ounce.to.troy.pound
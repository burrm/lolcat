#' Unit Conversion - Mass/Weight - Troy Pennyweight to Troy Ounce  
#' 
#' Performs a conversion of weights from troy pennyweights to troy ounces. 
#'
#' @param x Vector - Values in units of pennyweights
#'
#' @return x, but converted to troy ounces 
unitconversion.pennyweight.to.troy.ounce <- function(
  x = 1
) {
  x / 20
}

#' @rdname unitconversion.pennyweight.to.troy.ounce
unitconversion.dwt.to.ozt <- unitconversion.pennyweight.to.troy.ounce
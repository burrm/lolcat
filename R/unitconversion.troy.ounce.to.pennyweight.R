#' Unit Conversion - Mass/Weight - Troy Ounce to Troy Pennyweight  
#' 
#' Performs a conversion of weights from troy ounces to troy pennyweights. 
#'
#' @param x Vector - Values in units of troy ounces
#'
#' @return x, but converted to troy pennyweights 
unitconversion.troy.ounce.to.pennyweight <- function(
  x = 1
) {
  x * 20
}

#' @rdname unitconversion.troy.ounce.to.pennyweight
unitconversion.ozt.to.dwt <- unitconversion.troy.ounce.to.pennyweight
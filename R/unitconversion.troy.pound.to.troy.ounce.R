#' Unit Conversion - Mass/Weight - Troy Pound to Troy Ounce  
#' 
#' Performs a conversion of weights from troy pounds to troy ounces. 
#'
#' @param x Vector - Values in units of troy pounds
#'
#' @return x, but converted to troy ounces 
unitconversion.troy.pound.to.troy.ounce <- function(
  x = 1
) {
  x * 12
}

#' @rdname unitconversion.troy.pound.to.troy.ounce
unitconversion.lbt.to.ozt <- unitconversion.troy.pound.to.troy.ounce
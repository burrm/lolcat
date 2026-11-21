#' Unit Conversion - Liquid Volume - Pint to Fluid Ounce  
#' 
#' Performs a conversion of volumes from pints to fluid ounces. 
#'
#' @param x Vector - Values in units of pints
#'
#' @return x, but converted to fluid ounces 
unitconversion.pint.to.fluid.ounce <- function(
  x = 1
) {
  x * 16
}

#' @rdname unitconversion.pint.to.fluid.ounce
unitconversion.pt.to.fl.oz <- unitconversion.pint.to.fluid.ounce
#' Unit Conversion - Liquid Volume - Fluid Ounce to Pint  
#' 
#' Performs a conversion of volumes from fluid ounces to pints. 
#'
#' @param x Vector - Values in units of fluid ounces
#'
#' @return x, but converted to pints 
unitconversion.fluid.ounce.to.pint <- function(
  x = 1
) {
  x / 16
}

#' @rdname unitconversion.fluid.ounce.to.pint
unitconversion.fl.oz.to.pt <- unitconversion.fluid.ounce.to.pint
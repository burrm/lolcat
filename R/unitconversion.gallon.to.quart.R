#' Unit Conversion - Liquid Volume -  Gallon to Quart  
#' 
#' Performs a conversion of volumes from gallons to quarts. 
#'
#' @param x Vector - Values in units of gallons
#'
#' @return x, but converted to quarts 
unitconversion.gallon.to.quart <- function(
  x = 1
) {
  x * 4
}

#' @rdname unitconversion.gallon.to.quart
unitconversion.gal.to.qt <- unitconversion.gallon.to.quart
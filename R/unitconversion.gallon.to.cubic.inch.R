#' Unit Conversion - Liquid Volume -  Gallon to Cubic Inch  
#' 
#' Performs a conversion of volumes from gallons to cubic inches. 
#'
#' @param x Vector - Values in units of gallons
#'
#' @return x, but converted to cubic inches 
unitconversion.gallon.to.cubic.inch <- function(
  x = 1
) {
  x * 231
}

#' @rdname unitconversion.gallon.to.cubic.inch
unitconversion.gal.to.cu.in <- unitconversion.gallon.to.cubic.inch
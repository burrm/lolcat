#' Unit Conversion - Liquid Volume -  Cubic Inch to Gallon  
#' 
#' Performs a conversion of volumes from cubic inches to gallons. 
#'
#' @param x Vector - Values in units of cubic inches
#'
#' @return x, but converted to gallons 
unitconversion.cubic.inch.to.gallon <- function(
  x = 1
) {
  x / 231
}

#' @rdname unitconversion.cubic.inch.to.gallon
unitconversion.cu.in.to.gal <- unitconversion.cubic.inch.to.gallon
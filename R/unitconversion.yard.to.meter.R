#' Unit Conversion - Length - Yard to Meter  
#' 
#' Performs a conversion of lengths from yards to meters. 
#'
#' @param x Vector - Values in units of yards
#'
#' @return x, but converted to meters 
#'
#' @references
#' NIST. Approximate Conversions from U.S. Customary Measures to Metric. 2022. Accessed 3/16/2022. 
#' https://www.nist.gov/pml/weights-and-measures/approximate-conversions-us-customary-measures-metric
unitconversion.yard.to.meter <- function(
  x = 1
) {
  x * 0.91
}

#' @rdname unitconversion.yard.to.meter
unitconversion.yd.to.m <- unitconversion.yard.to.meter
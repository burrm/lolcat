#' Unit Conversion - Length - Inch to Centimeter  
#' 
#' Performs a conversion of lengths from inches to centimeters. 
#'
#' @param x Vector - Values in units of inches
#'
#' @return x, but converted to centimeters 
#'
#' @references
#' NIST. Approximate Conversions from U.S. Customary Measures to Metric. 2022. Accessed 3/16/2022. 
#' https://www.nist.gov/pml/weights-and-measures/approximate-conversions-us-customary-measures-metric
unitconversion.inch.to.centimeter <- function(
  x = 1
) {
  x * 2.54
}

#' @rdname unitconversion.inch.to.centimeter
unitconversion.in.to.cm <- unitconversion.inch.to.centimeter
#' Unit Conversion - Length - Foot to Centimeter  
#' 
#' Performs a conversion of lengths from feet to centimeters. 
#'
#' @param x Vector - Values in units of feet
#'
#' @return x, but converted to centimeters 
#'
#' @references
#' NIST. Approximate Conversions from U.S. Customary Measures to Metric. 2022. Accessed 3/16/2022. 
#' https://www.nist.gov/pml/weights-and-measures/approximate-conversions-us-customary-measures-metric
unitconversion.foot.to.centimeter <- function(
  x = 1
) {
  x * 30.48
}

#' @rdname unitconversion.foot.to.centimeter
unitconversion.ft.to.cm <- unitconversion.foot.to.centimeter
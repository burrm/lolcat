#' Unit Conversion - Length - Mile to Kilometer  
#' 
#' Performs a conversion of lengths from miles to kilometers. 
#'
#' @param x Vector - Values in units of miles
#'
#' @return x, but converted to kilometers 
#'
#' @references
#' NIST. Approximate Conversions from U.S. Customary Measures to Metric. 2022. Accessed 3/16/2022. 
#' https://www.nist.gov/pml/weights-and-measures/approximate-conversions-us-customary-measures-metric
unitconversion.mile.to.kilometer <- function(
  x = 1
) {
  x * 1.61
}

#' @rdname unitconversion.mile.to.kilometer
unitconversion.mi.to.km <- unitconversion.mile.to.kilometer
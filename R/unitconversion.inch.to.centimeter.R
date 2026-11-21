#' Unit Conversion - Length - Inch to Centimeter  
#' 
#' Performs a conversion of lengths from inches to centimeters. 
#'
#' @param x Vector - Values in units of inches
#'
#' @return x, but converted to centimeters 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.inch.to.centimeter <- function(
  x = 1
) {
  x * 2.54
}

#' @rdname unitconversion.inch.to.centimeter
unitconversion.in.to.cm <- unitconversion.inch.to.centimeter
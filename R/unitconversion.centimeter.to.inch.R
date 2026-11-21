#' Unit Conversion - Length - Centimeter to Inch  
#' 
#' Performs a conversion of lengths from centimeters to inches. 
#'
#' @param x Vector - Values in units of centimeters
#'
#' @return x, but converted to inches 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.centimeter.to.inch <- function(
  x = 1
) {
  x / 2.54
}

#' @rdname unitconversion.centimeter.to.inch
unitconversion.cm.to.in <- unitconversion.centimeter.to.inch
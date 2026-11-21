#' Unit Conversion - Length - Inch to Hand 
#' 
#' Performs a conversion of lengths from inches to hands. 
#'
#' @param x Vector - Values in units of inches
#'
#' @return x, but converted to hands 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.inch.to.hand <- function(
  x = 1
) {
  x / 4
}
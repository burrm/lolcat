#' Unit Conversion - Length - Hand to Inch 
#' 
#' Performs a conversion of lengths from hands to inches. 
#'
#' @param x Vector - Values in units of hands
#'
#' @return x, but converted to inches 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.hand.to.inch <- function(
  x = 1
) {
  x * 4
}
#' Unit Conversion - Thickness - Inch to Mil  
#' 
#' Performs a conversion of lengths from inches to mils. 
#'
#' @param x Vector - Values in units of inches
#'
#' @return x, but converted to mils 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.inch.to.mil <- function(
  x = 1
) {
  x / .001
}

#' @rdname unitconversion.inch.to.mil
unitconversion.in.to.mil <- unitconversion.inch.to.mil
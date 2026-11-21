#' Unit Conversion - Volume - Cubic Inch to Cubic Foot  
#' 
#' Performs a conversion of volumes from cubic inches to cubic feet. 
#'
#' @param x Vector - Values in units of cubic inches
#'
#' @return x, but converted to cubic feet
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.cubic.inch.to.cubic.foot <- function(
  x = 1
) {
  x / 1728
}

#' @rdname unitconversion.cubic.inch.to.cubic.foot
unitconversion.cu.in.to.cu.ft <- unitconversion.cubic.inch.to.cubic.foot
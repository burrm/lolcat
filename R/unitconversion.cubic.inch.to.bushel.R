#' Unit Conversion - Dry Volume - Cubic Inch to Bushel  
#' 
#' Performs a conversion of volumes from cubic inches to bushels. 
#'
#' @param x Vector - Values in units of cubic inches
#'
#' @return x, but converted to bushels
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.cubic.inch.to.bushel <- function(
  x = 1
) {
  x / 2150.42
}

#' @rdname unitconversion.cubic.inch.to.bushel
unitconversion.cu.in.to.bu <- unitconversion.cubic.inch.to.bushel
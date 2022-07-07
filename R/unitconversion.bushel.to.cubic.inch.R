#' Unit Conversion - Dry Volume - Bushel to Cubic Inch  
#' 
#' Performs a conversion of volumes from bushels to cubic inches. 
#'
#' @param x Vector - Values in units of bushels
#'
#' @return x, but converted to cubic inches
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.bushel.to.cubic.inch <- function(
  x = 1
) {
  x * 2150.42
}

#' @rdname unitconversion.bushel.to.cubic.inch
unitconversion.bu.to.cu.in <- unitconversion.bushel.to.cubic.inch
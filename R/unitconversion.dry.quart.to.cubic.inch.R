#' Unit Conversion - Dry Volume - Dry Quart to Cubic Inch  
#' 
#' Performs a conversion of volumes from dry quarts to cubic inches. 
#'
#' @param x Vector - Values in units of quarts
#'
#' @return x, but converted to cubic inches 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.dry.quart.to.cubic.inch <- function(
  x = 1
) {
  x * 67.200625
}

#' @rdname unitconversion.dry.quart.to.cubic.inch
unitconversion.dry.qt.to.cu.in <- unitconversion.dry.quart.to.cubic.inch
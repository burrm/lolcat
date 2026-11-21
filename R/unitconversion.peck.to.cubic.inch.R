#' Unit Conversion - Dry Volume - Peck to Cubic Inch 
#' 
#' Performs a conversion of volumes from dry pecks to cubic inches. 
#'
#' @param x Vector - Values in units of pecks
#'
#' @return x, but converted to cubic inches
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.peck.to.cubic.inch <- function(
  x = 1
) {
  x * 537.605
}

#' @rdname unitconversion.peck.to.cubic.inch
unitconversion.pk.to.cu.in <- unitconversion.peck.to.cubic.inch
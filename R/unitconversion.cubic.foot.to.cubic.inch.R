#' Unit Conversion - Liquid Volume -  Cubic Foot to Cubic Inch  
#' 
#' Performs a conversion of volumes from cubic feet to cubic inches. 
#'
#' @param x Vector - Values in units of cubic feet
#'
#' @return x, but converted to cubic inches
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf 
unitconversion.cubic.foot.to.cubic.inch <- function(
  x = 1
) {
  x * 1728
}

#' @rdname unitconversion.cubic.foot.to.cubic.inch
unitconversion.cu.ft.to.cu.in <- unitconversion.cubic.foot.to.cubic.inch
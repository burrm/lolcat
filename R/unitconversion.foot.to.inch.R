#' Unit Conversion - Length - Foot to Inch  
#' 
#' Performs a conversion of lengths from feet to inches. 
#'
#' @param x Vector - Values in units of feet
#'
#' @return x, but converted to inches 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.foot.to.inch <- function(
  x = 1
) {
  x * 12
}

#' @rdname unitconversion.foot.to.inch
unitconversion.ft.to.in <- unitconversion.foot.to.inch
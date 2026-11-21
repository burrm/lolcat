#' Unit Conversion - Area - Square Foot to Square Inch  
#' 
#' Performs a conversion of areas from square feet to square inches. 
#'
#' @param x Vector - Values in units of square feet
#'
#' @return x, but converted to square inch 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.square.foot.to.square.inch <- function(
  x = 1
) {
  x * 144
}

#' @rdname unitconversion.square.foot.to.square.inch
unitconversion.sq.ft.to.sq.in <- unitconversion.square.foot.to.square.inch
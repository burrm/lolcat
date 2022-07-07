#' Unit Conversion - Area - Square Inch to Square Foot  
#' 
#' Performs a conversion of areas from square inches to square feet. 
#'
#' @param x Vector - Values in units of square inches
#'
#' @return x, but converted to square feet 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.square.inch.to.square.foot <- function(
  x = 1
) {
  x / 144
}

#' @rdname unitconversion.square.inch.to.square.foot
unitconversion.sq.in.to.sq.ft <- unitconversion.square.inch.to.square.foot
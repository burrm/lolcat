#' Unit Conversion - Area - Square Foot to Square Yard  
#' 
#' Performs a conversion of areas from square feet to square yards. 
#'
#' @param x Vector - Values in units of square feet
#'
#' @return x, but converted to square yards 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.square.foot.to.square.yard <- function(
  x = 1
) {
  x / 9
}

#' @rdname unitconversion.square.foot.to.square.yard
unitconversion.sq.ft.to.sq.yd <- unitconversion.square.foot.to.square.yard
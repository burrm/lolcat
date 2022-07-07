#' Unit Conversion - Length - Foot to Mile  
#' 
#' Performs a conversion of lengths from feet to miles. 
#'
#' @param x Vector - Values in units of feet
#'
#' @return x, but converted to miles 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.foot.to.mile <- function(
  x = 1
) {
  x / 5280
}

#' @rdname unitconversion.foot.to.mile
unitconversion.ft.to.mi <- unitconversion.foot.to.mile
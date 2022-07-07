#' Unit Conversion - Length - Foot to Link  
#' 
#' Performs a conversion of lengths from feet to links (surveyor's chain). 
#'
#' @param x Vector - Values in units of feet
#'
#' @return x, but converted to links 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.foot.to.link <- function(
  x = 1
) {
  x / .66
}

#' @rdname unitconversion.foot.to.link
unitconversion.ft.to.li <- unitconversion.foot.to.link
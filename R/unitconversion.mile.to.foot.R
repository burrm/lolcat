#' Unit Conversion - Length - Mile to Foot  
#' 
#' Performs a conversion of lengths from miles to feet. 
#'
#' @param x Vector - Values in units of miles
#'
#' @return x, but converted to feet 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.mile.to.foot <- function(
  x = 1
) {
  x * 5280
}

#' @rdname unitconversion.mile.to.foot
unitconversion.mi.to.ft <- unitconversion.mile.to.foot
#' Unit Conversion - Length - Furlong to Mile  
#' 
#' Performs a conversion of lengths from furlongs to miles (U.S. statute/survey miles). 
#'
#' @param x Vector - Values in units of furlongs
#'
#' @return x, but converted to miles 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.furlong.to.mile <- function(
  x = 1
) {
  x / 8
}

#' @rdname unitconversion.furlong.to.mile
unitconversion.fur.to.mi <- unitconversion.furlong.to.mile
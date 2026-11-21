#' Unit Conversion - Length - Kilometer to Miles  
#' 
#' Performs a conversion of lengths from kilometers to miles (international). 
#'
#' @param x Vector - Values in units of kilometers
#'
#' @return x, but converted to miles 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.kilometer.to.mile <- function(
  x = 1
) {
  x / 1.609344
}

#' @rdname unitconversion.kilometer.to.mile
unitconversion.km.to.mi <- unitconversion.kilometer.to.mile
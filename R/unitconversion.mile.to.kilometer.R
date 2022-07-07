#' Unit Conversion - Length - Mile to Kilometer  
#' 
#' Performs a conversion of lengths from miles to kilometers. 
#'
#' @param x Vector - Values in units of miles
#'
#' @return x, but converted to kilometers 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.mile.to.kilometer <- function(
  x = 1
) {
  x * 1.609344
}

#' @rdname unitconversion.mile.to.kilometer
unitconversion.mi.to.km <- unitconversion.mile.to.kilometer
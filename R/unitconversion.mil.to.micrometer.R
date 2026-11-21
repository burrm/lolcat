#' Unit Conversion - Thickness - Mil to Micrometer  
#' 
#' Performs a conversion of lengths from mils to micrometers. 
#' Note, the character 'u' is used in place of 'mu' in "micro-" SI unit abbreviations due to naming/keyboard limitations.
#'
#' @param x Vector - Values in units of mils
#'
#' @return x, but converted to micrometers 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E - General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.mil.to.micrometer <- function(
  x = 1
) {
  x * 25.4
}

#' @rdname unitconversion.mil.to.micrometer
unitconversion.mil.to.um <- unitconversion.mil.to.micrometer

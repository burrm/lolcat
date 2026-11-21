#' Unit Conversion - Thickness - Micrometer to Mil  
#' 
#' Performs a conversion of lengths from micrometers to mils. 
#' Note, the character 'u' is used in place of 'mu' in "micro-" SI unit abbreviations due to naming/keyboard limitations.
#'
#' @param x Vector - Values in units of micrometers
#'
#' @return x, but converted to mils 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E - General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.micrometer.to.mil <- function(
  x = 1
) {
  x / 25.4
}

#' @rdname unitconversion.micrometer.to.mil
unitconversion.um.to.mil <- unitconversion.micrometer.to.mil

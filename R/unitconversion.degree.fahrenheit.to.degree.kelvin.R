#' Unit Conversion - Temperature - Fahrenheit to Kelvin  
#' 
#' Performs a conversion of temperature from Fahrenheit to Kelvin. 
#'
#' @param x Vector - Values in units of degrees Fahrenheit.
#'
#' @return x, but converted to degrees Kelvin
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.degree.fahrenheit.to.degree.kelvin <- function(
  x = 1
) {
  ((x - 32)/1.8)+273.15
}

#' @rdname unitconversion.degree.fahrenheit.to.degree.kelvin
unitconversion.deg.F.to.deg.K <- unitconversion.degree.fahrenheit.to.degree.kelvin
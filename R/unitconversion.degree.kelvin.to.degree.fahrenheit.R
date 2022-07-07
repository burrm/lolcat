#' Unit Conversion - Temperature - Kelvin to Fahrenheit  
#' 
#' Performs a conversion of temperature from Kelvin to Fahrenheit. 
#'
#' @param x Vector - Values in units of degrees Kelvin.
#'
#' @return x, but converted to degrees Fahrenheit.
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.degree.kelvin.to.degree.fahrenheit <- function(
  x = 1
) {
  1.8*(x-273.15) + 32
}

#' @rdname unitconversion.degree.kelvin.to.degree.fahrenheit
unitconversion.deg.K.to.deg.F <- unitconversion.degree.kelvin.to.degree.fahrenheit
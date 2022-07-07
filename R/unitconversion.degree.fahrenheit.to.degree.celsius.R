#' Unit Conversion - Temperature - Fahrenheit to Celsius  
#' 
#' Performs a conversion of temperature from Fahrenheit to Celsius. 
#'
#' @param x Vector - Values in units of degrees Fahrenheit.
#'
#' @return x, but converted to degrees Celsius
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.degree.fahrenheit.to.degree.celsius <- function(
  x = 1
) {
  (x - 32)/1.8
}

#' @rdname unitconversion.degree.fahrenheit.to.degree.celsius
unitconversion.deg.F.to.deg.C <- unitconversion.degree.fahrenheit.to.degree.celsius
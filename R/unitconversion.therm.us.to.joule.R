#' Unit Conversion - Energy - U.S. Therm to Joule  
#' 
#' Performs a conversion of energy from U.S. therms to joules. 
#'
#' @param x Vector - Values in units of therms
#'
#' @return x, but converted to joules 
#'
#' @references
#' Thompson, Ambler and Taylor, Barry. NIST Special Publication 811 - Guide for the use of the International System of Units (SI). 2008.  
#' https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication811e2008.pdf
unitconversion.therm.us.to.joule <- function(
  x = 1
) {
  x * 1.054804E8
}
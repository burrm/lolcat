#' Unit Conversion - Energy - Joule to U.S. Therm  
#' 
#' Performs a conversion of energy from joules to U.S. therms. 
#'
#' @param x Vector - Values in units of joules
#'
#' @return x, but converted to therms 
#'
#' @references
#' Thompson, Ambler and Taylor, Barry. NIST Special Publication 811 - Guide for theUse of the International System of Units (SI). 2008.  
#' https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication811e2008.pdf
unitconversion.joule.to.therm.us <- function(
  x = 1
) {
  x / 1.054804E8
}
#' Unit Conversion - Length - Astronomical Unit to Meter  
#' 
#' Performs a conversion of length from astronomical units to meters. 
#'
#' @param x Vector - Values in units of astronomical units
#'
#' @return x, but converted to meters 
#'
#' @references
#' Thompson, Ambler and Taylor, Barry. NIST Special Publication 811 - Guide for theUse of the International System of Units (SI). 2008.  
#' https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication811e2008.pdf
unitconversion.astronomical.unit.to.meter <- function(
  x = 1
) {
  x * 1.495979E11
}

#' @rdname unitconversion.astronomical.unit.to.meter
unitconversion.au.to.m <- unitconversion.astronomical.unit.to.meter
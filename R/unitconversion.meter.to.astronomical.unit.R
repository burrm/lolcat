#' Unit Conversion - Length - Meter to Astronomical Unit  
#' 
#' Performs a conversion of length from meters to astronomical units. 
#'
#' @param x Vector - Values in units of meters
#'
#' @return x, but converted to astronomical units 
#'
#' @references
#' Thompson, Ambler and Taylor, Barry. NIST Special Publication 811 - Guide for theUse of the International System of Units (SI). 2008.  
#' https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication811e2008.pdf
unitconversion.meter.to.astronomical.unit <- function(
  x = 1
) {
  x / 1.495979E11
}

#' @rdname unitconversion.meter.to.astronomical.unit 
unitconversion.m.to.au  <- unitconversion.meter.to.astronomical.unit 
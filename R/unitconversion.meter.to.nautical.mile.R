#' Unit Conversion - Length -  Meter to Nautical Mile  
#' 
#' Performs a conversion of lengths from meters to nautical miles. 
#'
#' @param x Vector - Values in units of meters
#'
#' @return x, but converted to nautical miles 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.meter.to.nautical.mile <- function(
  x = 1
) {
  x / 1852
}

#' @rdname unitconversion.meter.to.nautical.mile
unitconversion.m.to.nmi <- unitconversion.meter.to.nautical.mile
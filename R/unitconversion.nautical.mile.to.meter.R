#' Unit Conversion - Length - Nautical Mile to Meter  
#' 
#' Performs a conversion of lengths from nautical miles to meters. 
#'
#' @param x Vector - Values in units of nautical miles
#'
#' @return x, but converted to meters 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.nautical.mile.to.meter <- function(
  x = 1
) {
  x * 1852
}

#' @rdname unitconversion.nautical.mile.to.meter
unitconversion.nmi.to.m <- unitconversion.nautical.mile.to.meter
#' Unit Conversion - Area - Square Meter to Hectare  
#' 
#' Performs a conversion of areas from square meters to hectares. 
#'
#' @param x Vector - Values in units of square meters
#'
#' @return x, but converted to hectares 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.square.meter.to.hectare <- function(
  x = 1
) {
  x / 10000
}

#' @rdname unitconversion.square.meter.to.hectare 
unitconversion.sq.m.to.ha <- unitconversion.square.meter.to.hectare 
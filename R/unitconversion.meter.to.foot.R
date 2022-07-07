#' Unit Conversion - Length - Meter to Foot  
#' 
#' Performs a conversion of lengths from meters to feet. 
#'
#' @param x Vector - Values in units of meters
#'
#' @return x, but converted to feet 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.meter.to.foot <- function(
  x = 1
) {
  x / .3048
}

#' @rdname unitconversion.meter.to.foot
unitconversion.m.to.ft <- unitconversion.meter.to.foot
#' Unit Conversion - Area - Hectare to Square Meter  
#' 
#' Performs a conversion of areas from hectares to square meters. 
#'
#' @param x Vector - Values in units of hectares
#'
#' @return x, but converted to square meters 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.hectare.to.square.meter <- function(
  x = 1
) {
  x * 10000
}

#' @rdname unitconversion.hectare.to.square.meter 
unitconversion.ha.to.sq.m <- unitconversion.hectare.to.square.meter 
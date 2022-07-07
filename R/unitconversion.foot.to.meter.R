#' Unit Conversion - Length - Foot to Meter  
#' 
#' Performs a conversion of lengths from feet to meters. 
#'
#' @param x Vector - Values in units of feet
#'
#' @return x, but converted to meters 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.foot.to.meter <- function(
  x = 1
) {
  x * .3048
}

#' @rdname unitconversion.foot.to.meter
unitconversion.ft.to.m <- unitconversion.foot.to.meter
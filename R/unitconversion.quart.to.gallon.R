#' Unit Conversion - Liquid Volume - Quart to Gallon  
#' 
#' Performs a conversion of volumes from quarts to gallons. 
#'
#' @param x Vector - Values in units of quarts
#'
#' @return x, but converted to gallons
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf 
unitconversion.quart.to.gallon <- function(
  x = 1
) {
  x / 4
}

#' @rdname unitconversion.quart.to.gallon
unitconversion.qt.to.gal <- unitconversion.quart.to.gallon
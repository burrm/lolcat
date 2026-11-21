#' Unit Conversion - Dry Volume - Dry Quart to Peck  
#' 
#' Performs a conversion of volumes from dry quarts to pecks. 
#'
#' @param x Vector - Values in units of quarts
#'
#' @return x, but converted to pecks 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.dry.quart.to.peck <- function(
  x = 1
) {
  x / 8
}

#' @rdname unitconversion.dry.quart.to.peck
unitconversion.dry.qt.to.pk <- unitconversion.dry.quart.to.peck
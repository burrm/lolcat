#' Unit Conversion - Dry Volume - Peck to Dry Quart  
#' 
#' Performs a conversion of volumes from dry pecks to dry quarts. 
#'
#' @param x Vector - Values in units of pecks
#'
#' @return x, but converted to dry quarts 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.peck.to.dry.quart <- function(
  x = 1
) {
  x * 8
}

#' @rdname unitconversion.peck.to.dry.quart
unitconversion.pk.to.dry.qt <- unitconversion.peck.to.dry.quart
#' Unit Conversion - Dry Volume - Dry Quart to Dry Pint  
#' 
#' Performs a conversion of volumes from dry quarts to dry pints. 
#'
#' @param x Vector - Values in units of quarts
#'
#' @return x, but converted to pints 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.dry.quart.to.dry.pint <- function(
  x = 1
) {
  x * 2
}

#' @rdname unitconversion.dry.quart.to.dry.pint
unitconversion.dry.qt.to.dry.pt <- unitconversion.dry.quart.to.dry.pint
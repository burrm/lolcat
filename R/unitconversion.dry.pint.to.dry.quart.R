#' Unit Conversion - Dry Volume - Dry Pint to Dry Quart  
#' 
#' Performs a conversion of volumes from dry pints to dry quarts. 
#'
#' @param x Vector - Values in units of pints
#'
#' @return x, but converted to quarts 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.dry.pint.to.dry.quart <- function(
  x = 1
) {
  x / 2
}

#' @rdname unitconversion.dry.pint.to.dry.quart
unitconversion.dry.pt.to.dry.qt <- unitconversion.dry.pint.to.dry.quart
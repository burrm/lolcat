#' Unit Conversion - Mass/Weight - Carat to Milligram  
#' 
#' Performs a conversion of weights from carats to milligrams. 
#'
#' @param x Vector - Values in units of carats
#'
#' @return x, but converted to milligrams 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.carat.to.milligram <- function(
  x = 1
) {
  x * 200
}

#' @rdname unitconversion.carat.to.milligram
unitconversion.ct.to.mg <- unitconversion.carat.to.milligram
#' Unit Conversion - Mass/Weight - Milligram to Carat  
#' 
#' Performs a conversion of weights from milligrams to carats. 
#'
#' @param x Vector - Values in units of milligrams
#'
#' @return x, but converted to carats 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.milligram.to.carat <- function(
  x = 1
) {
  x / 200
}

#' @rdname unitconversion.milligram.to.carat
unitconversion.mg.to.ct <- unitconversion.milligram.to.carat
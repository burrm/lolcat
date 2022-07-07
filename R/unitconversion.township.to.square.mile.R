#' Unit Conversion - Area - Township to Square Mile   
#' 
#' Performs a conversion of areas from townships to square miles (U.S. Survey). 
#'
#' @param x Vector - Values in units of townships
#'
#' @return x, but converted to square miles 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.township.to.square.mile <- function(
  x = 1
) {
  x * 36
}
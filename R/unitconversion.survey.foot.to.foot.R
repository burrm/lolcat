#' Unit Conversion - Length - U.S. Foot to Foot  
#' 
#' Performs a conversion of lengths from U.S. survey feet to feet (international). 
#'
#' @param x Vector - Values in units of survey
#'
#' @return x, but converted to international feet 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.survey.foot.to.foot <- function(
  x = 1
) {
  x / .999998
}
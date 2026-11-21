#' Unit Conversion - Area - Square Yard to Acre  
#' 
#' Performs a conversion of areas from square yard to acres. Note, per NIST standard, this is a "survey foot"
#' instead of an "international foot" as is commonly used. In 2022, this will likely become the "international foot."
#' For most purposes, either survey feet or international feet may be used, but very large areas being calculated will have
#' an error of roughly a quarter sheet of paper (A4 or letter) per acre (2/1 000 000 difference between survey feet 
#' and international feet).
#'
#' @param x Vector - Values in units of square yards
#'
#' @return x, but converted to acres 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.square.yard.to.acre <- function(
  x = 1
) {
  x / 4840
}

#' @rdname unitconversion.square.yard.to.acre
unitconversion.sq.yd.to.ac <- unitconversion.square.yard.to.acre
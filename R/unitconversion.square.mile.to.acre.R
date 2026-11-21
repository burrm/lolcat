#' Unit Conversion - Area - Square Mile to Acre  
#' 
#' Performs a conversion of areas from square miles (U.S. Survey) to acres. 
#'
#' @param x Vector - Values in units of square miles
#'
#' @return x, but converted to acres 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.square.mile.to.acre <- function(
  x = 1
) {
  x * 640
}

#' @rdname unitconversion.square.mile.to.acre
unitconversion.sq.mi.to.ac <- unitconversion.square.mile.to.acre
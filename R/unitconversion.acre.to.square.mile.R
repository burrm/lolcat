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
unitconversion.acre.to.square.mile <- function(
  x = 1
) {
  x / 640
}

#' @rdname unitconversion.acre.to.square.mile
unitconversion.ac.to.sq.mi <- unitconversion.acre.to.square.mile
#' Unit Conversion - Length - Mile to Furlong  
#' 
#' Performs a conversion of lengths from miles (U.S. statute/survey miles) to furlongs. 
#'
#' @param x Vector - Values in units of miles
#'
#' @return x, but converted to furlongs 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.mile.to.furlong <- function(
  x = 1
) {
  x * 8
}

#' @rdname unitconversion.mile.to.furlong
unitconversion.mi.to.fur <- unitconversion.mile.to.furlong
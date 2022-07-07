#' Unit Conversion - Mass/Weight - U.S. Pound to Stone  
#' 
#' Performs a conversion of weights from pounds (avoirdupois) to stones. 
#'
#' @param x Vector - Values in units of pounds
#'
#' @return x, but converted to stone 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.pound.to.stone <- function(
  x = 1
) {
  x / 14
}

#' @rdname unitconversion.pound.to.stone
unitconversion.lb.to.st <- unitconversion.pound.to.stone
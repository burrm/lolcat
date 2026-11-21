#' Unit Conversion - Length - Chain to Link  
#' 
#' Performs a conversion of lengths from chains to links (surveyor's chain). 
#'
#' @param x Vector - Values in units of chains
#'
#' @return x, but converted to links 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.chain.to.link <- function(
  x = 1
) {
  x * 100
}

#' @rdname unitconversion.chain.to.link
unitconversion.ch.to.li <- unitconversion.chain.to.link
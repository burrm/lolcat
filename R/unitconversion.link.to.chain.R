#' Unit Conversion - Length - Link to Chain  
#' 
#' Performs a conversion of lengths from links (surveyor's chain) to chains. 
#'
#' @param x Vector - Values in units of links
#'
#' @return x, but converted to chains 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.link.to.chain <- function(
  x = 1
) {
  x / 100
}

#' @rdname unitconversion.link.to.chain
unitconversion.li.to.ch <- unitconversion.link.to.chain
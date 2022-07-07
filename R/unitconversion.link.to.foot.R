#' Unit Conversion - Length - Link to Foot  
#' 
#' Performs a conversion of lengths from links (surveyor's chain) to feet (U.S. survey). 
#'
#' @param x Vector - Values in units of links
#'
#' @return x, but converted to feet 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.link.to.foot <- function(
  x = 1
) {
  x * .66
}

#' @rdname unitconversion.link.to.foot
unitconversion.li.to.ft <- unitconversion.link.to.foot
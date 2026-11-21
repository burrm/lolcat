#' Unit Conversion - Length - Furlong to Rod  
#' 
#' Performs a conversion of lengths from furlongs to rods (also called poles and perches). 
#'
#' @param x Vector - Values in units of furlongs
#'
#' @return x, but converted to rods 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.furlong.to.rod <- function(
  x = 1
) {
  x * 40
}

#' @rdname unitconversion.furlong.to.rod
unitconversion.fur.to.rd <- unitconversion.furlong.to.rod

#' @rdname unitconversion.furlong.to.rod
unitconversion.furlong.to.pole <- unitconversion.furlong.to.rod

#' @rdname unitconversion.furlong.to.rod
unitconversion.furlong.to.perch <- unitconversion.furlong.to.rod



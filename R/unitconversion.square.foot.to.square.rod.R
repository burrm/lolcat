#' Unit Conversion - Area - Square Foot to Square Rod  
#' 
#' Performs a conversion of areas from square feet (U.S. Survey) to square rods. 
#'
#' @param x Vector - Values in units of square feet
#'
#' @return x, but converted to square yards 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.square.foot.to.square.rod <- function(
  x = 1
) {
  x / 272.25
}

#' @rdname unitconversion.square.foot.to.square.rod
unitconversion.sq.ft.to.sq.rd <- unitconversion.square.foot.to.square.rod

#' @rdname unitconversion.square.foot.to.square.rod
unitconversion.square.foot.to.square.pole <- unitconversion.square.foot.to.square.rod

#' @rdname unitconversion.square.foot.to.square.rod
unitconversion.square.foot.to.square.perch <- unitconversion.square.foot.to.square.rod
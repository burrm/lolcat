#' Unit Conversion - Length - Foot to Rod  
#' 
#' Performs a conversion of lengths from feet to rods (also called poles and perches). 
#'
#' @param x Vector - Values in units of feet (survey feet)
#'
#' @return x, but converted to rods 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.foot.to.rod <- function(
  x = 1
) {
  x / 16.5
}

#' @rdname unitconversion.foot.to.rod
unitconversion.ft.to.rd <- unitconversion.foot.to.rod

#' @rdname unitconversion.foot.to.rod
unitconversion.foot.to.pole <- unitconversion.foot.to.rod

#' @rdname unitconversion.foot.to.rod
unitconversion.foot.to.perch <- unitconversion.foot.to.rod
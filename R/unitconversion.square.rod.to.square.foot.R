#' Unit Conversion - Area - Square Rod to Square Foot  
#' 
#' Performs a conversion of areas from square rods to square feet (U.S. Survey). 
#'
#' @param x Vector - Values in units of square rods
#'
#' @return x, but converted to square feet 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.square.rod.to.square.foot <- function(
  x = 1
) {
  x * 272.25
}

#' @rdname unitconversion.square.rod.to.square.foot
unitconversion.sq.rd.to.sq.ft <- unitconversion.square.rod.to.square.foot

#' @rdname unitconversion.square.rod.to.square.foot
unitconversion.square.pole.to.square.foot <- unitconversion.square.rod.to.square.foot

#' @rdname unitconversion.square.rod.to.square.foot
unitconversion.square.perch.to.square.foot <- unitconversion.square.rod.to.square.foot
#' Unit Conversion - Length - Rod to Foot  
#' 
#' Performs a conversion of lengths from rods (also called poles and perches) to feet (survey feet). 
#'
#' @param x Vector - Values in units of rods
#'
#' @return x, but converted to feet 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.rod.to.foot <- function(
  x = 1
) {
  x * 16.5
}

#' @rdname unitconversion.rod.to.foot
unitconversion.rd.to.ft <- unitconversion.rod.to.foot

#' @rdname unitconversion.rod.to.foot
unitconversion.pole.to.foot <- unitconversion.rod.to.foot

#' @rdname unitconversion.rod.to.foot
unitconversion.perch.to.foot <- unitconversion.rod.to.foot

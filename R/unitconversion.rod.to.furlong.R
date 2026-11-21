#' Unit Conversion - Length - Rod to Furlong  
#' 
#' Performs a conversion of lengths from rods (also called poles and perches) to furlongs. 
#'
#' @param x Vector - Values in units of rods
#'
#' @return x, but converted to furlongs 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.rod.to.furlong <- function(
  x = 1
) {
  x / 40
}

#' @rdname unitconversion.rod.to.furlong
unitconversion.rd.to.fur <- unitconversion.rod.to.furlong

#' @rdname unitconversion.rod.to.furlong
unitconversion.pole.to.furlong <- unitconversion.rod.to.furlong

#' @rdname unitconversion.rod.to.furlong
unitconversion.perch.to.furlong <- unitconversion.rod.to.furlong
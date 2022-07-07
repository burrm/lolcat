#' Unit Conversion - Thickness - Mil to Millimeter  
#' 
#' Performs a conversion of lengths from mils to millimeters. 
#'
#' @param x Vector - Values in units of mils
#'
#' @return x, but converted to millimeters 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.mil.to.millimeter <- function(
  x = 1
) {
  x * .0254
}

#' @rdname unitconversion.mil.to.millimeter
unitconversion.mil.to.mm <- unitconversion.mil.to.millimeter

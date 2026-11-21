#' Unit Conversion - Thickness - Millimeter to Mil 
#' 
#' Performs a conversion of lengths from millimeters to mils. 
#'
#' @param x Vector - Values in units of millimeters
#'
#' @return x, but converted to mils 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.millimeter.to.mil <- function(
  x = 1
) {
  x / .0254
}

#' @rdname unitconversion.millimeter.to.mil
unitconversion.mm.to.mil <- unitconversion.millimeter.to.mil

#' Unit Conversion - Dry Volume - Dry Pint to Cubic Inch  
#' 
#' Performs a conversion of volumes from dry pints to cubic inches. 
#'
#' @param x Vector - Values in units of pints
#'
#' @return x, but converted to cubic inches 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.dry.pint.to.cubic.inch <- function(
  x = 1
) {
  x * 1.12
}

#' @rdname unitconversion.dry.pint.to.cubic.inch
unitconversion.dry.pt.to.cu.in <- unitconversion.dry.pint.to.cubic.inch
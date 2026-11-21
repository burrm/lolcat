#' Unit Conversion - Mass/Weight - Grain to Milligram  
#' 
#' Performs a conversion of weights from grains to milligrams. 
#'
#' @param x Vector - Values in units of grains
#'
#' @return x, but converted to milligrams 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.grain.to.milligram <- function(
  x = 1
) {
  x * 64.79891
}

#' @rdname unitconversion.grain.to.milligram
unitconversion.gr.to.mg <- unitconversion.grain.to.milligram
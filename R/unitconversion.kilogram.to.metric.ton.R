#' Unit Conversion - Mass/Weight - Kilogram to Metric Ton  
#' 
#' Performs a conversion of weights from kilograms to metric tons. 
#'
#' @param x Vector - Values in units of kilograms
#'
#' @return x, but converted to metric tons 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.kilogram.to.metric.ton <- function(
  x = 1
) {
  x / 1000
}
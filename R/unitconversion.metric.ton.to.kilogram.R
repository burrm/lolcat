#' Unit Conversion - Mass/Weight - Metric Ton to Kilogram  
#' 
#' Performs a conversion of weights from metric tons to kilograms. 
#'
#' @param x Vector - Values in units of metric tons
#'
#' @return x, but converted to kilograms 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.metric.ton.to.kilogram <- function(
  x = 1
) {
  x * 1000
}
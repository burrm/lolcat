#' Unit Conversion - Dry Volume - Bushel to Peck  
#' 
#' Performs a conversion of volumes from bushels to pecks. 
#'
#' @param x Vector - Values in units of bushels
#'
#' @return x, but converted to pecks
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
unitconversion.bushel.to.peck <- function(
  x = 1
) {
  x * 4
}

#' @rdname unitconversion.bushel.to.peck
unitconversion.bu.to.pk <- unitconversion.bushel.to.peck
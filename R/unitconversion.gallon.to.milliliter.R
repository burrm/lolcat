#' Unit Conversion - Liquid Volume -  Gallon to Milliliter
#' 
#' Performs a conversion of volumes from gallons to milliliters (mL). 
#'
#' @param x Vector - Values in units of gallons
#'
#' @return x, but converted to milliliters 
#'
#' @references
#' NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
#' https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf 
unitconversion.gallon.to.milliliter <- function(
  x = 1
) {
  x * 3785.411784
}

#' @rdname unitconversion.gallon.to.milliliter
unitconversion.gal.to.mL <- unitconversion.gallon.to.milliliter
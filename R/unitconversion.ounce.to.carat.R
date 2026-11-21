#' Unit Conversion - Mass/Weight - U.S. Ounce to Carat  
#' 
#' Performs a conversion of weights from ounces  (avoirdupois) to carats. 
#'
#' @param x Vector - Values in units of ounces
#'
#' @return x, but converted to carats 
unitconversion.ounce.to.carat <- function(
  x = 1
) {
  x / .00705
}

#' @rdname unitconversion.ounce.to.carat
unitconversion.oz.to.ct <- unitconversion.ounce.to.carat
#' Unit Conversion - Mass/Weight - U.S. Pound to Grain  
#' 
#' Performs a conversion of weights from pounds (avoirdupois) to grains. 
#'
#' @param x Vector - Values in units of pounds
#'
#' @return x, but converted to grains 
unitconversion.pound.to.grain <- function(
  x = 1
) {
  x * 7000
}

#' @rdname unitconversion.pound.to.grain
unitconversion.lb.to.gr <- unitconversion.pound.to.grain
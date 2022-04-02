#' Unit Conversion - Mass/Weight - U.S. Pound to Stone  
#' 
#' Performs a conversion of weights from pounds to stones. 
#'
#' @param x Vector - Values in units of pounds
#'
#' @return x, but converted to stone 
unitconversion.pound.to.stone <- function(
  x = 1
) {
  x * 14
}

#' @rdname unitconversion.pound.to.stone
unitconversion.lb.to.st <- unitconversion.pound.to.stone
#' Unit Conversion - Mass/Weight - Troy Pound to Grain  
#' 
#' Performs a conversion of weights from pounds (Troy) to grains. 
#'
#' @param x Vector - Values in units of pounds
#'
#' @return x, but converted to grains 
unitconversion.troy.pound.to.grain <- function(
  x = 1
) {
  x * 5760
}

#' @rdname unitconversion.troy.pound.to.grain
unitconversion.lbt.to.gr <- unitconversion.troy.pound.to.grain
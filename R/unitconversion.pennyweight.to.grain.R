#' Unit Conversion - Mass/Weight - Troy Pennyweight to Grain 
#' 
#' Performs a conversion of weights from troy pennyweights to grains. 
#'
#' @param x Vector - Values in units of pennyweight
#'
#' @return x, but converted to grain 
unitconversion.pennyweight.to.grain <- function(
  x = 1
) {
  x * 24
}

#' @rdname unitconversion.pennyweight.to.grain
unitconversion.dwt.to.gr <- unitconversion.pennyweight.to.grain
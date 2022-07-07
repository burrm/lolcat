#' Unit Conversion - Liquid Volume - Pint to Quart  
#' 
#' Performs a conversion of volumes from pints to quarts. 
#'
#' @param x Vector - Values in units of pints
#'
#' @return x, but converted to quarts 
unitconversion.pint.to.quart <- function(
  x = 1
) {
  x / 2
}

#' @rdname unitconversion.pint.to.quart
unitconversion.pt.to.qt <- unitconversion.pint.to.quart
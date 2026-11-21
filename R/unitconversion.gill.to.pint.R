#' Unit Conversion - Liquid Volume - Gill to Pint  
#' 
#' Performs a conversion of volumes from gills to pints. 
#'
#' @param x Vector - Values in units of gills
#'
#' @return x, but converted to pints 
unitconversion.gill.to.pint <- function(
  x = 1
) {
  x / 4
}

#' @rdname unitconversion.gill.to.pint
unitconversion.gi.to.pt <- unitconversion.gill.to.pint
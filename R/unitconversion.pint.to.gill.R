#' Unit Conversion - Liquid Volume - Pint to Gill  
#' 
#' Performs a conversion of volumes from pints to gills. 
#'
#' @param x Vector - Values in units of pints
#'
#' @return x, but converted to gills 
unitconversion.pint.to.gill <- function(
  x = 1
) {
  x * 4
}

#' @rdname unitconversion.pint.to.gill
unitconversion.pt.to.gi <- unitconversion.pint.to.gill
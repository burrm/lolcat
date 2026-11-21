#' Unit Conversion - Mass/Weight - Grain to Troy Pennyweight  
#' 
#' Performs a conversion of weights from grains to pennyweights. 
#'
#' @param x Vector - Values in units of grains
#'
#' @return x, but converted to pennyweight 
unitconversion.grain.to.pennyweight <- function(
  x = 1
) {
  x / 24
}

#' @rdname unitconversion.grain.to.pennyweight
unitconversion.gr.to.dwt <- unitconversion.grain.to.pennyweight
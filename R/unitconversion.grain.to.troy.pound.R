#' Unit Conversion - Mass/Weight - Grain to Troy Pound  
#' 
#' Performs a conversion of weights from grains to pounds (Troy). 
#'
#' @param x Vector - Values in units of grains
#'
#' @return x, but converted to Troy pounds 
unitconversion.grain.to.troy.pound <- function(
  x = 1
) {
  x / 5760
}

#' @rdname unitconversion.grain.to.troy.pound
unitconversion.gr.to.lbt <- unitconversion.grain.to.troy.pound
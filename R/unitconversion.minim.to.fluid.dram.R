#' Unit Conversion - Liquid Volume - Minim to Fluid Dram  
#' 
#' Performs a conversion of volumes from minims to fluid drams. 
#'
#' @param x Vector - Values in units of minims
#'
#' @return x, but converted to fluid drams 
unitconversion.minim.to.fluid.dram <- function(
  x = 1
) {
  x / 60
}
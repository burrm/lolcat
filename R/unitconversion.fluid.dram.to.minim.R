#' Unit Conversion - Liquid Volume - Fluid Dram to Minim  
#' 
#' Performs a conversion of volumes from fluid drams to minims. 
#'
#' @param x Vector - Values in units of fluid drams
#'
#' @return x, but converted to minims 
unitconversion.fluid.dram.to.minim <- function(
  x = 1
) {
  x * 60
}
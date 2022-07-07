#' Unit Conversion - Liquid Volume - Fluid Ounce to Fluid Dram 
#' 
#' Performs a conversion of volumes from fluid ounces to fluid drams. 
#'
#' @param x Vector - Values in units of fluid ounces
#'
#' @return x, but converted to fluid drams 
unitconversion.fluid.ounce.to.fluid.dram <- function(
  x = 1
) {
  x * 8
}

#' @rdname unitconversion.fluid.ounce.to.fluid.dram
unitconversion.fl.oz.to.fl.dr <- unitconversion.fluid.ounce.to.fluid.dram
#' Unit Conversion - Liquid Volume - Fluid Ounce to Cubic Inch 
#' 
#' Performs a conversion of volumes from fluid ounces to cubic inches. 
#'
#' @param x Vector - Values in units of fluid ounces
#'
#' @return x, but converted to cubic inches 
unitconversion.fluid.ounce.to.cubic.inch <- function(
  x = 1
) {
  x * 1.8046875
}

#' @rdname unitconversion.fluid.ounce.to.cubic.inch
unitconversion.fl.oz.to.cu.in <- unitconversion.fluid.ounce.to.cubic.inch
#' Absolute Deviation From Median  
#' 
#' Calculates absolute deviation from median (ADM).
#'
#' @param x Vector to compute ADM values for.
#'
#' @return Vector with results of ADM calculation.
dispersion.ADM <- function(x) {
  median.x<-median(x)
  abs(x-median.x)
}
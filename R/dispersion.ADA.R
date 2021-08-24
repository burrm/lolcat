#' Absolute Deviation From Average  
#' 
#' Calculates absolute deviation from average (ADA). This is also sometimes called mean absolute deviation (MAD)
#'
#' @param x Vector to compute ADA values for.
#'
#' @return Vector with results of ADA calculation.
dispersion.ADA <- function(x) {
  mean.x<-mean(x)
  abs(x-mean.x)
}
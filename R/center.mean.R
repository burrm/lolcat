#' Center Data Using Arithmetic Mean 
#' 
#' Subtract a measure of central tendency from data causing data's center to become zero.
#'
#' @param x A vector of values to be centered using the mean value
#'
#' @return A vector containing centered data (mean of zero). 
center.mean <- function(x) {
  center(x, center.function = mean)
}
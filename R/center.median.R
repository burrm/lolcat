#' Center Data Using Median
#' 
#' Subtract a measure of central tendency from data causing data's center to become zero.
#'
#' @param x A vector of values to be centered using the median value
#'
#' @return A vector containing centered data (median of zero). 

center.median <- function(x) {
  center(x, center.function = median)
}
#' Center Data  
#' 
#' Subtract a measure of central tendency from data causing data's center to become zero.
#'
#' @param x A vector of values to be centered
#' @param center.function A function to use as a measure of center
#' @param center.function.na.rm logial (T/F) identifying whether NA should be dropped from input
#'
#' @return A vector containing centered data (central tendency of zero). 

center <- function(x
                   ,center.function = mean
                   ,center.function.na.rm = T) {
  c1 <- center.function(
          ifelse(center.function.na.rm
                 ,na.omit(x) 
                 ,x) 
          )
  
  x - c1
  
}
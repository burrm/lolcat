#' Confusion Matrix Generation - Threshold Function  
#' 
#' Returns a function for use with confusion.matrix based on actual or predicted being larger than a particular value. 
#'
#' @param threshold Scalar - Value to compare actual and predicted with.
#'
#' @return A function for use with a call to confusion.matrix(). 
confusion.matrix.fn.threshold <- function(threshold) {
  f <- function(x) {
    ifelse(x > threshold,1,0)
  }
  f
}
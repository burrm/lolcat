#' Confusion Matrix Generation - Individual Value Function  
#' 
#' Returns a function for use with confusion.matrix based on actual or predicted being a particular value. 
#'
#' @param value Scalar - Value to compare actual and predicted with.
#'
#' @return A function for use with a call to confusion.matrix(). 
confusion.matrix.fn.value <- function(value) {
  function(x) {
    ifelse(x == value,1,0)
  }
}
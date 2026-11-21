#' Confusion Matrix Generation - Multiple Values Function  
#' 
#' Returns a function for use with confusion.matrix based on actual or predicted being in a set of values. 
#'
#' @param values Vector - Values to compare actual and predicted with.
#'
#' @return A function for use with a call to confusion.matrix(). 
confusion.matrix.fn.multiple.values <- function(values) {
  function(x) {
    ifelse(x %in% value,1,0)
  }
}
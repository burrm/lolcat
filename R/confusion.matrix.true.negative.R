#' Confusion Matrix Measures - True Negative  
#' 
#' Extract true negative for a given confusion matrix. 
#'
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @return A scalar with extracted value. 
confusion.matrix.true.negative <- function(confusion.matrix) {
  confusion.matrix[2,2]
}
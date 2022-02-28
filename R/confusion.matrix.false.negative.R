#' Confusion Matrix Measures - False Negative  
#' 
#' Extract false negative for a given confusion matrix. 
#'
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @return A scalar with extracted value. 
confusion.matrix.false.negative <- function(confusion.matrix) {
  confusion.matrix[2,1]
}
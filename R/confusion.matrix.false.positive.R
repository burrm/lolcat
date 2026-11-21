#' Confusion Matrix Measures - False Positive  
#' 
#' Extract false positive for a given confusion matrix. 
#'
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @return A scalar with extracted value. 
confusion.matrix.false.positive <- function(confusion.matrix) {
  confusion.matrix[1,2]
}
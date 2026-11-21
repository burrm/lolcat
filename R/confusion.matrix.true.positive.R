#' Confusion Matrix Measures - True Positive  
#' 
#' Extract true positive for a given confusion matrix. 
#'
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @return A scalar with extracted value.
confusion.matrix.true.positive <- function(confusion.matrix) {
  confusion.matrix[1,1]
}
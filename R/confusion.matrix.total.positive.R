#' Confusion Matrix Measures - Total Positive Cases  
#' 
#' Calculate Total Positive Cases, defined as FP+TN, for a given confusion matrix. 
#'
#' @param confusion.matrix Matrix - Confusion matrix
#'
#' @return A scalar with computed value. 
confusion.matrix.total.positive <- function(
  confusion.matrix
) {
  confusion.matrix.total.positive.simple(
    false.positive = confusion.matrix.false.positive(confusion.matrix)
    ,true.negative = confusion.matrix.true.negative(confusion.matrix)
  ) 
}
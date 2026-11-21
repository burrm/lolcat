#' Confusion Matrix Measures - Total Negative Cases 
#' 
#' Calculate Total Negative cases, defined as TN+FP, for a given confusion matrix. 
#'
#' @param confusion.matrix Matrix - Confusion matrix
#'
#' @return A scalar with computed value. 
confusion.matrix.total.negative <- function(
  confusion.matrix
) {
  confusion.matrix.total.negative.simple(
    true.negative = confusion.matrix.true.negative(confusion.matrix)
    ,false.positive = confusion.matrix.false.positive(confusion.matrix)
  ) 
}
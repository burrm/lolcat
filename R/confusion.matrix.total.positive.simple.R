#' Confusion Matrix Measures - Total Positive Cases  
#' 
#' Calculate Total Positive Cases, defined as FP+TN, for a given confusion matrix. 
#'
#' @param false.positive Scalar - Cases identified as false positive
#' @param true.negative Scalar - Cases identified as true negative
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @return A scalar with computed value. 
confusion.matrix.total.positive.simple <- function(
  false.positive
  ,true.negative
) {
  false.positive + true.negative
}
#' Confusion Matrix Measures - False Omission Rate  
#' 
#' Calculate False Omission Rate, defined as FN/(FN+TN), for a given confusion matrix. 
#'
#' @param true.negative Scalar - Cases identified as true negative
#' @param false.negative Scalar - Cases identified as false negative
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @aliases confusion.matrix.false.omission.rate
#'
#' @return A scalar with computed value. 
confusion.matrix.false.omission.rate.simple <- function(
  false.negative = 0
  ,true.negative = 1 
) {
  false.negative / (false.negative + true.negative)
}

#' Confusion Matrix Measures - F1 Score  
#' 
#' Calculate F1 Score, defined as 2*TP / (2*TP+FP+FN), for a given confusion matrix. 
#'
#' @param true.positive Scalar - Cases identified as true positive
#' @param false.positive Scalar - Cases identified as false positive 
#' @param false.negative Scalar - Cases identified as false negative 
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @return A scalar with computed value. 
confusion.matrix.f1.score.simple <- function(
  true.positive = 1
  ,false.positive = 0
  ,false.negative = 0
) {
  (2*true.positive)/ (2*true.positive + false.positive + false.negative)
}
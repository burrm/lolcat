#' Confusion Matrix Measures - Negative Predictive Value 
#' 
#' Calculate Negative Predictive Value, defined as TN/(TN+FN), for a given confusion matrix. 
#'
#' @param true.negative Scalar - Cases identified as true negative 
#' @param false.negative Scalar - Cases identified as false negative 
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @aliases confusion.matrix.negative.predictive.value
#'
#' @return A scalar with computed value. 
confusion.matrix.negative.predictive.value.simple <- function(
  true.negative = 0
  ,false.negative = 1 
) {
  true.negative / (true.negative + false.negative)
}


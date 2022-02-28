#' Confusion Matrix Measures - Positive Predictive Value/Precision  
#' 
#' Calculate Positive Predictive Value (also called Precision), defined as TP/(TP+FP), for a given confusion matrix. 
#'
#' @param true.positive Scalar - Cases identified as true positive
#' @param false.positive Scalar - Cases identified as false positive
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @aliases confusion.matrix.positive.predictive.value confusion.matrix.precision.simple confusion.matrix.precision 
#'
#' @return A scalar with computed value. 
confusion.matrix.positive.predictive.value.simple <- function(
  true.positive = 0
  ,false.positive = 1 
) {
  true.positive / (true.positive + false.positive)
}

#' @rdname confusion.matrix.positive.predictive.value.simple
confusion.matrix.precision.simple <- confusion.matrix.positive.predictive.value.simple

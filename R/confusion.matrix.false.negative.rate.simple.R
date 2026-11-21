#' Confusion Matrix Measures - False Negative/Miss Rate  
#' 
#' Calculate False Negative Rate (also called Miss Rate), defined as FN/P, for a given confusion matrix. 
#'
#' @param true.positive Scalar - Cases identified as true positive
#' @param false.negative Scalar - Cases identified as false negative 
#' @param count.positive Scalar - Total cases identified as positive - optional if first two parameters are used. 
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @aliases confusion.matrix.false.negative.rate confusion.matrix.miss.rate.simple confusion.matrix.miss.rate 
#'
#' @return A scalar with computed value. 

#AKA miss rate, false negative rate
confusion.matrix.false.negative.rate.simple <- function(
  false.negative = 0
  ,true.positive = 1 #optional
  ,count.positive = false.negative + true.positive
) {
  false.negative / count.positive
}

#' @rdname confusion.matrix.false.negative.rate.simple
confusion.matrix.miss.rate.simple <- confusion.matrix.false.negative.rate.simple
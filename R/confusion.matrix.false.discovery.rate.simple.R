#' Confusion Matrix Measures - False Discovery Rate  
#' 
#' Calculate False Discovery Rate, defined as FP / (FP + TP), for a given confusion matrix. 
#'
#' @param true.positive Scalar - Cases identified as true positive
#' @param false.positive Scalar - Cases identified as false positive 
#'
#' @return A scalar with computed value. 
confusion.matrix.false.discovery.rate.simple <- function(
  false.positive = 0
  ,true.positive = 1 
) {
  false.positive / (false.positive + true.positive)
}
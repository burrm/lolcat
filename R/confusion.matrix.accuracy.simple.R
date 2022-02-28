#' Confusion Matrix Measures - Accuracy  
#' 
#' Calculate Accuracy, defined as (TP+TN)/(P+N), for a given confusion matrix. 
#'
#' @param true.positive Scalar - Cases identified as true positive
#' @param false.positive Scalar - Cases identified as false positive - optional if count.positive is used
#' @param true.negative Scalar - Cases identified as true negative
#' @param false.negative Scalar - Cases identified as false negative - optional if count.negative is used
#' @param count.positive Scalar - Total cases identified as positive - optional if first four parameters are used. 
#' @param count.negative Scalar - Total cases identified as negative - optional if first four parameters are used.
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @aliases confusion.matrix.accuracy
#'
#' @return A scalar with computed accuracy value. 

confusion.matrix.accuracy.simple <- function(
  true.positive = 0
  ,false.positive = 0 #optional
  ,true.negative = 1
  ,false.negative = 0 #optional
  ,count.positive = true.positive + false.negative
  ,count.negative = true.negative + false.positive
) {
  (true.positive + true.negative) / (count.positive + count.negative)
}

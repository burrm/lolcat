#' Confusion Matrix Measures - True Negative Rate/Specificity  
#' 
#' Calculate True Negative Rate (also called Specificity), defined as TN/N, for a given confusion matrix. 
#'
#' @param true.positive Scalar - Cases identified as true positive 
#' @param false.positive Scalar - Cases identified as false positive - optional if count.negative specified
#' @param count.negative Scalar - Total cases identified as negative - optional if FP specified.
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @aliases confusion.matrix.specificity.simple confusion.matrix.true.negative.rate confusion.matrix.specificity
#'
#' @return A scalar with computed value. 
confusion.matrix.true.negative.rate.simple <- function(
  true.negative = 0
  ,false.positive = 1 #optional
  ,count.negative = true.negative + false.positive
) {
  true.negative / count.negative
}

#' @rdname confusion.matrix.true.negative.rate.simple 
confusion.matrix.specificity.simple <- confusion.matrix.true.negative.rate.simple

#' Confusion Matrix Measures - False Positive/Fallout Rate  
#' 
#' Calculate False Positive Rate (sometimes called Fallout rate), defined as FP/N, for a given confusion matrix. 
#'
#' @param false.positive Scalar - Cases identified as false positive 
#' @param true.negative Scalar - Cases identified as true negative
#' @param count.negative Scalar - Total cases identified as negative - optional if first two parameters are used.
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @aliases confusion.matrix.fall.out.simple confusion.matrix.fall.out confusion.matrix.false.positive.rate
#'
#' @return A scalar with computed value. 
confusion.matrix.false.positive.rate.simple <- function(
  false.positive = 0
  ,true.negative = 1 
  ,count.negative = false.positive + true.negative
) {
  false.positive / count.negative
}

#' @rdname confusion.matrix.false.positive.rate.simple
confusion.matrix.fall.out.simple <- confusion.matrix.false.positive.rate.simple
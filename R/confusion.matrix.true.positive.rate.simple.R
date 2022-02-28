#' Confusion Matrix Measures - True Positive Rate  
#' 
#' Calculate True Positive Rate, defined as TP/P, for a given confusion matrix. True positive rate is also sometimes called sensitivity, recall, and hit rate. 
#'
#' @param true.positive Scalar - Cases identified as true positive 
#' @param false.negative Scalar - Cases identified as false negative - optional if count.positive specified
#' @param count.positive Scalar - Total cases identified as positive - optional if false.negative specified. 
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @aliases confusion.matrix.sensitivity.simple confusion.matrix.sensitivity confusion.matrix.recall.simple confusion.matrix.recall confusion.matrix.hit.rate.simple confusion.matrix.hit.rate confusion.matrix.true.positive.rate
#'
#' @return A scalar with computed value. 
confusion.matrix.true.positive.rate.simple <- function(
  true.positive = 0
  ,false.negative = 1 #optional
  ,count.positive = true.positive + false.negative
) {
  true.positive / count.positive
}

#' @rdname confusion.matrix.true.positive.rate.simple
confusion.matrix.sensitivity.simple <- confusion.matrix.true.positive.rate.simple

#' @rdname confusion.matrix.true.positive.rate.simple
confusion.matrix.recall.simple      <- confusion.matrix.true.positive.rate.simple

#' @rdname confusion.matrix.true.positive.rate.simple
confusion.matrix.hit.rate.simple    <- confusion.matrix.true.positive.rate.simple

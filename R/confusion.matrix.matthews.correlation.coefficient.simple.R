#' Confusion Matrix Measures - Matthews' Correlation Coefficient  
#' 
#' Calculate Matthews' Correlation Coefficient, defined as (TP*TN - FP*FN)/sqrt((TP+FP)(TP+FN)(TN+FP)(TN+FN)), for a given confusion matrix. 
#'
#' @param true.positive Scalar - Cases identified as true positive
#' @param false.positive Scalar - Cases identified as false positive
#' @param true.negative Scalar - Cases identified as true negative
#' @param false.negative Scalar - Cases identified as false negative 
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @aliases confusion.matrix.matthews.correlation.coefficient cor.matthews.simple cor.matthews
#'
#' @return A scalar with computed value. 
confusion.matrix.matthews.correlation.coefficient.simple <- function(
  true.positive = 1
  ,false.positive = 1
  ,true.negative =1
  ,false.negative = 1
) {
  (true.positive * true.negative - false.positive * false.negative) / sqrt((true.positive +false.positive) * (true.positive + false.negative)*(true.negative + false.positive) * (true.negative + false.negative))
}

#' @rdname confusion.matrix.matthews.correlation.coefficient.simple
cor.matthews.simple <- confusion.matrix.matthews.correlation.coefficient.simple
#' Confusion Matrix Measures - Bookmaker Informedness  
#' 
#' Calculate Bookmaker Informedness, defined as TPR+TNR-1, for a given confusion matrix. 
#'
#' @param true.positive Scalar - Cases identified as true positive
#' @param false.positive Scalar - Cases identified as false positive 
#' @param true.negative Scalar - Cases identified as true negative
#' @param false.negative Scalar - Cases identified as false negative 
#' @param count.positive Scalar - Total cases identified as positive - optional if first four parameters are used. 
#' @param count.negative Scalar - Total cases identified as negative - optional if first four parameters are used.
#' @param true.positive.rate Scalar - True positive rate, overrides other parameters used to calculate TPR.
#' @param true.negative.rate Scalar - True negative rate, overrides other parameters used to calculate TNR.
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @return A scalar with computed value. 

confusion.matrix.bookmaker.informedness.simple <- function(
  #TPR inputs
  true.positive = 0
  ,false.negative = 1 
  ,count.positive = true.positive + false.negative
  
  #TNR inputs
  ,false.positive = 0
  ,true.negative = 1 
  ,count.negative = false.positive + true.negative  
  
  #Only true required parameters:
  ,true.positive.rate = confusion.matrix.true.positive.rate.simple(
    true.positive = true.positive
    ,count.positive = count.positive
  )
  
  ,true.negative.rate =confusion.matrix.true.negative.rate.simple(
    true.negative = true.negative
    ,count.negative = count.negative
  )
  
) {
  true.positive.rate + true.negative.rate - 1
}
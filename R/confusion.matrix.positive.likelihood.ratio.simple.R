#' Confusion Matrix Measures - Positive Likelihood Ratio  
#' 
#' Calculate Positive Likelihood Ratio, defined as TPR/FPR, for a given confusion matrix. 
#'
#' @param true.positive Scalar - Cases identified as true positive - optional if TPR specified
#' @param false.positive Scalar - Cases identified as false positive - optional if FPR specified
#' @param true.negative Scalar - Cases identified as true negative - optional if FPR specified
#' @param false.negative Scalar - Cases identified as false negative - optional if TPR specified
#' @param count.positive Scalar - Total cases identified as positive - optional if TPR specified. 
#' @param count.negative Scalar - Total cases identified as negative - optional if FPR specified.
#' @param true.positive.rate Scalar - True Positive Rate (TPR) - optional if TPR parameters specified.
#' @param false.positive.rate Scalar - False Positive Rate (FPR) - optional if FPR parameters specified.
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @aliases confusion.matrix.positive.likelihood.ratio
#'
#' @return A scalar with computed value.  
confusion.matrix.positive.likelihood.ratio.simple <- function(
  #TPR inputs
  true.positive = 0
  ,false.negative = 1 
  ,count.positive = true.positive + false.negative
  
  #FPR inputs
  ,false.positive = 0
  ,true.negative = 1
  ,count.negative = false.positive + true.negative  

  #Only true required parameters:
  ,true.positive.rate = confusion.matrix.true.positive.rate.simple(
    true.positive = true.positive
    ,count.positive = count.positive
  )
  ,false.positive.rate = confusion.matrix.false.positive.rate.simple(
    false.positive = false.positive
    ,count.negative = count.negative
  )  
) {
  true.positive.rate / false.positive.rate
}
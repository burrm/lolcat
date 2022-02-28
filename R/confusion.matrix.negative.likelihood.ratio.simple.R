#' Confusion Matrix Measures - Negative Likelihood Ratio  
#' 
#' Calculate Negative Likelihood Ratio, defined as FNR/TNR, for a given confusion matrix. 
#'
#' @param true.positive Scalar - Cases identified as true positive - optional if  FNR specified
#' @param false.positive Scalar - Cases identified as false positive - optional if TNR specified
#' @param true.negative Scalar - Cases identified as true negative - optional if TNR specified
#' @param false.negative Scalar - Cases identified as false negative - optional if FNR specified
#' @param count.positive Scalar - Total cases identified as positive - optional if TNR specified. 
#' @param count.negative Scalar - Total cases identified as negative - optional if FNR specified.
#' @param true.negative.rate Scalar - True Negative Rate (TNR) - optional if TNR parameters specified.
#' @param false.negative.rate Scalar - False Negative Rate (FNR) - optional if FNR parameters specified.
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @aliases confusion.matrix.negative.likelihood.ratio
#'
#' @return A scalar with computed value. 
confusion.matrix.negative.likelihood.ratio.simple <- function(
  #FNR inputs
  false.negative = 0
  ,true.positive = 1 #optional
  ,count.positive = false.negative + true.positive
  
  #TNR inputs
  ,true.negative = 0
  ,false.positive = 1 #optional
  ,count.negative = true.negative + false.positive
  
  #Only true required parameters:
  ,true.negative.rate = confusion.matrix.true.negative.rate.simple(
    true.negative = true.negative
    ,count.negative = count.negative
  )
  ,false.negative.rate = confusion.matrix.false.negative.rate.simple(
    false.negative = false.negative
    ,count.positive = count.positive
  )
) {
  false.negative.rate / true.negative.rate
}
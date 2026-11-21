#' Confusion Matrix Measures - Diagnostic Odds Ratio  
#' 
#' Calculate Diagnostic Odds Ratio, defined as Positive Likelihood Ratio / Negative Likelihood Ratio, for a given confusion matrix. 
#'
#' @param true.positive Scalar - Cases identified as true positive
#' @param false.positive Scalar - Cases identified as false positive 
#' @param true.negative Scalar - Cases identified as true negative
#' @param false.negative Scalar - Cases identified as false negative 
#' @param count.positive Scalar - Total cases identified as positive - optional if first four parameters are used. 
#' @param count.negative Scalar - Total cases identified as negative - optional if first four parameters are used.
#' @param true.positive.rate Scalar - True positive rate, overrides other parameters used to calculate TPR.
#' @param false.positive.rate Scalar - False positive rate, overrides other parameters used to calculate FPR.
#' @param true.negative.rate Scalar - True negative rate, overrides other parameters used to calculate TNR.
#' @param false.negative.rate Scalar - False negative rate, overrides other parameters used to calculate FNR.
#' @param positive.likelihood.ratio Scalar - Positive Likelihood Ratio, overrides other parameters used to calculate positive likelihood ratio.
#' @param negative.likelihood.ratio Scalar - Negative Likelihood Ratio, overrides other parameters used to calculate negative likelihood ratio.
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @return A scalar with computed value. 


confusion.matrix.diagnostic.odds.ratio.simple <- function(
  #TPR inputs
  true.positive = 0
  ,false.negative = 1 
  ,count.positive = true.positive + false.negative
  
  #FPR inputs
  ,false.positive = 0
  ,true.negative = 1 
  ,count.negative = false.positive + true.negative  
  
  #FNR inputs - already via TPR above
  
  #TNR inputs already via FPR above

  #Only true required parameters:
  ,true.positive.rate = confusion.matrix.true.positive.rate.simple(
    true.positive = true.positive
    ,count.positive = count.positive
  )
  ,false.positive.rate = confusion.matrix.false.positive.rate.simple(
    false.positive = false.positive
    ,count.negative = count.negative
  )  
  
  ,true.negative.rate =confusion.matrix.true.negative.rate.simple(
    true.negative = true.negative
    ,count.negative = count.negative
  )
  ,false.negative.rate = confusion.matrix.false.negative.rate.simple(
    false.negative = false.negative
    ,count.positive = count.positive
  )
  
  ,positive.likelihood.ratio = confusion.matrix.positive.likelihood.ratio.simple(
    true.positive.rate = true.positive.rate
    ,false.positive.rate = false.positive.rate
  )  
  
  ,negative.likelihood.ratio = confusion.matrix.negative.likelihood.ratio.simple(
    true.negative.rate = true.negative.rate
    ,false.negative.rate = false.negative.rate
  )
    
) {
  positive.likelihood.ratio / negative.likelihood.ratio
}
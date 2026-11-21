#' Confusion Matrix Measures - Markedness  
#' 
#' Calculate Markedness, defined as PPV+NPV-1, for a given confusion matrix. 
#'
#' @param true.positive Scalar - Cases identified as true positive - optional if PPV specified
#' @param false.positive Scalar - Cases identified as false positive - optional if PPV specified
#' @param true.negative Scalar - Cases identified as true negative - optional if NPV specified
#' @param false.negative Scalar - Cases identified as false negative - optional if NPV specified
#' @param positive.predictive.value Scalar - Positive Predictive Value (PPV) - optional if true.positive and false.positive parameters are used. 
#' @param negative.predictive.value Scalar - Negative Predictive Value (NPV) - optional if true.negative and false.negative parameters are used.
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @return A scalar with computed value. 
confusion.matrix.markedness.simple <- function(
  #PPV inputs
  true.positive = 0
  ,false.positive = 1
  
  #NPV inputs
  ,true.negative = 0
  ,false.negative = 1
  
  ,positive.predictive.value = confusion.matrix.positive.predictive.value.simple(
    true.positive =  true.positive
    ,false.positive = false.positive
    )
  ,negative.predictive.value = confusion.matrix.negative.predictive.value.simple(
    true.negative = true.negative
    ,false.negative = false.negative
  )
) {
  positive.predictive.value + negative.predictive.value - 1
}
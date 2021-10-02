#' Confusion Matrix Measures - Accuracy  
#' 
#' Calculate Accuracy, defined as (TP+TN)/(P+N), for a given confusion matrix. 
#'
#' @param confusion.matrix Matrix - confusion matrix.
#'
#' @return A scalar with computed accuracy value. 

confusion.matrix.accuracy <- function(
  confusion.matrix
) {
   confusion.matrix.accuracy.simple(
     true.positive = confusion.matrix.true.positive(confusion.matrix)
     ,false.positive = confusion.matrix.false.positive(confusion.matrix)
     ,true.negative = confusion.matrix.true.negative(confusion.matrix)
     ,false.negative = confusion.matrix.false.negative(confusion.matrix)
   ) 
}

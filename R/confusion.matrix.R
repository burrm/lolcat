#' Create a confusion matrix 
#' 
#' Create a confusion matrix from actual and predicted values. Note that this function follows the convention 
#' of placing "Predicted" as the row variable and "Actual/Correct" as the column variable for the returned contingency 
#' table. This is unlikely to be a compatible format with other packages that use the reverse convention for predicted/actual.
#'
#' @param actual A vector of values identifying the actual value (default 0,1 or use actual.positive.fn to convert)
#' @param predicted A vector of values identifying the predicted value (default 0,1 or use predicted.positive.fn to convert)
#' @param actual.positive.fn A function that takes values present in actual and converts them to 0,1
#' @param predicted.positive.fn A function that takes values in predicted and converts them to 0,1
#'
#' @return A 2x2 matrix showing agreement between actual and predicted. 

confusion.matrix <- function(
  actual
  ,predicted
  #Function returns 0 or 1
  ,actual.positive.fn = function(actual) {
    ifelse(actual == 1, 1, 0)
  }
  #Function returns 0 or 1
  ,predicted.positive.fn = function(predicted) {
    ifelse(predicted == 1, 1, 0)
  }) {
  actual <- actual.positive.fn(actual)
  predicted <- predicted.positive.fn(predicted)
  
  m <- matrix(
    c(
      length(which(actual == 1 & predicted == 1))
      ,length(which(actual == 0 & predicted == 1))
      ,length(which(actual == 1 & predicted == 0))
      ,length(which(actual == 0 & predicted == 0))
      )
    ,nrow = 2
    ,byrow = T
  )
  
  rownames(m) <- c("Predicted Positive", "Predicted Negative")
  colnames(m) <- c("Actual Positive", "Actual Negative")
  m
}

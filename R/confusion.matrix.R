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

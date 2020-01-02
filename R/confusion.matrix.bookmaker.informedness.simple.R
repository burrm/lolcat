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
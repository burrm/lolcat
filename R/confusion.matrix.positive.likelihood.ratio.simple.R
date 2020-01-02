confusion.matrix.positive.likelihood.ratio.simple <- function(
  #TPR inputs
  true.positive = 0
  ,false.negative = 1 
  ,count.positive = true.positive + false.negative
  
  #FPR inputs
  ,false.positive = 0
  ,true.negative = 1 #optional
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
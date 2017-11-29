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
  ,true.negative.rate =confusion.matrix.true.negative.rate.simple(
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